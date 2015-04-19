/*
 *  QCC - Tiny C Compiler V2
 * 
 *  Copyright (c) 2001-2004 Fabrice Bellard
 *  Copyright (c) 2006-2007 Rob Landley
 *  Copyright (c) 2015 Bruce Ewing
 *
 *  Licensed under 2-clause BSD, see file LICENSE in this tarball
 */

// Design considerations:
// This program is designed to go fast, even when compiling something large.

// In general, there are several things that slow a program down:
// 1) using more memory than the OS can make available,
// 2) not using enough memory,
//     so lots of stuff has to be written to disk and then read back in,
// 3) system calls,
// 4) inefficient memory handling functions.

// In the end, all this comes down to allocating a nice big hunk of memory
// (but not TOO big), and then packing all the important data in really tightly.
// Most OSes leave a program to guess if a particular sized memory allocation
// will send the OS to pageswapping hell, and that is unfortunate.

// A compiler generates a lot of data, so the memory issue is fairly critical.
// So this code plays some games with creating needed data, then cramming
// it into the smallest possible space to make as much room available as
// possible for the next step. It also minimizes using malloc, since most
// malloc implementations are rather large and slow -- and they can't avoid
// having a little memory overhead that is better kept in the program.

// The extra complexity of playing these memory games will slow down the
// compilation of tiny programs a little bit, but will make large improvements
// in performance when compiling big stuff. It also adds some code complexity,
// and a few extra passes over the input, which is also unfortunate.


// TODOS:
// make the ternary test in eval_const_expr active

// later:
// sizeof() needs to be able to recognize a function pointer
// function pointer prototype info from typedefs

#include "qcc.h"

// include ALL the supported output formats, but only ONE is selected with an internal #ifdef
#include "formats/elf.c"
//#include "formats/win_pe.c"
//#include "formats/coff.c"
//#include "formats/binflt.c"

// include ALL the supported target CPU code-generation backends, but only ONE is selected with an internal #ifdef
#include "targets/tcg_int.c"				// the TCG target is special in that it has multiple sub-targets, itself

// the following targets are only created to be specially optimized -- better than TCG can do it
//#include "targets/i_386.c"
#include "targets/i_586.c"
//#include "targets/i_x86-64.c"
//#include "targets/arm.c"
//#include "targets/c67.c"
//#include "targets/cil.c"


// QCC can theoretically be built with any host compiler, with any host C library
// -- however, the host compilers and C libraries are not completely interchangeable
// -- so all the fiddly code to make the build work under all possible scenarios is contained in libc_compat.c.
#include "tok.h"
#include "libc_compat.c"

// in the same spirit as having multiple targets and formats,
// multiple hash algorithms are supported (uncomment the one you want)
#include "crc32.c"


#include "cpp.c"
#include "token.c"
#include "proto.c"


void onetime_init()
{
	int i;
	char *p;
	uint32_t j;

	// if QCC is running on a bigendian host, it needs to know that -- just do a quick test
	host_bigendian = 0;
	j = 0x42;
	p = (char *) &j;
	if (*p != 0x42)
		host_bigendian = 1;
	
	total_bytes = 0;			// init compilation statistics
	total_lines = 0;
	total_errs = 0;
	total_warns = 0;

	// build a lookup table for which chars are acceptable for function and variable names
	// (alpha-numeric + '_')
// prebuild these arrays as static consts?
	memset (alnum_, 0, 256);
	i = 'A';
	while (i <= 'Z') alnum_[i++] = 1;
	i = 'a';
	while (i <= 'z') alnum_[i++] = 1;
	i = '0';
	while (i <= '9') alnum_[i++] = 1;
	alnum_['_'] = 1;

	// build a lookup table to convert from ascii to hex
	memset (hex_lkup, 0xff, 256);
	i = 'A';
	while (i <= 'F') hex_lkup[i++] = i - 'A' + 10;
	i = 'a';
	while (i <= 'f') hex_lkup[i++] = i - 'a' + 10;
	i = '0';
	while (i <= '9') hex_lkup[i++] = i - '0';

	// and a lookup table in the other direction
	i = 16;
	while (--i >= 10) hexout[i] = 'a' + i - 10;
	while (i >= 0) hexout[i--] = '0' + i;

	// build a lookup table to recognize C operators
	memset (c_ops, 0, 256);
	c_ops['('] = TOK_O_PAREN;
	c_ops[')'] = TOK_C_PAREN;
	c_ops['='] = TOK_ASSIGN;
	c_ops['<'] = TOK_B_LT;
	c_ops['>'] = TOK_B_LT;
	c_ops['&'] = TOK_AND;
	c_ops['|'] = TOK_OR;
	c_ops['!'] = TOK_B_NOT;
	c_ops['+'] = TOK_ADD;
	c_ops['-'] = TOK_SUB;
	c_ops['*'] = TOK_MULT;
	c_ops['/'] = TOK_DIV;
	c_ops['%'] = TOK_MOD;
	c_ops['^'] = TOK_XOR;
	c_ops['~'] = TOK_NOT;
	c_ops['?'] = TOK_QMARK;
	c_ops['\''] = TOK_SQUOTE;
	c_ops['"'] = TOK_DQUOTE;
	c_ops['['] = TOK_OSQUARE;
	c_ops[']'] = TOK_CSQUARE;
	c_ops['{'] = TOK_OCURLY;
	c_ops['}'] = TOK_CCURLY;
	c_ops[':'] = TOK_COLON;
	c_ops[';'] = TOK_SEMIC;
	c_ops[','] = TOK_COMMA;
	c_ops['.'] = TOK_DOT;
	c_ops[' '] = 1;				// space chars are no-ops, but they act as preprocessor delimiters

	// non-newline whitespace lookup table -- for use in the preprocessor only
	memset (whtsp_lkup, 0, 128);
	whtsp_lkup['\t'] = 1;
	whtsp_lkup['\r'] = 1;
	whtsp_lkup['\v'] = 1;
	whtsp_lkup['\f'] = 1;
	whtsp_lkup[' '] = 1;

	memset (prep_ops, 0, 256);
	prep_ops['~'] = 1;
	prep_ops['!'] = 1;
	prep_ops['%'] = 1;
	prep_ops['^'] = 1;
	prep_ops['&'] = 1;
	prep_ops['*'] = 1;
	prep_ops['('] = 1;
	prep_ops[')'] = 1;
	prep_ops['-'] = 1;
	prep_ops['+'] = 1;
	prep_ops['='] = 1;
//	prep_ops['"'] = 1;			// HIHI!!! should this not be a valid preprocessor operator either?
	prep_ops['|'] = 1;
	prep_ops[','] = 1;
	prep_ops['/'] = 1;
	prep_ops['<'] = 1;
	prep_ops['>'] = 1;
	prep_ops['?'] = 1;
	prep_ops[' '] = 1;

	// there is also a special char needed when tokenizing #if statements
	prep_ops['$'] = 1;			// precedes tokenized preprocessor intrinsic names
	prep_ops[0x1b] = 1;			// and two more escape chars for processing char literals
	prep_ops[0x1c] = 1;

	memset (prep_src, 0, 0x21);
	memset (prep_src + 0x21, 1, 128 - 0x21);
	prep_src['"'] = 0;			// special case chars that make read_compressed change state
	prep_src['\\'] = 0;
	prep_src['\''] = 0;
	prep_src['/'] = 0;

	memset (stoppers, 0, 128);
	stoppers[0] = 1;
	stoppers['\n'] = 1;

	// allocate a big workspace (unless the OS insists on less) -- but not TOO big!
	// XXX: there are several ways to make this smarter,
	// but this code should work very well on almost all machines.

	// start at 16M, and work down to 1M if malloc fails -- and live within it
	wrk_size = 32 * 1024 * 1024;			// twice the initial allocation request
	wrksp = NULL;
	while (wrksp == NULL && wrk_size > 1024 * 1024)
	{
		wrk_size = wrk_size / 2;
		wrksp = (uint8_t *) malloc(wrk_size);
	}
	if (wrksp == NULL)
	{
		write (2, "fatal error: unable to allocate a minimum 1M of working memory\n", 63);
		exit (1);
	}

	// init wrksp_top and wrk_rem
	wrk_rem = wrk_size;
	wrksp_top = wrksp + wrk_size;
	line_nums = NULL;

	// init all the arrays associated with da_buffers -- space the pointers about equally through the workspace
	// -- they are just about to get filled with info in parse_args()
	i = 7;
	while (--i >= 0)
	{
		da_tot_entrylen[i] = 0;
		da_entry_count[i] = 0;
		da_buffers[i] = wrksp + (wrk_size / 8) * i;
	}
	// the first entry in the "includes" da should be 0 length
	da_tot_entrylen[INCLUDE_PATHS] = 1;
	*da_buffers[INCLUDE_PATHS] = 0;
}


// detect all intrinsic keywords
uint8_t detect_c_keyword(uint8_t *s, uint32_t j)
{
	switch (*s)
	{
	case '_':
		if (j == 5)
		{
			if (strncmp((const char *) s+1, keywords[TOK_BOOL_T- KEYWORDS_OFF] + 1, 4) == 0) return TOK_BOOL_T;
		}
		else if (j == 7)
		{
			if (strncmp((const char *) s+1, keywords[TOK_QWORD_T- KEYWORDS_OFF] + 1, 6) == 0) return TOK_QWORD_T;
		}
		return 0;
	case 'a':
		if (j == 3)
		{
			if (strncmp((const char *) s+1, keywords[TOK_ASM- KEYWORDS_OFF] + 1, 2) == 0) return TOK_ASM;
		}
		else if (j == 6)
		{
			if (strncmp((const char *) s+1, keywords[TOK_ALLOCA- KEYWORDS_OFF] + 1, 5) == 0) return TOK_ALLOCA;
		}
		return 0;
	case 'b':
		if (j == 5)
		{
			if (strncmp((const char *) s+1, keywords[TOK_BREAK- KEYWORDS_OFF] + 1, 4) == 0) return TOK_BREAK;
		}
		return 0;
	case 'c':
		if (j == 4)
		{
			if (strncmp((const char *) s+1, keywords[TOK_CHAR_T- KEYWORDS_OFF] + 1, 3) == 0) return TOK_CHAR_T;
			if (strncmp((const char *) s+1, keywords[TOK_CASE- KEYWORDS_OFF] + 1, 3) == 0) return TOK_CASE;
		}
		else if (j == 5)
		{
			if (strncmp((const char *) s+1, keywords[TOK_CONST- KEYWORDS_OFF] + 1, 4) == 0) return TOK_CONST;
		}
		else if (j == 7)
		{
			if (strncmp((const char *) s+1, keywords[TOK_COUNT- KEYWORDS_OFF] + 1, 6) == 0) return TOK_COUNT;
		}
		else if (j == 8)
		{
			if (strncmp((const char *) s+1, keywords[TOK_CONTINUE- KEYWORDS_OFF] + 1, 7) == 0) return TOK_CONTINUE;
		}
		return 0;
	case 'd':
		if (j == 2)
		{
			if (s[1] == 'o') return TOK_DO;
		}
		else if (j == 6)
		{
			if (strncmp((const char *) s+1, keywords[TOK_DBL_T- KEYWORDS_OFF] + 1, 5) == 0) return TOK_DBL_T;
		}
		else if (j == 7)
		{
			if (strncmp((const char *) s+1, keywords[TOK_DEFAULT- KEYWORDS_OFF] + 1, 6) == 0) return TOK_DEFAULT;
		}
		return 0;
	case 'e':
		if (j == 4)
		{
			if (strncmp((const char *) s+1, keywords[TOK_ELSE- KEYWORDS_OFF] + 1, 3) == 0) return TOK_ELSE;
			if (strncmp((const char *) s+1, keywords[TOK_ENUM- KEYWORDS_OFF] + 1, 3) == 0) return TOK_ENUM;
		}
		else if (j == 6)
		{
			if (strncmp((const char *) s+1, keywords[TOK_EXTERN- KEYWORDS_OFF] + 1, 5) == 0) return TOK_EXTERN;
		}
		return 0;
	case 'f':
		if (j == 3)
		{
			if (strncmp((const char *) s+1, keywords[TOK_FOR- KEYWORDS_OFF] + 1, 2) == 0) return TOK_FOR;
		}
		else if (j == 5)
		{
			if (strncmp((const char *) s+1, keywords[TOK_FLOAT_T- KEYWORDS_OFF] + 1, 4) == 0) return TOK_FLOAT_T;
		}
		return 0;
	case 'g':
		if (j == 4)
		{
			if (strncmp((const char *) s+1, keywords[TOK_GOTO- KEYWORDS_OFF] + 1, 3) == 0) return TOK_GOTO;
		}
		return 0;
	case 'i':
		if (j == 2)
		{
			if (s[1] == 'f') return TOK_IF;
		}
		else if (j == 3)
		{
			if (strncmp((const char *) s+1, keywords[TOK_INT_T- KEYWORDS_OFF] + 1, 2) == 0) return TOK_INT_T;
		}
		else if (j == 6)
		{
			if (strncmp((const char *) s+1, keywords[TOK_INLINE- KEYWORDS_OFF] + 1, 5) == 0) return TOK_INLINE;
		}
		return 0;
	case 'l':
		if (j == 4)
		{
			if (strncmp((const char *) s+1, keywords[TOK_LONG_T- KEYWORDS_OFF] + 1, 3) == 0) return TOK_LONG_T;
		}
		return 0;
	case 'o':
		if (j == 8)
		{
			if (strncmp((const char *) s+1, keywords[TOK_OFFSET- KEYWORDS_OFF] + 1, 7) == 0) return TOK_OFFSET;
		}
		return 0;
	case 'r':
		if (j == 6)
		{
			if (strncmp((const char *) s+1, keywords[TOK_RETURN- KEYWORDS_OFF] + 1, 5) == 0) return TOK_RETURN;
		}
		else if (j == 8)
		{
			if (strncmp((const char *) s+1, keywords[TOK_REGISTER- KEYWORDS_OFF] + 1, 7) == 0) return TOK_REGISTER;
			if (strncmp((const char *) s+1, keywords[TOK_RESTRICT- KEYWORDS_OFF] + 1, 7) == 0) return TOK_RESTRICT;
		}
		return 0;
	case 's':
		if (j == 5)
		{
			if (strncmp((const char *) s+1, keywords[TOK_SHORT_T- KEYWORDS_OFF] + 1, 4) == 0) return TOK_SHORT_T;
		}
		else if (j == 6)
		{
			if (strncmp((const char *) s+1, keywords[TOK_SGND_T- KEYWORDS_OFF] + 1, 5) == 0) return TOK_SGND_T;
			if (strncmp((const char *) s+1, keywords[TOK_SIZEOF- KEYWORDS_OFF] + 1, 5) == 0) return TOK_SIZEOF;
			if (strncmp((const char *) s+1, keywords[TOK_STATIC- KEYWORDS_OFF] + 1, 5) == 0) return TOK_STATIC;
			if (strncmp((const char *) s+1, keywords[TOK_STRUCT- KEYWORDS_OFF] + 1, 5) == 0) return TOK_STRUCT;
			if (strncmp((const char *) s+1, keywords[TOK_SWITCH- KEYWORDS_OFF] + 1, 5) == 0) return TOK_SWITCH;
		}
		return 0;
	case 't':
		if (j == 7)
		{
			if (strncmp((const char *) s+1, keywords[TOK_TYPEDEF- KEYWORDS_OFF] + 1, 6) == 0) return TOK_TYPEDEF;
		}
		return 0;
	case 'u':
		if (j == 5)
		{
			if (strncmp((const char *) s+1, keywords[TOK_UNION- KEYWORDS_OFF] + 1, 4) == 0) return TOK_UNION;
		}
		else if (j == 8)
		{
			if (strncmp((const char *) s+1, keywords[TOK_UNSGN_T- KEYWORDS_OFF] + 1, 7) == 0) return TOK_UNSGN_T;
		}
		return 0;
	case 'v':
		if (j == 4)
		{
			if (strncmp((const char *) s+1, keywords[TOK_VOID- KEYWORDS_OFF] + 1, 3) == 0) return TOK_VOID;
		}
		else if (j == 8)
		{
			if (strncmp((const char *) s+1, keywords[TOK_VOLATILE- KEYWORDS_OFF] + 1, 7) == 0) return TOK_VOLATILE;
		}
		return 0;
	case 'w':
		if (j == 5)
		{
			if (strncmp((const char *) s+1, keywords[TOK_WHILE- KEYWORDS_OFF] + 1, 4) == 0) return TOK_WHILE;
		}
	}
	return 0;
}


// once per-source-file init function for qcc
void init_qcc_state(char *fname)
{
//	cur_fname = fname;
	// besides the fact that I start out at "top level", what other state info needs setting?
	// copy down the generic tokens to the bottom of wrksp for appending
//	num_toks = cur_tokid = 0;
}


int tokenize_op(uint8_t *s, uint8_t *d, int prep_flg)
{
	uint8_t j;
	int i = 1;
	*d = c_ops[*s];			// assume no special 2-byte coding
	j = TOK_XOR;
	// many 2-byte combo operators are not valid in the preprocessor, but are valid in C
	if (prep_flg != 0) j = TOK_B_NOT;
	// can this operator be a two byte combo? == >= != && || >> << etc.
	if (c_ops[*s] >= TOK_ASSIGN && c_ops[*s] <= j)
	{
		// = > < + - & and | can all be doubled
		if (*s == s[1])
		{
			if (*s == '!') return 1;
			if (prep_flg != 0 && (*s == '+' || *s == '-')) return 1;		// ++ and -- are not valid in the preprocessor
			*d = c_ops[*s] + TOK_B_EQ - TOK_ASSIGN;
		}
		else
		{
			// < > & | ! + - * / % and ^ can have an = after them
			if (s[1] == '=')  *d = c_ops[*s] + TOK_B_LE - TOK_B_LT;
			// one last special case is the arrow -- not valid in the preprocessor
			else if (prep_flg == 0 && *s == '-' && s[1] == '>') *d = TOK_ARROW;
			else return 1;
		}
	}
	else return 1;
	return 2;			// one of the 2-byte cases triggered
}

uint8_t next_tok(uint8_t *tok_ptr)
{
	while (*++tok_ptr == TOK_NO_OP);		// skip all the no-op tokens
	return *tok_ptr;
}


uint8_t prev_tok(uint8_t *tok_ptr)
{
	while (*--tok_ptr == TOK_NO_OP);		// skip all the no-op tokens
	return *tok_ptr;
}


// check a pointer cast for a valid type format
uint8_t ptr_cast_errchk(uint8_t *pattern, uint8_t len)
{
	while (pattern[len] == TOK_MULT && len != 0) --len;			// peel off all trailing stars
	switch (len)
	{
	case 1:
		return TARGET_PTR_SIZE;						// all possibilities are legal

	case 3:					// only 1 possibility for 3 tokens: (un)signed long long
		if (*pattern == TOK_SGND_T && pattern[1] == TOK_LONG_T && pattern[2] == TOK_LONG_T) return TARGET_PTR_SIZE;
		return 0;			// anything else is an error

	case 2:					// so the length is 2 (or 0 -- illegal)
		if (*pattern == TOK_LONG_T)
		{
			if (pattern[1] == TOK_DBL_T || pattern[1] == TOK_LONG_T) return TARGET_PTR_SIZE;
		}
		else if (*pattern == TOK_SGND_T)
		{
			if (pattern[1] == TOK_CHAR_T || pattern[1] == TOK_SHORT_T ||
				pattern[1] == TOK_INT_T || pattern[1] == TOK_LONG_T) return TARGET_PTR_SIZE;
		}
	case 0:	;
	}
	return 0;			// anything else is an illegal mashup of types
}


// examine the tokens inside a cast to determine the bytesize
// note: there may be a string of NO_OP tokens before and between the type tokens
uint8_t size_of(uint8_t *s)
{
	uint8_t i, pattern[4];
	i = 0;
	while (*s == TOK_NO_OP) ++s;
	while (*s != TOK_C_PAREN)
	{
		if (i == 4) return 0;		// HIHI!! formatting error!
		if (*s != TOK_MULT && (*s < TOK_BOOL_T || *s > TOK_UNSGN_T)) return 0;		// illegal token
		pattern[i++] = *(s++);
		while (*s == TOK_NO_OP) ++s;
	}
	if (*pattern == TOK_UNSGN_T) *pattern = TOK_SGND_T;		// simplify -- unsigned and signed act identically
	if (pattern[i - 1] == TOK_MULT)								// if it's a pointer, the target architecture sets the size
		return ptr_cast_errchk(pattern, --i);
	switch (i)
	{
		case 1:				// if there is only 1 token, it's easy
			if (*pattern == TOK_BOOL_T || *pattern == TOK_CHAR_T) return 1;
			else if (*pattern == TOK_SHORT_T) return 2;
			return 4;

		case 3:				// only 1 legal possibility for 3 tokens: (un)signed long long
			if (*pattern == TOK_SGND_T && pattern[1] == TOK_LONG_T && pattern[2] == TOK_LONG_T) return 8;
			return 0;		// anything else is an error

		case 2:				// so the length is 2 (4 is illegal)
			if (*pattern == TOK_LONG_T)
			{
				if (pattern[1] == TOK_DBL_T) return 10;			// HIHI is this the right value?
				if (pattern[1] == TOK_LONG_T) return 8;
			}
			else if (*pattern == TOK_SGND_T)
			{
				if (pattern[1] == TOK_CHAR_T) return 1;
				if (pattern[1] == TOK_SHORT_T) return 2;
				if (pattern[1] == TOK_INT_T || pattern[1] == TOK_LONG_T) return 4;
			}
		case 4:	;
	}
	return 0;			// anything else is an illegal mashup of types
}


// HIHI!! almost certainly need to change the return value -- return error codes, return float values, or int values
void binary_op(uint8_t *p, uint64_t *llp, uint8_t *type)
{
	uint64_t result;
	uint8_t rval_idx, op, i, j;
	op = *p;										// preserve the operator token
	rval_idx = next_tok(p);
	if (rval_idx <= TOK_SIZEOF) rval_idx = TOK_SIZEOF + 1;		// a blank "variable" means a value of 0
	
	while (*--p == TOK_NO_OP);							// find the lval
	if (*p <= TOK_SIZEOF)  *++p = TOK_SIZEOF + 1;

	i = rval_idx - TOK_SIZEOF - 1;
	j = *p - TOK_SIZEOF - 1;
	result = 0;

	switch (op)
	{
	case TOK_B_EQ:
		if (llp[j] == llp[i]) result = BOOL_TRUE;
		break;
	case TOK_B_NE:
		if (llp[j] != llp[i]) result = BOOL_TRUE;
		break;
	case TOK_B_LT:
		if (llp[j] < llp[i]) result = BOOL_TRUE;
		break;
	case TOK_B_GT:
		if (llp[j] > llp[i]) result = BOOL_TRUE;
		break;
	case TOK_B_LE:
		if (llp[j] <= llp[i]) result = BOOL_TRUE;
		break;
	case TOK_B_GE:
		if (llp[j] >= llp[i]) result = BOOL_TRUE;
		break;
	case TOK_MULT:
		result = llp[j] * llp[i];
		break;
	case TOK_DIV:
		result = llp[j] / llp[i];
		break;
	case TOK_MOD:
		result = llp[j] % llp[i];
		break;
	case TOK_ADD:
		result = llp[j] + llp[i];
		break;
	case TOK_SUB:
		result = llp[j] - llp[i];
		break;
	case TOK_SHL:
		result = llp[j] << (char) llp[i];
		break;
	case TOK_SHR:
		result = llp[j] >> (char) llp[i];
		break;
	case TOK_B_AND:
	case TOK_AND:
		result = llp[j] & llp[i];
		break;
	case TOK_XOR:
		result = llp[j] ^ llp[i];
		break;
	case TOK_B_OR:
	case TOK_OR:
		result = llp[j] | llp[i];
	}
	// delete the variable with the shorter type -- set its type to 0 (longer types have lower "type" values)
	// -- unless the varaible was undefined (and has a value of 0)
	if (type[i] < type[j]) i = j, j = rval_idx - TOK_SIZEOF - 1;
	if (i != 0) type[i] = 0;

	// if *BOTH* variables were undefined, then allocate an actual variable for the result
	if (j == 0)
	{
		while (type[++j] != 0);		// the caller is required to have one slot open in the array
		type[j] = 1;
	}

	// if the result was from a boolean operation, set the type of the remaining variable to bool, and mask to 1 byte
	if (op == TOK_B_EQ || op == TOK_B_NE || op == TOK_B_LT || op == TOK_B_GT || op == TOK_B_LE || op == TOK_B_GE ||
		op == TOK_B_AND || op == TOK_B_OR)
	{
		type[j] = 4;
		result &= 0xff;
	}

	// store result in the remaining variable
	llp[j] = result;

	// overwrite *p with the variable token
	*p = j + TOK_SIZEOF + 1;

	// scan forward to the operator, overwrite with a no-op
	while (*++p == TOK_NO_OP);
	*p = TOK_NO_OP;
	// scan forward to the rval, overwrite it with a no-op -- if it existed!
	if (rval_idx != TOK_SIZEOF + 1)
	{
		while (*++p == TOK_NO_OP);
		*p = TOK_NO_OP;
	}
}

// take a tokenized expression (expr) with precalculated internal values (llp and ldp) and evaluate it
// HIHI maybe put the return value in an arg, and pass back an error flag? -- otherwise need *inf, so I can show_errors?
// -- move this back into cpp?? It's not generic enough.
int32_t calculate_expr(uint8_t *expr, uint64_t *llp, int32_t llcnt, double *ldp, int32_t ldblcnt)
{
	uint8_t *s, *e, *p, *c, restart, tok, type[200];
	int i;
	restart = 0;

	i = 200;
	while (--i >= 0) type[i] = 0;			// init the unused part of the array to 0
	i = llcnt;
	while (--i >= 0) type[i] = 1;			// init all the ints to uint64_t
	i = ldblcnt;
	while (--i >= 0) type[199 - i] = 42;	// init all the floats to long double
// HIHI! enforce that llcnt + ldblcnt < 256 - TOK_SIZEOF

	s = expr;
	// scan to the first ')' token, scan backwards to the first '(' token -- then evaluate that subexpression
	// -- each pair of parens get deleted after their whole subexpression is done parsing
	while (*s != TOK_C_PAREN && *s != TOK_ILLEGAL) ++s;
	while (*s != TOK_ILLEGAL)
	{
		e = s;
		while (*--s != TOK_O_PAREN);
		// find the highest precedence operator in the expression
		// no "primary" operators are allowed in the preprocessor -- scan right to left for the first active unary operator
		p = e;
		// + - ! ~ sizeof(cast) (casts)=masking
		while (*--p != TOK_O_PAREN && restart == 0)
		{
			if (*p == TOK_B_NOT || *p == TOK_NOT)		// in this compiler, bitwise not and boolean not are the same operator
			{
				tok = next_tok(p);			// the next token *must* be an rvalue for the operator
//				if (tok <= TOK_SIZEOF, or if the variable is a float?) show_error;
				// modify the rvalue at the right end of the expression
				llp[tok - TOK_SIZEOF - 1] = ~llp[tok - TOK_SIZEOF - 1];
				*p = TOK_NO_OP;							// now that the operator has been processed, delete it
				p = e;
			}
			// a minus sign is a NEG unary operator if there is another operator (and not a "variable") to the left
	// HIHI!! gotta handle the float case, too!
			else if (*p == TOK_SUB && prev_tok(p) <= TOK_SIZEOF)
			{
				tok = next_tok(p);			// the next token *must* be an rvalue for the operator
//				if (tok <= TOK_SIZEOF) show_error;
				// modify the rvalue at the right end of the expression
	// check "type" to see if this tok is a float
				llp[tok - TOK_SIZEOF - 1] = - (int64_t) llp[tok - TOK_SIZEOF - 1];
				*p = TOK_NO_OP;							// now that the operator has been processed, delete it
				p = e;
			}
			// a plus sign is a unary operator (and a no-op) if there is another operator (and not a "variable") to the left
			else if (*p == TOK_ADD && prev_tok(p) <= TOK_SIZEOF)
			{
				*p = TOK_NO_OP;							// delete the + operator and restart the scan
				p = e;
			}
			else if (*p >= TOK_BOOL_T && *p <= TOK_UNSGN_T)
			{
				// either this is an input into sizeof (as a cast), or it's a cast of the following value
				// -- so (multiple) type keywords should be the only thing inside the parens
				// -- because the preprocessor doesn't know about function or variable declarations
				c = p - 1;
				while (*c == TOK_NO_OP || (*c >= TOK_BOOL_T && *c <= TOK_UNSGN_T)) --c;
				// verify the open and close parens around this (cast)
				// obviously, s also points at the '(' and e points at the ')'
	// HIHI!!! woops! the next tok may be a *, and not a paren! Need to scan that direction, too!
				if (next_tok(p) != TOK_C_PAREN || c != s)
				{
					i = 0;
//					show_error(0,"typecast(?) not in parens",NULL,1);
				}
				p = s;
				while (*--p == TOK_NO_OP);
				if (*p == TOK_SIZEOF)		// immediately convert a sizeof(cast) into a variable (with the correct value)
				{
					// get the next open int variable number
					type[llcnt] = 0;
					i = -1;
					while (type[++i] != 0);
					if (i == llcnt) ++llcnt;				// HIHI!! need to put a 199 limit on llcnt!
					// replace the sizeof() token with the actual size, and no-op the cast
					*p = i + TOK_SIZEOF + 1;
					llp[i] = size_of(s + 1);
					type[i] = 1;
					while (*++p != TOK_C_PAREN) *p = TOK_NO_OP;
					*p = TOK_NO_OP;
					restart = 1;
				}
				else
				{
					// do an immediate mask and type modification of the rvalue variable
					tok = next_tok(p);				// get the variable index
					i = size_of(s + 1) - 1;			// get the byte count - 1
					if (i == 3) i = 2;				// convert to a mask index
					if (i < 3)
						llp[tok - TOK_SIZEOF - 1] &= size_mask[i];
					else i = 3;
					type[tok - TOK_SIZEOF - 1] = 4 - i;		// convert to a type (maybe I should reverse the order of the types?? HIHI)
				}
			}
		}
		// then the multiplicative operators -- p already points to the start
		while (restart == 0 && *++p != TOK_C_PAREN)
		{
			if (*p == TOK_MULT || *p == TOK_DIV || *p == TOK_MOD)
			{
				restart = 1;
				binary_op(p, llp, type);
			}
		}

		// then additiive
		p = s;
		while (restart == 0 && *++p != TOK_C_PAREN)
		{
			if (*p == TOK_ADD || *p == TOK_SUB)
			{
				restart = 1;
				binary_op(p, llp, type);
			}
		}

		// then shifts
		p = s;
		while (restart == 0 && *++p != TOK_C_PAREN)
		{
			if (*p == TOK_SHR || *p == TOK_SHL)
			{
				restart = 1;
				binary_op(p, llp, type);
			}
		}

		// then relational conditionals
		p = s;
		while (restart == 0 && *++p != TOK_C_PAREN)
		{
			if (*p == TOK_B_LT || *p == TOK_B_GT || *p == TOK_B_LE || *p == TOK_B_GE)
			{
				restart = 1;
				binary_op(p, llp, type);
			}
		}

		// then equality conditionals
		p = s;
		while (restart == 0 && *++p != TOK_C_PAREN)
		{
			if (*p == TOK_B_EQ || *p == TOK_B_NE)
			{
				restart = 1;
				binary_op(p, llp, type);
			}
		}

		// then bitwise operators -- AND &
		p = s;
		while (restart == 0 && *++p != TOK_C_PAREN)
		{
			if (*p == TOK_AND)
			{
				restart = 1;
				binary_op(p, llp, type);
			}
		}

		// XOR ^
		p = s;
		while (restart == 0 && *++p != TOK_C_PAREN)
		{
			if (*p == TOK_XOR)
			{
				restart = 1;
				binary_op(p, llp, type);
			}
		}

		// OR |
		p = s;
		while (restart == 0 && *++p != TOK_C_PAREN)
		{
			if (*p == TOK_OR)
			{
				restart = 1;
				binary_op(p, llp, type);
			}
		}

		// then booleans &&
		p = s;
		while (restart == 0 && *++p != TOK_C_PAREN)
		{
			if (*p == TOK_B_AND)
			{
				restart = 1;
				binary_op(p, llp, type);
			}
		}

		// ||
		p = s;
		while (restart == 0 && *++p != TOK_C_PAREN)
		{
			if (*p == TOK_B_OR)
			{
				restart = 1;
				binary_op(p, llp, type);
			}
		}

		// and last, the stupid "ternary condtional" ?	evaluate right to left, p is already set properly
		while (restart == 0 && *--p != TOK_O_PAREN)
		{
			if (*p == TOK_QMARK)
				i = 8;			// HIHI!!! gotta parse it and eliminate it from the expr
		}

		// are there any operators left between s and e? If not, destroy the parens of this fully evaluated subexpression.
		p = s + 1;
		while (*p > TOK_SIZEOF || *p == TOK_NO_OP) ++p;
		if (*p == TOK_C_PAREN)
		{
			*s = TOK_NO_OP;
			*p = TOK_NO_OP;
		}
		restart = 0;
		s = expr;
		while (*s != TOK_C_PAREN && *s != TOK_ILLEGAL) ++s;
	}
	// there must be one variable left in the expression now -- that's the return value
	p = expr + 1;
	while (*p < TOK_SIZEOF) ++p;

	if (llp[*p - TOK_SIZEOF - 1] == 0) return 0;
	return -1;
}


void handle_emit_overflow()		// struct pass_info *inf
{
	int i;
	char *b = (char *) wrksp + wrk_used_base;
	i = (char *) emit_ptr - b;
	if (outfd < 0)
	{
		outfd = qcc_create("hi1");
//		outfd = qcc_create(inout_fnames[iof_in_toggle ^ 1]);		// HIHI need to init inout_fnames, still
// HIHI and what happens if the open fails? show_error? fatal error and die?
	}
	write (outfd, b, i);			// dump out the entire emit buffer
	emit_ptr = (uint8_t *) b;		// and reset the emit ptr back to base
}


// compile a single C source file
int do_c_compile(char *fname)
{
	int in;
	// init the compiler state machine
	init_qcc_state(fname);

	// open the source file (as text, for reading)
	in = qcc_open_r (fname, 0);
	if (in < 0) return QCC_ERR_FNOTFOUND;

	// get a source file version number and timestamp, to include as extra info in the object file

	// HIHI there is an option for "preprocess only" -- which means don't do the read compression,
	// and don't tokenize -- so give it a different code path
//	if (preprocess only)
//	readfile(&in);
//	preprocess_to_text(in);
//	return 0;

	preprocess (in, (uint8_t *) fname);		// preprocessing: includes, macros, #ifs, etc.

	tokenize();			// take the messy output from the preprocessor and tokenize it prettily

	prototypes();		// parse funct/struct/union/enum/typedef info at global scope

	declarations();		// parse variables at global scope

	syntax_check();		// hopefully a complete and final check on syntax, one function at a time?

	if (total_errs != 0) return 1;

//	expression_simplification();	-- do dead code elimination

//	tcg_frontend_conversion();		-- convert tokens into a superset of TCG frontend code
// Note: tcg can only handle function returns in a single register,
// but the C99 spec requires (??) functions to be able to return complete structs, long doubles, 64bit values, etc.
// so this frontend conversion cannot be to pure tcg, I don't think -- unless there is a clever workaround for the return value thing.

	emit_to_target();	// convert to binary
	
	// XXX: -- then separate into text/data/rodata mem buffers?

	return 0;
}


// process all the "source" files in the array --
// with the intent of compiling objects, building a library, or building an executable
int compile()
{
	int i, j, err_lvl;
	char *p, *c;

	err_lvl = 0;
	i = 0;				// argument file number

	p = da_buffers[SOURCE_FNAMES];
	// loop through all the source files from the argument list
	while (i < da_entry_count[SOURCE_FNAMES])
	{
		// check the extension on the file -- if it's not .c, then assume it's an ASM file
		c = p;
		while (*c != 0) ++c;
		if (c[-2] == '.' && (c[-1] == 'c' || c[-1] == 'C'))
			j = do_c_compile(p);
//		else
//			j = do_asm_compile(p);		HIHI!  -- .s files don't need preprocessing, and .S files DO?? And they use # chars for comments! Gah!
// -- and what extension will I use for intel syntax asm files?

		if (j > err_lvl) err_lvl = j;
		p = c + 1;
		++i;				// next source file
	}

//	build_object();			// call the output formatter to dump the mem buffers out as ELF or whatever

	free (wrksp);
	return err_lvl;
}

// "main" contains the program entrypoint, and command line parsing functions
#include "main.c"
