/*  QCC - Tiny C Compiler V2
 * 
 *  Copyright (c) 2015 Bruce Ewing
 *  Licensed under 2-clause BSD, see file LICENSE in this tarball
 */

// PASS #3: sweep out global prototypes from the codestream

// The basic point of this pass is that .h files add a lot of crud to the code, in the form of
// typedefs, structure definitions, enums, and function prototypes that are never referenced by
// the sourcecode afterward. Parsing all that stuff back into its own buffer reduces the code
// size by a small but still noticeable amount, which makes the remaining code faster to parse.

// This also provides an opportunity to automatically prototype every function in the sourcecode,
// which means the programmer does not need to perform that chore anymore.

// The entire token stream (the output from the previous pass) is loaded into memory (if it is not
// already there), and edited in place. Any sections that are fully processed and do not need any
// more parsing are replaced with NO_OP tokens.


struct proto_info
{
	uint32_t cur_didx;
	uint8_t *defs;
	uint8_t *curdef;
	uint32_t *defs_idx;
	uint8_t *emitbuf;
};


// HIHI!! any of these synchk things need to have enough info to be able to calculate a line number!
// -- and the input pointer I have now may not be directly into the emit buffer, either!
// so these routines need a SECOND input pointer that IS into the emit buf? Need to subtract to get an offset, then parse line_nums.
// -- but the point is that I only need to do that crap IF I see an error.


int synchk_vardef(uint8_t *c, int32_t len)
{
	// vardefs must be a typespec, modifier, typedef idx, or su index? (or no op) -- and can include [expression] after the name!
	// rearrange the tokens into a standard order!
	return 1;
}


// len inlcudes the open and close parens -- HIHI!!! problem! I need to count LEVELS on the parens!!!
int synchk_arglist(uint8_t *c, int32_t len)
{
	uint8_t *sp;
	int32_t i;
	if (*c != TOK_O_PAREN || c[len - 1] != TOK_C_PAREN)
	// HIHI!!! these are supposed to do show_error on any 0 return, I think!
		return 0;
	while (*++c != TOK_C_PAREN)
	{
//		if (*c == TOK_ELLIPSIS)		-- HIHI!! this token doesn't exist yet
//		{
//			if (*++c != TOK_C_PAREN) return 0;
//			break;
//		}
// there is also a (*)(blah) function pointer possibility
		if (*c == TOK_O_PAREN && c[1] == TOK_MULT && c[2] == TOK_C_PAREN)
		{
			c += 3;
			sp = c;
			i = 2;
			while (*++c != TOK_C_PAREN) ++i;
			if (synchk_arglist(sp, i) == 0) return 0;
		}
		if (*c != TOK_INT_CONST && TOK_FP_CONST)
			synchk_vardef(c, 42);			// HIHI!! remove the "len" arg?
		++c;
		if (*c != TOK_COMMA && *c != TOK_C_PAREN)
			return 0;
	}
	return 1;
}


int synchk_su(uint8_t *c, int32_t len)
{
	// HIHI -- I think I need the idx and defidx stuff as args, because I must check all the subdefs??
	return 1;
}


int synchk_fn_decl(uint8_t *c, int32_t len, int32_t flag)
{
	// rearrange the tokens into a standard order!
	return 1;
}


// p must be pointing at an open curly token -- find the corresponding close curly token
uint32_t curlylen(uint8_t *p, int32_t lim)
{
	int32_t i, level;
	i = 0;
	level = 1;
	while (--lim >= 0)
	{
		++i;
		if (*++p == TOK_OCURLY) ++level;
		else if (*p == TOK_CCURLY)
		{
			if (--level == 0) return i;
		}
	}
	return i;
}


// return both a def offset and a starting def_idx
int32_t find_def_match(uint8_t tok_type, int32_t name_off, int32_t *defoff, uint8_t varname_ok, struct proto_info *inf)
{
	uint8_t *dp;
	int32_t def_idx;

	dp = inf->defs;
	def_idx = 0;
	while (1)
	{
		if (dp >= inf->curdef)
		{
			def_idx = -1;
			if (varname_ok != 0) ;
			else if (tok_type == TOK_TYPEDEF_IDX)
				show_error(0, "unknown type: ", (char *) name_strings + name_off, 1);
			else
				show_error(0, "unknown symbol: ", (char *) name_strings + name_off, 1);
			break;
		}
		if (*dp == tok_type)
		{
			if (inf->defs_idx[def_idx] == name_off)			// found the match?
				break;
		}
		while (*dp != TOK_ILLEGAL)							// if no match, scan to the next def
		{
			if (*dp >= FIRST_IDX_TOK) ++def_idx;
			++dp;
		}
		++dp;
	}
	*defoff = dp - inf->defs;
	return def_idx + 1;				// return the first index *inside the DEFINITION*
}


// maximum theoretical tokens in a declaration -- base type + modifiers + declspecs + storage attributes + extras
#define TOT_BUF_SIZE		16			// round up to a mult of 8, for fun

// HIHI!!! there are many calls to this function! -- is it only fn args that have issues with varnames???
uint8_t expand_typedef(int32_t name_off, uint8_t *p, uint32_t buf_off, uint8_t varname_ok, struct proto_info *inf)
{
	int32_t i, j, k;
	uint8_t *dp;
	// find the match for name_off
	i = find_def_match(TOK_TYPEDEF_IDX, name_off, &j, varname_ok, inf);
	if (i == 0) return 0;				// not found -- already displayed err msg
	// scan to the end, get the len, copy vardef into THE END of buf doing down
	dp = inf->defs + j;
	k = 0;								// don't include the terminator in the length
	while (*++dp != TOK_ILLEGAL) ++k;	// get a length, find the end
	buf_off += k - 1;					// the NAME_IDX token is getting overwritten
	if (buf_off >= TOT_BUF_SIZE)		// HIHI!!! there is a problem here with long function pointer typedefs!
	{
		show_error (0, "invalid typedef expansion", NULL, 1);
		return 1;
	}
	j = k;
	while (--j >= 0) *(p--) = *--dp;		// copy the def BACK INTO THE INPUT

	// then parse the new data in buf --
	// copy all non-typedefs on the front of buf to defs (and any indexes, too)
	// in case of a typedef, recurse
	while (--k >= 0)
	{
		if (*++p == TOK_NAME_IDX)			// found a recursive typedef -- that's the only thing it could be
		{
			j = inf->defs_idx[i++];
			expand_typedef(j, p, buf_off, varname_ok, inf);
		}
		else if ((*p == TOK_STRUCT || *p == TOK_UNION) && p[1] == TOK_NAME_IDX)
			p[1] += TOK_STRUCT_IDX - TOK_STRUCT;
		else if (*p >= FIRST_IDX_TOK)
		{
			j = inf->defs_idx[i++];
			inf->defs_idx[inf->cur_didx++] = j;
			*(inf->curdef++)= *p;
		}
		else *(inf->curdef++)= *p;
	}
	return 1;
}


// found a global struct/union -- find its "name" and store it away properly
// totlen is the length to the close-curly bracket, inclusive
void parse_su(uint8_t *def, int32_t totlen, int32_t idx_strt, struct proto_info *inf)
{
	uint8_t *p, *c;
	int32_t i, nxtidx;

	synchk_su(def, totlen);
	nxtidx = idx_strt + 1;
	// recursively parse and remove any struct/union subdefs within this def
	p = def + 2;
	i = totlen - 2;
	while (--i >= 0)
	{
		// count the number of idx tokens (of any type) in the definition
		++p;
		if (*p >= FIRST_IDX_TOK || *p == TOK_NOIDX_OP) ++nxtidx;
		if (*p == TOK_STRUCT || *p == TOK_UNION)
		{
			if (p[1] == TOK_NAME_IDX || p[1] == TOK_NONAME_IDX)
			{
				if (p[2] == TOK_OCURLY)
					parse_su(p, 2 + curlylen(p + 2, i - 2), nxtidx, inf);		// recurse!
			}
		}
	}

	// start over at the beginning -- everything that's left between the curly brackets is the definition
	nxtidx = idx_strt;
	i = idx_tbl[nxtidx];				// build a defidx entry -- point it at the name (get it from idx tbl)
	inf->defs_idx[inf->cur_didx++] = i;
	p = inf->curdef;
	// put a calculated IDX token at the beginning of the definition
	if (def[1] == TOK_NONAME_IDX) *p = *def + (TOK_ASTRUCT_IDX - TOK_STRUCT);
	else *p = *def + (TOK_STRUCT_IDX - TOK_STRUCT);
	*def = *(p++);				// also copy it back into wrkbuf
	def[1] = TOK_NO_OP;			// the name/noname index has been incorporated into the IDX token now
	c = def + 2;				// this pointer gets preincremented in the loop
	i = totlen - 4;
	while (--i >= 0)
	{
		if (*++c >= FIRST_IDX_TOK)
			inf->defs_idx[inf->cur_didx++] = idx_tbl[++nxtidx];
		if (*c == TOK_NOIDX_OP) ++nxtidx;
		else if (*c != TOK_NO_OP) *(p++) = *c;		// don't copy no ops
	}
	*(p++) = TOK_ILLEGAL;
	inf->curdef = p;
	p = def;				// treat "abstract" definitions and allocations slightly differently
	c += 2;
	if (*c != TOK_SEMIC) ++p;		// it had allocations, so leave the IDX *and* allocations
	else totlen = c - p + 1;		// it ended in a semicolon, so stomp on the whole thing
	while (--totlen >= 0)
	{
		if (*p == TOK_NOIDX_OP) ++p;
		else if (*p >= FIRST_IDX_TOK) *(p++) = TOK_NOIDX_OP;
		else *(p++) = TOK_NO_OP;
	}
}



// found a global typedef -- find its "name" (= NEWTYPE) and store it away properly
// 2 possible formats -- standard: typedef knowntype *** NEWTYPE;
// function pointer: typedef return_type (calling_conventions NEWTYPE)(prototype_arglist);
// XXX: **one possible issue!!** -- in the function pointer format, if the return type is char **,
//		then exactly where do the stars go, according to the standard?
void parse_typedef(uint8_t *def, uint32_t totlen, uint32_t idx_strt, struct proto_info *inf)
{
	uint8_t *p, *c;				// buf[TOT_BUF_SIZE],
	int32_t i, nxtidx;

	if (totlen < 4) show_error(0, "typedef syntax error", NULL, 1);
	nxtidx = idx_strt;
	// parse and remove struct/union/enum subdefs within the typedef (actually, there can be only one)
	p = def;
	while (*p != TOK_SEMIC)
	{
		// count the number of idx tokens (of any type) in the definition
		++p;
		if (*p >= FIRST_IDX_TOK || *p == TOK_NOIDX_OP) ++nxtidx;
		if (*p == TOK_STRUCT || *p == TOK_UNION)
		{
			if (p[1] == TOK_NAME_IDX || p[1] == TOK_NONAME_IDX)
			{
				if (p[2] == TOK_OCURLY)
				{
					// evaluate, store, and erase the definition within the curly brackets
					parse_su(p, 2 + curlylen(p+2, def + totlen - p - 2), nxtidx, inf);
					--p;		// to get the nxtidx count right, the idx that is now AT P must be counted -- so back up 1
				}
			}
		}
	}

	// find the "newtype" name within the two possible formats -- scan backwards from the semicolon
	// if the previous character before the end was a close paren, it's a function pointer typedef
	if (*--p == TOK_C_PAREN)
	{
		i = 1;
		// scan backwards through a prototype arglist to find the matching open paren
// HIHI!! the problem is that the arglist can have function pointers in it, with function pointers inside that -- must also detect the "level" of the parens!
		while (*p != TOK_TYPEDEF && *p != TOK_O_PAREN)
		{
			if (*--p >= FIRST_IDX_TOK) --nxtidx;
			++i;
		}
		// the prev token must be a close paren, and the previous token before *that* must be the "newtype" name_idx
		if (*p != TOK_O_PAREN || p[-1] != TOK_C_PAREN || p[-2] != TOK_NAME_IDX || synchk_arglist(p, i) == 0)
			show_error(0, "typedef syntax error", NULL, 1);
		// start building a defidx entry -- it should point at the name (from idxidx)
		i = idx_tbl[--nxtidx];
		inf->defs_idx[inf->cur_didx++] = i;
		// then scan backwards some more through the function declaration, to verify more syntax
		// note: the declaration must be *complete* -- the logic cannot accept modifiers later
		p -= 2;
		*p = TOK_FUNCT_PTR;			// replace the "name idx" token with a standin function ptr token
		i = 0;
		while (*p != TOK_TYPEDEF && *p != TOK_O_PAREN)
		{
			if (*--p >= FIRST_IDX_TOK) --nxtidx;
			++i;
		}
		c = def;
		i = p - def;		// get the distance back to beginning of def
// HIHI!! still need to deal with the recursive typedef stuff -- maybe use sc_fn_decl()
		if (*p != TOK_O_PAREN || synchk_fn_decl(p + 1, i, 0) == 0)
			show_error(0, "typedef syntax error", NULL, 1);
		p = inf->curdef;
		*(p++) = TOK_FUNCT_IDX;
		// start at the beginning and verify the return type syntax
		if (synchk_vardef(c, i) == 0) show_error(0, "typedef syntax error", NULL, 1);
		i = totlen - 3;
	}
	else if (*p != TOK_NAME_IDX)		// otherwise, p *must* be pointing at the "newtype" name index token
	{
		show_error(0, "typedef syntax error", NULL, 1);
		return;
	}
	else						// build a defidx entry -- point it at the name (get it from idx tbl)
	{
		i = idx_tbl[--nxtidx];
		inf->defs_idx[inf->cur_didx++] = i;
		p = inf->curdef;
		*(p++) = TOK_TYPEDEF_IDX;
		// copy all the rest of the bytes (totlen - 3 of them, starting at beg + 1) into def
		i = totlen - 3;
		c = def;
		if (synchk_vardef(c, i) == 0) show_error(0, "typedef syntax error", NULL, 1);
	}
	nxtidx = idx_strt;
	while (--i >= 0)
	{
		if (*++c >= FIRST_IDX_TOK)
			inf->defs_idx[inf->cur_didx++] = idx_tbl[nxtidx++];
		if (*c == TOK_NOIDX_OP) ++nxtidx;
		else if (*c != TOK_NO_OP) *(p++) = *c;		// don't copy no ops
	}
	*(p++) = TOK_ILLEGAL;
	inf->curdef = p;
	memset (def, 1, totlen);
}


//	HIHI!! must either malloc space, or postpose saving the arg list as another def until after the current one is complete! Ack!
// -- In reality, the arglist storage will always be tiny (and there will only be 1), so I can use the off[] buffer from cpp.c?
// any []'s must be at the end of the arg, any variable name must immedately precede the first [
int32_t parse_fn_arg(uint8_t *beg, uint8_t *end, uint8_t *buf, uint32_t idx_strt, uint8_t proto_flg, struct proto_info *inf)
{
	int16_t i, len, vname_off;
	uint8_t *p;
	i = len = (int16_t)(end - beg);
	p = end;
	while (p[-1] == TOK_CSQUARE)
	{
		while (*--p != TOK_OSQUARE && i > 0) --i;
	}
	vname_off = i;
	i = len;

	// since this is a function *definition*, args must be: typedefs, typespecs, variables, ellipses, or function ptrs
	// -- function pointers are the bad case -- they can be distinguished by all the extra parentheses
	p = beg;
	while (i > 0 && *p != TOK_O_PAREN && *p != TOK_OSQUARE) ++p, --i;
	if (*p == TOK_O_PAREN)
	{
	// HIHI!! use the off[256] buffer from cpp to temporarily store the arglist? Is there any point in that?
// HIHI!! then malloc a space to store that function ptr arglist?
// And store that malloc ptr on a malloc'ed list! With a count! and an idx_strt for each arglist
		*(inf->curdef++) = TOK_FUNCT_PTR;
		while (--len > 0)
		{
			if (*++p >= FIRST_IDX_TOK)
				++idx_strt;
		}
		return idx_strt;
	}
//		if (len >= TOT_BUF_SIZE) show_error();
	// copy len chars into buf, below the 0
	p = buf + TOT_BUF_SIZE - 1 - len;
	memcpy (p, beg, len);
	while (--len >= 0)
	{
		if (*p == TOK_NAME_IDX)
		{
			if (len != 0 && p[1] != TOK_OSQUARE)			// extension: add TOK_ASSIGN to have args with default values?
				expand_typedef(idx_tbl[idx_strt], p, len, 0, inf);
			else
			{
				if (expand_typedef(idx_tbl[idx_strt], p, len, 1, inf) == 0)
				{
					// since it wasn't a typedef, it IS an argument variable name -- copy it unmodified to defs
					*(inf->curdef++) = TOK_NAME_IDX;
					inf->defs_idx[inf->cur_didx++] = idx_tbl[idx_strt];
				}
				// otherwise, real fn definitions REQUIRE variable names
				else if (proto_flg == 0) show_error (0, "missing variable name", NULL, 1);
			}
			++idx_strt;
		}
		else if ((*p == TOK_STRUCT || *p == TOK_UNION) && p[1] == TOK_NAME_IDX)
			p[1] += TOK_STRUCT_IDX - TOK_STRUCT;
		else
		{
			if (*p >= FIRST_IDX_TOK)
				inf->defs_idx[inf->cur_didx++] = idx_tbl[idx_strt++];
			*(inf->curdef++) = *p;
		}
		++p;
	}
	return idx_strt;
}


int32_t parse_fn(uint8_t *def, uint32_t totlen, uint32_t idx_strt, struct proto_info *inf, uint32_t *m)
{
	uint8_t buf[TOT_BUF_SIZE], *p, *c, is_proto, level;
	int32_t i, j, name_off;

	is_proto = level = 0;
	if (def[totlen] == TOK_SEMIC)		 // if this is a prototype, set a flag
		is_proto = 1;
	// start building a defidx entry -- it should point at the name (from idx tbl)
	name_off = idx_tbl[idx_strt - 1];
	--*m;								// back up *m until it's the first *compressed* idx of the definition (in the emit buf)
	inf->defs_idx[inf->cur_didx++] = name_off;
	// then scan backwards some more through the function declaration, to verify more syntax
	p = def - 2;						// start one token before the function name token
	c = buf + TOT_BUF_SIZE - 1;
	*(c--) = TOK_ILLEGAL;
	j = idx_strt - 2;					// and one index before the function name index
	while (*p != TOK_SEMIC && *p != TOK_NO_OP && *p != TOK_CCURLY)
	{
		*(c--) = *p;
		if (*(p--) >= FIRST_IDX_TOK) --*m, --j;
	}
	i = def - (++p);					// get the distance back to beginning of definition (= p)
	if (is_proto != 0)
		*(inf->curdef++) = TOK_FN_PROT_IDX;
	else
		*(inf->curdef++) = TOK_FUNCT_IDX;		// begin storing prototype in defs (defs_idx is already set)
	// parse any typedefs in the declaration, and copy all of it into inf->curdef
	// note: j was backed up 1 too far
	while (--i > 0)
	{
		if (*++c == TOK_NAME_IDX)
			expand_typedef(idx_tbl[++j], c, i, 0, inf);
		else if ((*c == TOK_STRUCT || *c == TOK_UNION) && c[1] == TOK_NAME_IDX)
			c[1] += TOK_STRUCT_IDX - TOK_STRUCT;
		else
		{
			if (*c >= FIRST_IDX_TOK)
				inf->defs_idx[inf->cur_didx++] = idx_tbl[++j];
			*(inf->curdef++) = *c;
		}
	}
	// rescan the emitted function declaration bytes and look for any TOK_INLINE
	j = 0;
	c = inf->curdef - 1;
	while (*c != TOK_FUNCT_IDX && *c != TOK_FN_PROT_IDX)
	{
		if (*c == TOK_INLINE)
		// HIHI!!!!! convert this to a TOK_INLINE_IDX and add a slot in the defs_idx to point to the macro?
		// -- alternately, I could always put the macro immediately after the terminating 0 -- then I don't need an index
			j = 1;				// flag all inline functions for special processing
		--c;
	}
	// synchk_fn_decl (c + 1);		-- HIHI!! this fn should just scan until it hits an O_PAREN

	c = def + 1;				// point just past the O_PAREN
	i = totlen - 2;				// length from c to ending close paren
	totlen += def - p;			// back totlen and def up to BOL
	def = p;
	// then parse each function argument: do typedef expansions, do syntax checking
	*(inf->curdef++) = TOK_O_PAREN;
	while (i > 0)
	{
		p = c;					// save a ptr to the beginning of this arg, and find the end
		while ((*c != TOK_COMMA || level != 0) && i > 0)
		{
			// a single argument can be a function pointer (containing its own complete argument list!)
			// and those arglists can have commas inside them -- which must be skipped!
			if (*c == TOK_O_PAREN)
				++level;
			if (*c == TOK_C_PAREN)
				--level;
			--i;
			++c;
		}
		// parse the arg into defs, then skip the comma
		idx_strt = parse_fn_arg(p, c, buf, idx_strt, is_proto, inf);
		if (*c == TOK_COMMA) --i, *(inf->curdef++) = *(c++);
	}
	*(inf->curdef++) = TOK_C_PAREN;
	*(inf->curdef++) = TOK_ILLEGAL;

	if (is_proto != 0)		 // if this is a prototype, destroy it including the semicolon
	{
		memset (def, 1, totlen + 1);
		return j;
	}
	memset (def, 1, totlen);
	*def = TOK_FUNCT_IDX;
	idx_tbl[*m] = name_off;
	++*m;
	return j;
}


void parse_gbl_vardefs(uint8_t *def, uint32_t totlen, uint32_t idx_strt, struct proto_info *inf)
{
	uint8_t buf[TOT_BUF_SIZE], *p, *sp;
	int32_t i, j;
	buf[15] = TOK_ILLEGAL;
	i = idx_strt;
	p = def;
	sp = inf->curdef;		// save the starting ptr of the vardef
	// loop forward through the tokens until there is one with a comma, open square, equal, or semicolon AFTER it
	// -- "recursively" parse all typedefs found, then verify syntax of this variable definition string
	while (p[1] != TOK_COMMA && p[1] != TOK_SEMIC && p[1] != TOK_ASSIGN && p[1] != TOK_OSQUARE)
	{
		if (*p == TOK_NAME_IDX)			// found a typedef -- that's the only thing it could be
		{
			j = idx_tbl[i++];
			expand_typedef(j, buf + 15, 1, 0, inf);
		}
		else if (*p >= FIRST_IDX_TOK)	// found a struct/union/enum/funct ptr
		{
			j = idx_tbl[i++];
			inf->defs_idx[inf->cur_didx++] = j;
			*(inf->curdef++)= *p;
		}
		else *(inf->curdef++)= *p;				// found an intrinsic typespec
		++p;
	}

	synchk_vardef(sp, inf->curdef - sp);		// do a syntax check on the full typespec
	// then finish copying the declared variable names all the way out to the semicolon
	while (*p != TOK_SEMIC)
	{
		if (*p >= FIRST_IDX_TOK)
		{
			j = idx_tbl[i++];
			inf->defs_idx[inf->cur_didx++] = j;
		}
		*(inf->curdef++)= *(p++);
	 }
	*(inf->curdef++) = TOK_ILLEGAL;
	memset (def, TOK_NO_OP, totlen + 1);		// stomp on the processed declaration in the emit buffer
}


// bitflags contained in def_flg
#define DEF_SAWNAME		1
#define DEF_SAWSU		2
#define DEF_SAWTDEF		4
#define DEF_SAWFN		8
#define MIN_TRIG		16
#define DEF_SUTRIG		16
#define DEF_TDEFTRIG	32
#define DEF_FNTRIG		64


// suck all the global function, struct, union, enum, and typedef prototypes out of the token stream
// -- which means creating a new "defs" token stream, and splitting out a defs_idx from the idx_tbl
uint32_t proto_pass(struct proto_info *inf)
{
	uint32_t j, k, m, idx_strt;
	uint8_t bol_flag, *p, *sp, def_flg;
	int16_t level;

	bol_flag = 1;
	def_flg = 0;
	level = 0;
	k = m = 0;
	p = sp = wrksp_top;
	p[-1] = TOK_NO_OP;				// HIHI!!!! I need to make sure this mem location is valid!! malloc 1 extra byte!
	if (inf->emitbuf != NULL)
		p = sp = inf->emitbuf;
	while (1)
	{
if (p - emit_base > 676)
	j = 0;
		if (level == 0 && def_flg <= 1)
		{
			idx_strt = k;			// save the *next* idx index
			sp = p;					// save a ptr to the beginning of any struct/union/enum prototype
		}

		switch (*p)
		{
		case TOK_STRUCT:			// detect struct/union/emun *definitions* (as opposed to declarations)
		case TOK_UNION:
			if (level == 0)
			{
				if (def_flg <= 1) def_flg = DEF_SAWSU;
				bol_flag = 0;
			}
			break;

		case TOK_NAME_IDX:
		case TOK_NONAME_IDX:
			if (level == 0)
			{
				def_flg ^= DEF_SAWNAME;
				// TWO names in a row after a "struct" means that it's a variable declaration, not a definition
				if (def_flg == DEF_SAWSU) def_flg = 0;
			}

		case TOK_INT_CONST:
		case TOK_FP_CONST:
			j = idx_tbl[k++];							// must keep k aligned with the token stream
			if (++m != k) idx_tbl[m - 1] = j;			// rebuild a compressed idx_tbl
			if (level == 0) bol_flag = 0;
			break;

		case TOK_TYPEDEF:
			if (bol_flag != 0)
			{
				if (level == 0) def_flg = DEF_SAWTDEF;
				bol_flag = 0;
// HIHI can you do typedefs with only local scope? I suppose you could TRY, but it doesn't get you much?
			}
			break;

		case TOK_NO_OP:			// sometimes semicolons can get converted into no ops in the input
		case TOK_SEMIC:
			if (level == 0)
			{
				if (p[-1] == TOK_C_PAREN && (def_flg & DEF_SAWFN) != 0)
					def_flg = DEF_FNTRIG;											// completed proto fn arglist
				else if ((def_flg & DEF_SAWTDEF) != 0) def_flg = DEF_TDEFTRIG;		// completed typedef
				bol_flag = 1;
			}
			break;

		case TOK_OCURLY:
			if (level == 0 && p[-1] == TOK_C_PAREN && (def_flg & DEF_SAWFN) != 0)
				def_flg = DEF_FNTRIG;												// completed function arglist

			++level;
			break;

		case TOK_CCURLY:
			--level;
			if (level < 0) show_error(0, "too many '}' chars detected", NULL, 1);
			else if (level == 0)
			{
				// test for completed struct/union definition
				bol_flag = 0;
				if (def_flg == (DEF_SAWSU|DEF_SAWNAME)) def_flg = DEF_SUTRIG;
				else bol_flag = 1;
			}
			break;

		case TOK_ENDOFBUF:
			return m;

		case TOK_O_PAREN:				// detect the beginning of a function arglist (open paren preceded by a name)
			if (level == 0 && def_flg <= 1 && p[-1] == TOK_NAME_IDX) def_flg = DEF_SAWFN;

		default:
			if (level == 0)
			{
				// must kill a potential struct def on: struct name *
				// (anything else except a curly bracket or name is illegal anyway)
				if (def_flg == (DEF_SAWSU|DEF_SAWNAME)) def_flg = 0;
				bol_flag = 0;
			}

		}		// end of switch on *p

		++p;
		if (def_flg >= MIN_TRIG)			// found a definition?
		{
			m -= k - idx_strt;
			j = p - sp;
			if (def_flg == DEF_SUTRIG) parse_su(sp, j, idx_strt, inf);
			else if (def_flg == DEF_TDEFTRIG) parse_typedef(sp, j, idx_strt, inf);
			else parse_fn(sp, j - 1, idx_strt, inf, &m);		// had to parse 1 too far to verify detection
			def_flg = 0;
		}
	}		// infinite loop
}


// global declarations belong in .bss, .data, or .rodata -- not in the .text section
// -- and they should be the only unprocessed thing left at global scope (besides functions)
uint32_t declarations(struct proto_info *inf)
{
	// HIHI global declarations also end up as symbols -- but I need to know if they are private (static) or not
	// should I use the underbar thing to make some of them hidden scope?
	// -- but that means I need to sort each defined global variable by destination -- is it finally time that I need the Sym array?
	uint32_t j, k, m;
	uint8_t *p, *sp, *decl_strt;
	int16_t level;

	decl_strt = inf->curdef;
	k = m = 0;
	level = 0;
	p = wrksp_top;
	if (inf->emitbuf != NULL)
		p = inf->emitbuf;
	while (*p != TOK_ENDOFBUF)
	{
		while (*p == TOK_NO_OP || *p == TOK_NOIDX_OP) ++p;
		// at top level, the only things that remain are variable defs, variable initializations, and functions
		if (level == 0)
		{
			// the function definitions should all begin with a TOK_FUNCT_IDX -- skip over them
			if (*p == TOK_FUNCT_IDX)
			{
				j = idx_tbl[k++];							// must keep k aligned with the token stream
				if (++m != k) idx_tbl[m - 1] = j;			// rebuild a compressed idx_tbl
				sp = name_strings + j;			// HIHI!!! debugging!
				sp += level;
			}
			else if (*p == TOK_OCURLY) ++level;
			else if (*p != TOK_ENDOFBUF)
			{
				// anything else is the beginning of a variable definition
				sp = p;
				j = k;
				// advance the k value, before the underlying data gets erased
				while (*p != TOK_SEMIC)
				{
					if (*(p++) >= FIRST_IDX_TOK) ++k;
				}
				parse_gbl_vardefs(sp, p - sp, j, inf);
			}
		}
		else if (*p == TOK_OCURLY) ++level;
		else if (*p == TOK_CCURLY) --level;
		else if (*p >= FIRST_IDX_TOK)					// copy/skip function definitions (not at level 0)
		{
			j = idx_tbl[k++];							// must keep k aligned with the token stream
			if (++m != k) idx_tbl[m - 1] = j;			// rebuild a compressed idx_tbl
		}
		++p;
	}
	*(inf->curdef) = TOK_ILLEGAL;
	inf->curdef = decl_strt;
	return m;
}



void post_proto(struct proto_info *inf)
{
	uint8_t *eb, *p;
	int32_t i;
	uint32_t ln_in, ln_out, linfil, lncnt, j;
	if (inf->emitbuf == NULL)
		eb = inf->curdef;
	else
		eb = emit_base;
	emit_ptr = eb;
	p = emit_base;

	// memory contains: line nums, defs_idx (compressed), defs (including declarations) ... emit_buf, idx_tbl (compressed), int&float, name strings
	// -- throw away line_nums and recompress everything!

	ln_in = ln_out = linfil = j = 0;
	while (*p != TOK_ENDOFBUF)
	{
		while (*p == TOK_NO_OP || *p == TOK_NOIDX_OP) ++p;
		if ((line_nums[ln_in] >> 8) <= (unsigned)(p - emit_base))
		{
			if (j != 0) linfil = j;			// when 'j' is 0 it does not contain a valid linfil
			j = lncnt = 0;
			// update the entries in the line_nums buffer to point into the *compressed* emit buffer
			do {
				lncnt += line_nums[ln_in] & 0xff;
				if ((line_nums[++ln_in] & 0xff) == 0)
				{
					j = line_nums[ln_in++] | 1;
					if ((line_nums[ln_in] >> 8) <= (unsigned)(p - emit_base)) lncnt = 0, linfil = j;
				}
			} while ((line_nums[ln_in] >> 8) <= (unsigned)(p - emit_base));
			if (lncnt > 0)
			{
				if ((linfil & 1) != 0) line_nums[ln_out++] = linfil & ~1;
				do {
					i = lncnt;
					if (i > 255) i = 255;
					lncnt -= i;
					line_nums[ln_out++] = i | ((emit_ptr - eb) << 8);
				} while (lncnt > 0);
			}
		}
		*(emit_ptr++) = *(p++);
	}
	*(emit_ptr++) = TOK_ENDOFBUF;
	emit_base = eb;

	// memory contains: line nums, defs_idx (compressed), defs (including declarations) ... emit_buf, idx_tbl (compressed), int&float, name strings
	// copydown compressed idx_tbl
	// emit buffer? -- needs to be compressed, along with line_nums
	// copyup defs, defs_idx
	// then idx_tbl
}



// the token stream is currently in a file, and must be completely loaded into RAM
// -- either into the current workspace, or into a malloc
void ld_tokenstream(struct proto_info *inf)
{
	int i, j;
	uint8_t *p;
	// open the file and read **the entire thing** in binary mode (size = num_toks)
	wrksp_top = wrksp + wrk_rem - idxidx * 4 - num_toks;

	infd = qcc_open_r("hi1", 1);
//	inf_infd = qcc_open_r(inout_fnames[??], 1);
// HIHI! if num_toks will not fit in the wrksp with a little left for processing, then malloc it! (plus a couple extra bytes, to inf->emitbuf)
//	else
		p = wrksp_top;
	i = j = num_toks;		// standard read loop
	while (i != 0)
	{
		i = read(infd, p, j);
		p += i;
		j -= i;
	}
	*p = TOK_ENDOFBUF;

	qcc_close (infd);
	outf_exists = 0;		// reset the flag
	infd = outfd = -1;
}


void build_symtbl()
{
	// memory contains: line nums, defs_idx (compressed), defs (including declarations) ... emit_buf, idx_tbl (compressed), int&float, name strings
}


void syntax_check()
{
	// apply all the grammar rules with a state machine
	// finalize all ambiguous operators (eg: * for mult or deref)
}


// process global funct prototypes, typedefs, struct, union, and enum definitions
void prototypes()
{
	uint32_t j, wrk_avail;
	struct proto_info inf;

	emit_base = wrksp_top;
	// At this point, the logic requires somewhat more random access to the token stream,
	// so it is *required* to fit within a malloc, now.
	if (outf_exists != 0)		// read in the tokenized data, if it was dumped into a file
		ld_tokenstream(&inf);
	wrk_avail = wrksp_top - wrksp;
	emit_ptr = emit_base;

//	if ((lnum_cnt + idxidx) * 4 + num_toks > wrk_avail && lnum_cnt > wrk_avail / 32)
//		handle_lnums_overflow();
// set up the base_ptrs & etc, if these tables have to be truncated to fit in mem
	// maximum possible size of defs_idx is the old idxidx size, max possible size of defs is num_toks
	inf.defs_idx = line_nums + lnum_cnt;				// put the defs index table just above line_nums
	inf.defs = (uint8_t *)(inf.defs_idx + idxidx);		// and defs just above that
	inf.curdef = inf.defs;
	inf.cur_didx = 0;
	inf.emitbuf = NULL;
	j = proto_pass(&inf);		// process prototypes -- including struct definitions, etc.

	j = declarations(&inf);		// process global variable declarations
	idxidx = j;
	build_symtbl();		// HIHI!!! can/should I combine this with either declarations or the synchk? Declarations seems best?

	syntax_check();
	post_proto(&inf);			// move the buffers around in mem a little more
}


