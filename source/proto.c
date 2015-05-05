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


// HIHI!!! make sure there is a semicolon just before EOF, once the input is tokenized
// HIHI!! any of these synchk things need to have enough info to be able to calculate a line number!
// -- and the input pointer I have now may not be directly into the emit buffer, either!
// so these routines need a SECOND input pointer that IS into the emit buf? Need to subtract to get an offset, then parse line_nums.
// -- but the point is that I only need to do that crap IF I see an error.


// HIHI these routines move into syntax.c, of course
int synchk_vardef(uint8_t *c, int32_t len)
{
	// vardefs must be a typespec, modifier, typedef idx, or sae index? (or no op)
	// in the proper order, with no repeats, and no logic conflicts
	return 1;
}


int synchk_arglist(uint8_t *c, int32_t len)
{
	// possibilities are: vardef, name_idx, comma, (assign?), number_idx, square brackets, parens (I think), ellipsis, (or no op)
	return 1;
}


// check the syntax of the declspecs and other stuff (sometimes including the return value) of a function
int synchk_fn_decl(uint8_t *c, int32_t len, int32_t flag)
{
	// possibilities are: typespecs, name_idx, fnp_idx, declspecs
	// -- the possibilities may change slightly based on whether this is a function pointer typdef, or a normal fn declaration
	return 1;
}


int synchk_sue(uint8_t *c, int32_t len)
{
	// HIHI -- I think I need the idx and defidx stuff as args, because I must check all the subdefs??
	return 1;
}


void set_to_noop(uint8_t *p, int32_t len)
{
	while (--len >= 0)
	{
		if (*p == TOK_NOIDX_OP) ++p;
		else if (*p >= FIRST_IDX_TOK) *(p++) = TOK_NOIDX_OP;
		else *(p++) = TOK_NO_OP;
	}
}


// p must be pointing at an open curly token -- find the corresponding close curly token
uint32_t curlylen(uint8_t *p, int32_t lim)
{
	int32_t i, level;
	i = level = 0;
	while (--lim >= 0)
	{
		++i;
		if (*p == TOK_OCURLY) ++level;
		else if (*p == TOK_CCURLY)
		{
			if (--level == 0) return i;
		}
		++p;
	}
	return i;
}


// found a global struct/union/enum -- find its "name" and store it away properly
// totlen is the length to the close-curly bracket, inclusive
void parse_sue(uint8_t *def, uint32_t totlen, int32_t idx_strt, struct proto_info *inf)
{
	uint8_t *p, *c;
	int32_t i, nxtidx;

	synchk_sue(def, totlen);
	nxtidx = idx_strt + 1;
	// recursively parse and remove any struct/union/enum subdefs within this def
	p = def + 2;
	i = totlen - 2;
	while (--i >= 0)
	{
		// count the number of idx tokens (of any type) in the definition
		++p;
		if (*p >= FIRST_IDX_TOK || *p == TOK_NOIDX_OP) ++nxtidx;
		if (*p == TOK_STRUCT || *p == TOK_UNION || *p == TOK_ENUM)
		{
			if (p[1] == TOK_NAME_IDX || p[1] == TOK_NONAME_IDX)
			{
				if (p[2] == TOK_OCURLY)
					parse_sue(p, 2 + curlylen(p + 2, i - 2), nxtidx, inf);		// recurse!
			}
		}
	}

	// start over at the beginning -- everything that's left between the curly brackets is the definition
	nxtidx = idx_strt;
	i = idx_tbl[nxtidx];				// build a defidx entry -- point it at the name (get it from idxidx)
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
	// if the definition had no allocations after it, delete the whole thing
	p = def;
	c += 2;
	if (*c != TOK_SEMIC) ++p;		// it had allocations, so leave the IDX *and* allocations
	else totlen = c - p + 2;		// it ended in a semicolon, so stomp on the whole thing
	set_to_noop(p, totlen - 1);
}



// found a global typedef -- find its "name" (= NEWTYPE) and store it away properly
// 2 possible formats -- standard: typedef knowntype *** NEWTYPE;
// function pointer: typedef return_type (calling_conventions NEWTYPE)(prototype_arglist);
// XXX: **one possible issue!!** -- in the function pointer format, if the return type is char **,
//		then exactly where do the stars go, according to the standard?
void parse_typedef(uint8_t *def, uint32_t totlen, uint32_t idx_strt, struct proto_info *inf)
{
	uint8_t *p, *c;
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
		if (*p == TOK_STRUCT || *p == TOK_UNION || *p == TOK_ENUM)
		{
			if (p[1] == TOK_NAME_IDX || p[1] == TOK_NONAME_IDX)
			{
				if (p[2] == TOK_OCURLY)
				{
					// evaluate, store, and erase the definition within the curly brackets
					parse_sue(p, 2 + curlylen(p+2, def + totlen - p - 2), nxtidx, inf);
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
		p -= 2;
		*p = TOK_FUNCT_PTR;			// replace the "name idx" token with a standin function ptr token
		i = 0;
		while (*p != TOK_TYPEDEF && *p != TOK_O_PAREN)
		{
			if (*--p >= FIRST_IDX_TOK) --nxtidx;
			++i;
		}
		c = def;
		i = p - c;		// get the distance back to beginning of def
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
	else						// build a defidx entry -- point it at the name (get it from idxidx)
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


void parse_fn(uint8_t *def, uint32_t totlen, uint32_t idx_strt, struct proto_info *inf, uint32_t *m)
{
	uint8_t *p, *c;
	int32_t i, nxtidx, idxval;
	nxtidx = idx_strt;
	// note: this synchk call is supposed to include the enclosing parens
	if (synchk_arglist(def, totlen) == 0)
		show_error(0, "function definition syntax error", NULL, 1);
	// start building a defidx entry -- it should point at the name (from idxidx)
	idxval = idx_tbl[--nxtidx];			// back up nxtidx unit it's the first uncompressed idx
	--*m;								// back up *m until it's the first *compressed* idx of the definition
	inf->defs_idx[inf->cur_didx++] = idxval;
	// then scan backwards some more through the function declaration, to verify more syntax
	p = def - 1;
	while (*p != TOK_SEMIC && *p != TOK_NO_OP && *p != TOK_CCURLY)
	{
		if (*--p >= FIRST_IDX_TOK) --nxtidx, --*m;
		++totlen;
	}
	i = def - (++p);		// get the distance back to beginning of definition (= p)
// HIHI in this case, the return type and declspecs are intermingled! Must parse them both together for syntax.
// -- must pass in nxtidx??
	if (synchk_fn_decl(p, i - 1, 1) == 0)
		show_error(0, "function definition syntax error", NULL, 1);
	c = def = p;
	p = inf->curdef;
	*(p++) = TOK_FUNCT_IDX;

	i = totlen;
	while (--i >= 0)
	{
		if (*c >= FIRST_IDX_TOK)
			inf->defs_idx[inf->cur_didx++] = idx_tbl[nxtidx++];
		if (*c == TOK_NOIDX_OP) ++nxtidx;
		else if (*c != TOK_NO_OP) *(p++) = *c;		// don't copy no ops
		++c;
	}
	*(p++) = TOK_ILLEGAL;
	inf->curdef = p;
	if (def[totlen] == TOK_SEMIC)		 // if this is a prototype, destroy it including the semicolon
	{
		memset (def, 1, totlen + 1);
		return;
	}
	memset (def, 1, totlen);
	*def = TOK_FUNCT_IDX;
	idx_tbl[*m] = idxval;
	++*m;
}


// return both a def offset and a starting def_idx
int32_t find_def_match(uint8_t tok_type, int32_t name_off, int32_t *defoff, struct proto_info *inf)
{
	uint8_t *dp;
	int32_t def_idx;

	dp = inf->defs;
	def_idx = 0;
	while (1)
	{
		if (dp >= inf->curdef)
		{
			show_error(0, "unknown name: ", (char *) name_strings + name_off, 1);
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


#define TOT_BUF_SIZE		16

int expand_typedef(int32_t name_off, uint8_t *p, uint32_t buf_off, struct proto_info *inf)
{
	int32_t i, j, k;
	uint8_t *dp;

	// find the match for name_off
	i = find_def_match(TOK_TYPEDEF_IDX, name_off, &j, inf);
	// scan to the end, get the len, copy vardef into THE END of buf doing down
	dp = inf->defs + j;
	k = 0;
	while (*++dp != TOK_ILLEGAL) ++k;	// get a length, find the end
	buf_off += k;
	if (buf_off > TOT_BUF_SIZE)
	{
		show_error (0, "invalid typedef expansion", NULL, 1);
		return 1;
	}
	while (--k >= 0) *--p = *--dp;		// copy the def BACK INTO THE INPUT

	// then parse the new data in buf --
	// copy all non-typedefs on the front of buf to defs (and any indexes, too)
	// when I run into a typedef, recurse
	while (*p != TOK_ILLEGAL)
	{
		if (*p == TOK_NAME_IDX)			// found a typedef -- that's the only thing it could be
		{
			j = inf->defs_idx[i++];
			if (expand_typedef(j, p, buf_off, inf) == 0)
				show_error(0, "unknown type: ", (char *) name_strings + j, 1);
		}
		else if (*p >= FIRST_IDX_TOK)
		{
			j = inf->defs_idx[i++];
			inf->defs_idx[inf->cur_didx++] = j;
			*(inf->curdef++)= *p;
		}
		else
		{
			if (*p == TOK_STRUCT || *p == TOK_UNION || *p == TOK_ENUM)		// many SUE tokens have not been replaced with IDX tokens yet
			{
				if (p[1] != TOK_NAME_IDX)
					show_error(0, "illegal type definition: ", (char *) name_strings + j, 1);
				*(inf->curdef++)= *p + (TOK_STRUCT_IDX - TOK_STRUCT);		// replace 
				j = inf->defs_idx[i++];
				inf->defs_idx[inf->cur_didx++] = j;
				++p;
			}
			else *(inf->curdef++)= *p;
		}
		++p;
	}
	return 1;
}


void parse_gbl_vardefs(uint8_t *def, uint32_t totlen, uint32_t idx_strt, struct proto_info *inf)
{
	uint8_t buf[TOT_BUF_SIZE], *p, *sp;
	int32_t i, j;
	buf[15] = TOK_ILLEGAL;
	i = idx_strt;
	p = def;
	sp = inf->curdef;		// save the starting ptr of the vardef
	// loop forward through the tokens, until there is one with a comma, open square, equal, or semicolon after it
	// -- "recursively" parse all typedefs found, then verify syntax of this variable definition string
	while (p[1] != TOK_COMMA && p[1] != TOK_SEMIC && p[1] != TOK_ASSIGN && p[1] != TOK_OSQUARE)
	{
		if (*p == TOK_NAME_IDX)			// found a typedef -- that's the only thing it could be
		{
			j = idx_tbl[i++];
			if (expand_typedef(j, buf + 15, 1, inf) == 0)
				show_error(0, "unknown type: ", (char *) name_strings + j, 1);
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
#define DEF_SAWSUE		2
#define DEF_SAWTDEF		4
#define DEF_SAWFN		8
#define MIN_TRIG		16
#define DEF_SUETRIG		16
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
	p[-1] = TOK_NO_OP;				// HIHI!!!! I need to make sure this mem location is valid!! malloc 8 extra bytes!
	if (inf->emitbuf != NULL)
		p = sp = inf->emitbuf;
	while (1)
	{
		if (level == 0 && def_flg <= 1)
		{
			idx_strt = k;			// save the *next* idx index
			sp = p;					// save a ptr to the beginning of any struct/union/enum prototype
		}

		switch (*p)
		{
		case TOK_STRUCT:			// detect struct/union/emun *definitions* (as opposed to declarations)
		case TOK_UNION:
		case TOK_ENUM:
			if (level == 0)
			{
				if (def_flg <= 1) def_flg = DEF_SAWSUE;
				bol_flag = 0;
			}
			break;

		case TOK_NAME_IDX:
		case TOK_NONAME_IDX:
			if (level == 0)
			{
				def_flg ^= DEF_SAWNAME;
				// TWO names in a row after a "struct" means that it's a variable declaration, not a definition
				if (def_flg == DEF_SAWSUE) def_flg = 0;
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
				// test for completed struct/union/enum definition
				if (def_flg == (DEF_SAWSUE|DEF_SAWNAME)) def_flg = DEF_SUETRIG;
				bol_flag = 0;
			}
			break;

		case TOK_ENDOFBUF:
			return m;

		case TOK_O_PAREN:								// detect the beginning of a function arglist (open paren preceded by a name)
			if (level == 0 && def_flg <= 1 && p[-1] == TOK_NAME_IDX) def_flg = DEF_SAWFN;

		default:
			if (level == 0)
			{
				// must kill a potential struct def on: struct name *
				// (anything else except a curly bracket or name is illegal anyway)
				if (def_flg == (DEF_SAWSUE|DEF_SAWNAME)) def_flg = 0;
				bol_flag = 0;
			}

		}		// end of switch on *p

		++p;
		if (def_flg >= MIN_TRIG)			// found a definition?
		{
			m -= k - idx_strt;
			j = p - sp;
			if (def_flg == DEF_SUETRIG) parse_sue(sp, j, idx_strt, inf);
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
	uint8_t *p, *sp, level, *decl_strt;

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



void post_proto()
{
	int32_t i;
	i = 0;
	// is there any post processing needed on defs? To turn them into something *useful* instead of just token streams?

	// copydown idx_tbl -- emit buffer should stay where it is
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
// HIHI! if num_toks will not fit in the wrksp with a little left for processing, then malloc it! (to inf->emitbuf)
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
	outf_exists = 0;							// reset the flag forever (unless I need to dump line_nums??)
	infd = outfd = -1;
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
	j = proto_pass(&inf);

	j = declarations(&inf);		// process global variable declarations
	idxidx = j;
	post_proto();				// move the buffers around in mem one more time
}



void syntax_check()
{
	// apply all the grammar rules with a state machine
	// finalize all ambiguous operators (eg: * for mult or deref)
}


