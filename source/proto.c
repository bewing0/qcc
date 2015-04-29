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


int synchk_fn_decl(uint8_t *c, int32_t len)
{
	// possibilities are: typespecs, name_idx, fnp_idx, declspecs
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
// HIHI!! the string from p (length i) to the end should be stored somewhere else as the function prototype, indexed by the name
		// start building a defidx entry -- it should point at the name (from idxidx)
		i = idx_tbl[--nxtidx];
		inf->defs_idx[inf->cur_didx++] = i;
		// then scan backwards some more through the function declaration, to verify more syntax
		p -= 2;
		i = 0;
		while (*p != TOK_TYPEDEF && *p != TOK_O_PAREN)
		{
			if (*--p >= FIRST_IDX_TOK) --nxtidx;
			++i;
		}
		c = def;
		i = p - c;		// get the distance back to beginning of def
		if (*p != TOK_O_PAREN || synchk_fn_decl(p + 1, i))
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
	set_to_noop(def, totlen);
}


// suck all the global function, struct, union, enum, and typedef prototypes out of the token stream
// -- which means creating a new "defs" token stream, and splitting out a defs_idx from the idx_tbl
uint32_t proto_pass(struct proto_info *inf)
{
	int32_t i;
	uint32_t j, k, m, idx_strt;
	uint8_t bol_flag, *p, *sp;
	int8_t level, def_flg;

	bol_flag = 1;
	def_flg = level = 0;
	i = k = m = 0;
	p = sp = wrksp_top;
	if (inf->emitbuf != NULL)
		p = sp = inf->emitbuf;
	while (1)
	{
		if (level == 0 && def_flg == 0)
		{
			idx_strt = k;			// save the *next* idx index
			sp = p;					// save a ptr to the beginning of any prototype
		}
		switch (*p)
		{
	// HIHI!! how to detect the beginning of a function definition?? It's the only thing that's not a typespec or declspec or variable name?
		case TOK_STRUCT:
		case TOK_UNION:
		case TOK_ENUM:
			if (level == 0 && def_flg == 0) def_flg = -1;
			break;

		case TOK_NAME_IDX:
		case TOK_INT_CONST:
		case TOK_FP_CONST:
		case TOK_NONAME_IDX:
			j = idx_tbl[k++];							// must keep k aligned with the token stream
			if (++m != k) idx_tbl[m - 1] = j;			// rebuild a compressed idx_tbl
			bol_flag = 0;
			break;

		case TOK_TYPEDEF:
			if (bol_flag != 0)
			{
				if (level == 0) def_flg = 2;
// HIHI can you do typedefs with only local scope? I suppose you could TRY, but it doesn't get you much.
			}
			bol_flag = 0;
			break;

		case TOK_SEMIC:
			if (level == 0 && def_flg != 0) def_flg = 32;
			bol_flag = 1;
			break;

		case TOK_OCURLY:
			if (def_flg < 0) def_flg = 1;
			bol_flag = 1;
			++level;
			break;

		case TOK_CCURLY:
			if (--level == 0)
			{
				if (def_flg == 1) def_flg = 16;
			}
			bol_flag = 1;
			break;

		case TOK_ENDOFBUF:
			return m;

		default:
			if (def_flg < 0) def_flg = 0;
			bol_flag = 0;

		}		// end of switch on *p

		++p;
		if (def_flg > 2)			// found a definition?
		{
			m -= k - idx_strt;
			j = p - sp;
			if (def_flg == 32) parse_typedef(sp, j, idx_strt, inf);
			else if (def_flg == 16) parse_sue(sp, j, idx_strt, inf);
			def_flg = 0;
		}
	}		// infinite loop
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

	idxidx = j;
	post_proto();
}



// global declarations belong in .bss, .data, or .rodata -- not in the .text section
void declarations()
{
	// HIHI global declarations also end up as symbols -- but I need to know if they are private (static) or not
	// should I use the underbar thing to make some of them hidden scope?
}

void syntax_check()
{
	// apply all the grammar rules with a state machine
	// finalize all ambiguous operators (eg: * for mult or deref)
}


