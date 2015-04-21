// PASS #3: sweep out global prototypes from the codestream

// The basic point of this pass is that .h files add a lot of crud to the code, in the form of
// typedefs, structure definitions, enums, and function prototypes that are never referenced by
// the sourcecode afterward. Parsing all that stuff back into its own buffer reduces the code
// size by a small but still noticeable amount, which makes the remaining code faster to parse.

// This also provides an opportunity to automatically prototype every function in the sourcecode,
// which means the programmer does not need to perform that task anymore.


struct proto_info
{
	uint8_t *fname;
	uint32_t line_num;
	int infd;
	uint32_t idx_strt;
	uint32_t wb_flag;
	uint32_t cur_didx;
	uint8_t *wrkbuf;
	uint8_t *cur_ptr;
	uint8_t *defs;
	uint8_t *curdef;
	uint32_t *defs_idx;
};


// found a global typedef -- find its "name" (= NEWTYPE) and store it away properly
// 2 possible formats -- standard: typedef knowntype *** NEWTYPE;
// function pointer: typedef return_type (calling_conventions NEWTYPE)(prototype_arglist);
// XXX: **one possible issue!!** -- in the function pointer format, if the return type is char **, exactly where do the stars go, according to the standard?
// HIHI parse out both kinds of typedefs separately, with different tokens at the front?
void parse_typedef(uint32_t totlen, struct proto_info *inf)
{
	uint8_t *p, *name_ptr, *beg, *end, hsh_val, *sp;
	int32_t i, level, namelen;

	beg = inf->wrkbuf;
	end = beg + totlen;
	*end = TOK_ILLEGAL;
	// scan forward and parse any struct/union/enum defs found
	p = beg;
	i = inf->idx_strt;
	while (*p != TOK_ILLEGAL)
	{
		if (*p == TOK_STRUCT || *p == TOK_UNION || *p == TOK_ENUM)
		{
			if (p[1] == TOK_NAME_IDX || p[1] == TOK_NONAME_IDX)
			{
			}
		}
		++p;
	}

	// first, find the "newtype" name within the two possible formats -- scan backwards from end
	// if the previous character before the end was a close paren, it's a function pointer typedef
	p = end - 1;
	while (*p == TOK_NL || *p == TOK_RLL_NL) --p;
	level = 0;
	if (*p == TOK_C_PAREN)
	{
		// scan backwards through curly bracket pairs to find the matching open paren
		while (*p != TOK_TYPEDEF && (*p != TOK_O_PAREN || level != 0))
		{
			if (*p == TOK_OCURLY) ++level;
			else if (*p == TOK_CCURLY)
			{
				if (--level < 0) goto td_syntax_err;
			}
			--p;
		}
		// the previous non-whitespace character before the open paren *must* be another close paren
		while (*p == ' ') --p;
		if (*p != ')') goto td_syntax_err;
		// and the previous non-whitespace character before *that* must be "newtype"
		--p;
		while (*p == ' ') --p;
		i = 1;		// set a flag that this is a function pointer typedef
	}
	// otherwise, p *must* be pointing at the end of the alphanumeric "newtype" name
	else if (alnum_[*p] == 0) goto td_syntax_err;
	namelen = 1;
	while (alnum_[*--p] != 0) ++namelen;
	name_ptr = p + 1;

	// verify no conflicts with intrinsics or previous macro names
	if (detect_c_keyword(name_ptr, namelen) != 0) goto td_syntax_err;
	hsh_val = hash(name_ptr, (int) namelen);
	sp = name_ptr;
	if (get_name_idx(hsh_val, &sp, namelen, 0) < 0) goto td_syntax_err;

	// for function pointer typedefs, store a function pointer token + all the prototype BS as the "definition"
	if (i != 0)
	{
		// use the emit_ptr area as a workspace -- a typedef does not emit any sourcecode
		sp = emit_ptr;
		*(sp++) = TOK_FUNCT_PTR;
		i = p - beg;
		while (--i >= 0) *(sp++) = *++beg;
		*(sp++) = TOK_FUNCT_PTR;
		p += namelen;
		i = end - p;
		while (--i >= 0) *(sp++) = *++p;
		i = sp - emit_ptr;
		sp = emit_ptr;
	}
	else		// for normal typedefs, just store from p down to the 0 (p - beg - 1)
	{
		sp = beg + 1;
		i = p - beg;
	}
//	sp = build_name(name_ptr, sp, hsh_val, namelen, inf, i);
	if (sp == NULL)			// HIHI!! what does build_name return on an error?
	{
td_syntax_err:
		// HIHI!! I can have multiple error strings based on 'i'!
		show_error(0, "typedef syntax error", NULL, 1);
	}
}


int32_t parse_proto(uint8_t **p, struct pass_info *inf, int depth)
{
	return 0;
}


void wrkbuf_alloc(uint8_t *sp, struct proto_info *inf)
{
// there are 3 scenarios for a workspace buffer in this pass -- because it can theoretically be very large
// 1) the whole file is already in mem so I don't need one,
// 2) there is enough space for it above the emit pointer,
// 3) malloc it (the max size is stored in *nxt_pass_info)
	if (inf->wb_flag != 0) return;						// check the flag for whether malloc has been called
	if (inf->infd >= 0)
	{
		// calculate if (?? - emit_ptr > *nxt_pass_info)
		//-- if not, inf->cur_ptr = (uint8_t *) malloc(*nxt_pass_info); inf->wb_flag = 1;
		// else
			 inf->cur_ptr = emit_ptr;	// case 2
	}
	else inf->cur_ptr = sp;				// case 1
	inf->wrkbuf = inf->cur_ptr;		// HIHI!! I don't remember what cur_ptr is supposed to be for!??
}



// suck all the global function, struct, union, enum, and typedef prototypes out of the token stream
// -- which means creating a new "defs" token stream, and splitting out a defs_idx from the idx_tbl
uint32_t proto_pass(struct proto_info *inf)
{
	int32_t i;
	uint32_t j, k, m;
	uint8_t bol_flag, *p, *c, *sp;
	int8_t level, def_flg;

	bol_flag = def_flg = level = 0;
	i = k = m = 0;
	p = sp = wrksp_top;
	while (1)
	{
// At top level, the only thing I should ever encounter at bol are typespecs/declspecs and typedefs?
// HIHI!! I must also copydown the EMIT buffer! Use the c pointer for that! But there is a problem for the defs = -1 case that gets cancelled ...
		if (level == 0 && def_flg == 0)
		{
			inf->idx_strt = k;		// save the current idx index
			sp = p;					// save a ptr to the beginning of any prototype
		}
		switch (*p)
		{
	// HIHI!! how to detect the beginning of a function definition?? It's the only thing that's not a typespec or declspec or variable name?
		case TOK_STRUCT:
		case TOK_UNION:
		case TOK_ENUM:
			if (level == 0) def_flg = -1;
			break;

		case TOK_NAME_IDX:
			j = idx_tbl[k++];
			// HIHI!! must look ahead one byte for the curly bracket and kill a potential struct def HERE
			// if (def_flg == 0) idx_tbl[m++] = j;			// rebuild a compressed idx_tbl
			c = name_strings + j;
//			if (level == 0 && def_flg == 0)				// HIHI debugging!
//				bol_flag = 0;
			break;

		case TOK_INT_CONST:
		case TOK_FP_CONST:
		case TOK_NONAME_IDX:
			j = idx_tbl[k++];			// must keep k aligned with the token stream
			break;

		case TOK_TYPEDEF:
			if (bol_flag != 0)
			{
				if (level == 0) def_flg = 2;
		// HIHI can you do typedefs with only local scope?
			}
			// else show_error (0, "typedef syntax error", NULL, 1, inf);		// must recalculate the line number and fname first!
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
			if (inf->infd < 0) return m;
		// HIHI!! if (def_flg != 0) then I must do a partial copy to the wrkbuf! (and reset sp = p;)
			read_tok (&p);
			break;

		default:
			if (def_flg < 0) def_flg = 0;
			bol_flag = 0;

		}		// end of switch on *p

		++p;
		if (def_flg > 2)
		{
			j = p - sp;
			wrkbuf_alloc(sp, inf);
			if (inf->cur_ptr != sp) memmove (inf->cur_ptr, sp, j);
//			if (def_flg == 32) parse_typedef(j, inf);
//			else if (def_flg == 16) parse_struct(j, inf->cur_ptr, inf->idx_strt, inf);
	// HIHI!!! then send it into a parsing routine, to break the whole thing into pieces and store it in defs
	// -- but I think I want separate parsing routines for typedefs, functions, and structs
//					parse_proto(&p, inf, 0);
			def_flg = 0;
		}

		if ((uint32_t) (emit_ptr - wrksp) > wrk_rem - 30 * 1024) handle_emit_overflow();

	}		// infinite loop
}


void post_proto()
{
	int32_t i;
	i = 0;
	// is there any post processing needed on defs?
	// copydown idx_tbl and the emitted stream
	// copyup defs, defs_idx
	// then idx_tbl, and the emitted stream
}


// process funct prototypes, typedefs, structs, unions, and enums
void prototypes()
{
	uint32_t j, wrk_avail;
	struct proto_info inf;
	// the input data is either in a file, or stored between wrksp_top and wrk_rem
	inf.wb_flag = 0;
	inf.infd = -1;
	if (outf_exists != 0)		// read in the tokenized data, if it was dumped into a file
	{
		// open the file and read 30K in binary mode
		wrksp_top = wrksp + wrk_rem - idxidx * 4 - 30 * 1024 - 4;
		inf.infd = qcc_open_r("hi1", 1);
//		inf_infd = qcc_open_r(inout_fnames[??], 1);
		j = qcc_read(inf.infd, (char *) wrksp_top, 30 * 1024);
		wrksp_top[j] = TOK_ENDOFBUF;
		// since there was enough data to overflow into a file, leave half the workspace open for it
		wrk_avail = (wrk_rem - idxidx * 4 - 30 * 1024) / 2;
		outf_exists = 0;							// reset the flag for this next pass
		outfd = -1;
	}
	else wrk_avail = wrksp_top - wrksp;				// OK to overwrite the tokenizer output
	emit_base = wrksp + wrk_avail;
	emit_ptr = emit_base;

	inf.defs_idx = (uint32_t *) wrksp;
	inf.defs = wrksp + idxidx * 4;
	j = proto_pass(&inf);

	if (inf.wb_flag != 0) free (inf.wrkbuf);
	idxidx = j;
	if (outfd > 0)			// if the emit buffer overflowed into a file, finish off the file and close it
	{
		handle_emit_overflow();			// dump the tail end
		close (outfd);
		outfd = -1;
		outf_exists = 1;	// set a flag for the next pass, to let it know there is an input file
// HIHI!! flip the iof_in_toggle?
	}

	post_proto();
}



// global declarations belong in .bss, .data, or .rodata -- not in the .text section
// -- deal with them, so that everything remaining belongs in .text
void declarations()
{
}

void syntax_check()
{
	// apply all the grammar rules with a state machine
	// finalize all ambiguous operators (eg: * for mult or deref)
}


