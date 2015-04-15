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
	uint8_t *wrkbuf;
	uint8_t *cur_ptr;
	uint8_t *defs;
	uint32_t *defs_idx;
};


// found a global typedef -- find its "name" (= NEWTYPE) and store it away properly
// 2 possible formats -- standard: typedef knowntype *** NEWTYPE;
// function pointer: typedef return_type (calling_conventions NEWTYPE)(prototype_arglist);
// XXX: **one possible issue!!** -- in the function pointer format, if the return type is char **, exactly where do the stars go, according to the standard?
// HIHI parse out both kinds of typedefs separately, with different tokens?
uint8_t * parse_typedef(uint8_t *p, struct pass_info *inf)
{
	uint8_t *sp, *name_ptr, *beg, *end, hsh_val;
	int32_t namelen, i;

	beg = p++;
	*beg = 0;			// place a stopper for scanning backwards
	// first, find the "newtype" name and its length within the two possible formats
	// -- must find the semicolon at EOL, but there can be more semicolons inside curly brackets
	i = 0;			// 'i' is the "recursion depth" level
	while (*p != 0 && (*p != ';' || i != 0))
	{
		if (*p == '{') ++i;
		// HIHI!!! I also need to count \n's!
		else if (*p == '}')
		{
			if (--i < 0)
			{
				// got some impossible curly brackets -- cross fingers, scan to the next semicolon, and quit
				while (*p != 0 && *p != ';') ++p;
				end = p;
				goto td_syntax_err;
			}
		}
		++p;
	}
	end = p;
// HIHI! different errors for whether this was the actual EOF, or just an EOB
	if (*p == 0) goto td_syntax_err;
	// if the previous non-whitespace character before the semicolon was a close paren, it's a function pointer typedef
	--p;
	while (*p == ' ' || *p == ESCNL_TOK) --p;
	if (p == 0) goto td_syntax_err;
	if (*p == ')')
	{
		// scan backwards through curly bracket pairs to find the matching open paren (note: i = 0 at this point)
		while (*p != 0 && (*p != '(' || i != 0))
		{
			if (*p == '{') ++i;
			else if (*p == '}')
			{
				if (--i < 0) goto td_syntax_err;
			}
			--p;
		}
		// the previous non-whitespace character before the open paren *must* be another close paren
		while (*p == ' ' || *p == ESCNL_TOK) --p;
		if (*p != ')') goto td_syntax_err;
		// and the previous non-whitespace character before *that* must be "newtype"
		--p;
		while (*p == ' ' || *p == ESCNL_TOK) --p;
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
		show_error(0, "typedef syntax error", NULL, 1, inf);
	}
	return end;
}


int32_t parse_proto(uint8_t **p, struct pass_info *inf, int depth)
{
	int32_t i;
	int8_t level;
	uint8_t *c, endtok;		// *wrkbuf

	if (inf->infd >= 0 && depth == 0)			// HIHI!! do this in proto_pass? Should the depth0 special code ALL be in proto_pass?
	{
		i = *nxt_pass_info;		// maybe add a few bytes?
		// if (wrksp + wrk_rem - idxidx*4 - emit_ptr > i)  ???
	// if it fits in the emit_ptr space, wrkbuf = emit_ptr;	else wrkbuf = (uint8_t *)malloc (i);
	}

	c = *p;
	endtok = TOK_CCURLY;
	if (*c == TOK_TYPEDEF)
		endtok = TOK_SEMIC;
// else scan forward to the open curly and start from there? -- need to keep track of the namstr offset! (Or even FIND it, for typespecs!)
	i = 0;
	level = 0;
	// so scan the token stream (handling OCURLY, CCURLY, ENDOFBUF) until level == 0 and *c == endtok
	while (level != 0 || *c != endtok)
	{
		if (*c == TOK_OCURLY) ++level;
		else if (*c == TOK_CCURLY) --level;
//		else if (*c == TOK_ENDOFBUF) read more -- can the read function handle this properly?
// I think I also need to be processing line numbers! And keeping track of the last LINEFILE seen!
// I also need to be keeping track of the idxidx value! And detecting struct, union, and enum tokens
		// -- and num and name tokens to increment idxidx!
		++c;
		++i;
	}

	*p = c + 1;			// ?? -- because this whole thing is gonna be processed into defs no matter what?
	// wait to emit the def until all recursive processing is finished!
	// when emitting the def, skip all NOOP bytes!
	// if (depth == 0 && wrkbuf != emit_ptr) free(wrkbuf);
	return i;
}


// HIHI!! when I add the extra entries to the struct, obviously it won't be a pass_info anymore
void wrkbuf_alloc(uint8_t *sp, struct proto_info *inf)
{
// there are 3 scenarios for a second workspace buffer in this pass
// 1) the whole file is already in mem so I don't need one,
// 2) there is enough space for it above the emit pointer,
// 3) malloc it (the size is stored in *nxt_pass_info)
	if (inf->wb_flag != 0) return;
	if (inf->infd >= 0)
	{
		// calculate if (?? - emit_ptr > *nxt_pass_info)
		//-- if not, inf->cur_ptr = inf->wrkbuf = (uint8_t *) malloc(*nxt_pass_info); inf->wb_flag = 1;
		// else
			 inf->cur_ptr = emit_ptr;	// case 2
	}
	else inf->cur_ptr = sp;				// case 1
}


// suck all the global function, struct, union, enum, and typedef prototypes out of the token stream
// -- which means creating a new "defs" token stream, and splitting out a defs_idx from the idx_tbl
uint32_t proto_pass(struct proto_info *inf)
{
	int32_t i;
	uint32_t j, k, m, cur_lfile;
	uint8_t bol_flag, *p, *c, *sp;
	int8_t level, def_flg;


	// so let's say I make an inf that contains wrkbuf, a flag for malloc, a stored 'k' value, & ???
	// -- I want a new token on the front, with an index containing the offset to the name

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
				// else show_error (0, "typedef syntax error", NULL, 1, inf);		// must recalculate the line number and fname first!
			}
			break;

		case TOK_SEMIC:
			if (level == 0 && def_flg != 0) def_flg = 100;
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
				if (def_flg == 1) def_flg = 100;
			}
			bol_flag = 1;
			break;

		case TOK_RLL_NL:
			if (p[1] == TOK_ENDOFBUF) goto read_more;
			j = *++p;
			inf->line_num += j + 3;
			break;

		case TOK_NL:
			inf->line_num++;
			break;

		case TOK_ENDOFBUF:
read_more:
			if (inf->infd < 0) return m;
		// HIHI!! if (def_flg != 0) then I must do a partial copy to the wrkbuf! (and reset sp = p;)
			read_tok (&p, inf);
			break;

		default:
			if (def_flg < 0) def_flg = 0;
			bol_flag = 0;
			break;

		case TOK_LINEFILE:
			// permanently save the offset to the string info from the idx table into cur_lfile
			// (but don't lookup the filename or line number until an error happens)
			cur_lfile = idx_tbl[k++];
			bol_flag = 1;
			inf->line_num = 0;		// line_num is now an OFFSET from cur_lfile

		}		// end of switch on *p

		++p;
		if (def_flg > 2)
		{
			j = p - sp;
			wrkbuf_alloc(sp, inf);
			if (inf->cur_ptr != sp) memmove (inf->cur_ptr, sp, j);
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
	// copydown idx_tbl
	// copyup defs, defs_idx, idx_tbl, and the emitted stream
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
	emit_ptr = wrksp + wrk_avail;
	wrk_used_base = wrk_avail;

	inf.defs_idx = (uint32_t *) wrksp;
	inf.defs = wrksp + idxidx * 4;
	j = proto_pass(&inf);
// HIHI!! copyup the defs buf -- but that (and its size) should probably be another global??
	// j is the new value for idxidx -- copyup wrksp, j * 4
	post_proto();
}


// final scan of tokenized code for syntax
void syntax_check()
{
	// apply all the grammar rules with a state machine
	// finalize all ambiguous operators (eg: * for mult or deref)
	// simplify expressions as much as possible
}


void declarations()
{
}



