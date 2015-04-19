// PASS #2 -- tokenize

// Tokenizing converts keywords, names, and operators into single bytes.
// The basic point is that it is much easier and faster to process single bytes than it is to process text,
// arrays, or structures.
// The switch statement is especially efficient when processing streams of bytes. Some disambiguation is
// also done for characters that are used in multiple contexts, such as the '-' character in --, -=, ->,
// - (unary) and - (binary). Converting multiple bytes into single bytes also reduces the memory
// requirements, which helps in the effort to keep the entire file packed in memory at all times for speed.


// put *unique* strings into the string table
int32_t tok_build_name(uint8_t *name, uint8_t hash, uint32_t len)
{
	int32_t i, j, k;
	uint32_t *handle_tbl;
//	if (max_names_per_hash * 1024 + namestr_len + len + MAX_STRBUF_SIZE > the int storage array??)
//		do a major_copyup()

	// tentatively copy the name into the string table
	alt_strncpy(name_strings + namestr_len, name, len);

	// find the insertion point in the token list -- verify whether it's a duplicate
	handle_tbl = (uint32_t *) wrksp;
	i = get_name_idx(hash, &name, len, 1);

	if (i < 0)										// got a match -- name is already defined
		return handle_tbl[-i - 1] >> TOK_STROFF_SHFT;

	k = max_names_per_hash - 1;						// create a mask for the length of each hash list
	if ((i & k) == k || handle_tbl[i] != 0)			// is the insertion point free?
	{
		// find the end of the hash column
		j = i;								// start scanning from 'i' to find the end of the column
		while (handle_tbl[j] != 0) ++j;		// j = the current END of the column
		if ((j & k) == k)					// time to expand the handle table?
		{
//			double_handle_table_size();		-- XXX: must make this routine more generic, so it can copyup a generic list of arrays
			// recalculate i, j, and k with the new nph value
			j = hash * max_names_per_hash + (j & k);
			i = hash * max_names_per_hash + (i & k);
			k = max_names_per_hash - 1;
		}
		// copyup the end of this hash column to create a hole at the insertion point
		while (j > i)		// [j] begins pointing at the 0 entry ABOVE the column
		{
			--j;
			handle_tbl[j + 1] = handle_tbl[j];
		}
	}

	// build the new name handle -- namestr_len is the *offset* to the definition
	// (this name handle convention must remain compatible with get_name_idx)
	handle_tbl[i] = len | (namestr_len << 8);
	k = namestr_len;
	namestr_len += len + 1;
	return k;
}


//void tok_pass(struct pass_info *inf)
void tok_pass()
{
	uint8_t *p, *c, *sp;
	int8_t level, def_flg;
	int32_t i;
	uint32_t j;
	uint64_t ll;
	double d;		// HIHI!! removing this later

	nxt_pass_info[TK_MAX_DEFLEN] = 0;
	level = def_flg = 0;		// "currently defining a structure" flag
	p = wrksp_top;
	sp = p;						// eliminate a compiler warning
	while (1)
	{
		switch (*p)
		{
		case ESCCHAR_TOK:
			++p;				// don't need ESCCHARs anymore
			if (def_flg == 0)
			{
				sp = emit_ptr;										// save a ptr to starts of defs
				nxt_pass_info[TK_CUR_DEFLEN] = 0;
			}
			if (*p == TOK_TYPEDEF && level == 0) def_flg = 2;		// parse a global typedef to the semicolon
			// parse for anonymous struct, union, and enum tokens
			if (*p == TOK_STRUCT || *p == TOK_UNION || *p == TOK_ENUM)
			{
				if (def_flg == 0 && level == 0) def_flg = -1;		// potentially start looking for a global "struct" definition
				*(emit_ptr++) = *(p++);
				// the current byte must be either the beginning of a name, or an open curly bracket?
// HIHI!! oh crap -- there is also newline whitespace to deal with! Use a next_tok() function? But it may have to read more!
				if (alnum_[*p] == 0)
				{
					*(emit_ptr++) = TOK_NONAME_IDX;
					idx_tbl[idxidx++] = noname_cnt++;
				}
			}
			else *(emit_ptr++) = *(p++);		// emit the token
			break;

		case ' ':
			++p;		// the input only contains whitespace to guarantee separation between names
			break;		// -- but that is no longer needed, so discard it

		case ESCCHAR_LE7F:
			++p;
			i = *(p++) & 0x7f;
			*(emit_ptr++) = TOK_INT_CONST;			// emit a "int constant token"
			idx_tbl[idxidx++] = i;
			break;

		case ESCCHAR_GT7F:
			++p;									// store the escaped value verbatim as an int
			*(emit_ptr++) = TOK_INT_CONST;			// emit a "int constant token"
			idx_tbl[idxidx++] = *(p++);
			break;

		case '0':	case '1':		// numbers
		case '2':	case '3':
		case '4':	case '5':
		case '6':	case '7':
		case '8':	case '9':
	// HIHI!! what about an escaped \n in the middle of a number? And if that works, if there is an error, which line is it on?
			i = num_parse(p, &p, &ll, &d, &cman_tbl[flt_cnt], &cexp_tbl[flt_cnt]);
			if (i == 0)
			{
				*(emit_ptr++) = TOK_INT_CONST;				// emit a "int constant token"
				if ((ll & ~0x7fffffff) == 0)
					idx_tbl[idxidx] = (uint32_t) ll;		// encode the value directly into the index tbl
				else
				{
					cint_tbl[int_cnt] = ll;					// store the value in the "int" storage array
					idx_tbl[idxidx] = int_cnt++ | 0x80000000;
				}
			}
			else						// the float values are already in ldmp and ldep
				*(emit_ptr++) = TOK_FP_CONST;			// emit a "float constant token"
				// idx_tbl[idxidx] = flt_cnt++;		HIHI!!!

			++idxidx;
			break;

		case '"':
			c = p;
			i = 1;
			while (*++p != '"') ++i;				// strings are stored with any escape chars
			// Note: these strings can be up to 15K in length, and the indexing mechanism can only handle 254
			if (i > 255)
			{
				// store the string directly to name_strings	-- XXX: this codepath is untested!
				idx_tbl[idxidx++] = namestr_len;
				alt_strncpy ((char *) name_strings, (char *) c, i);
				namestr_len += i + 1;
			}
			else
			{
				j = hash(c, i);
				idx_tbl[idxidx++] = tok_build_name(c, (uint8_t) j, i);
			}
			*(emit_ptr++) = TOK_NAME_IDX;			// emit a "name token"
			++p;			// skip the trailing dblquotes
			break;

		case '_':				// "names" must start with an alpha char, or an '_'
		case 'A':	case 'B':	case 'C':	case 'D':
		case 'E':	case 'F':	case 'G':	case 'H':
		case 'I':	case 'J':	case 'K':	case 'L':
		case 'M':	case 'N':	case 'O':	case 'P':
		case 'Q':	case 'R':	case 'S':	case 'T':
		case 'U':	case 'V':	case 'W':	case 'X':
		case 'Y':	case 'Z':	case 'a':	case 'b':
		case 'c':	case 'd':	case 'e':	case 'f':
		case 'g':	case 'h':	case 'i':	case 'j':
		case 'k':	case 'l':	case 'm':	case 'n':
		case 'o':	case 'p':	case 'q':	case 'r':
		case 's':	case 't':	case 'u':	case 'v':
		case 'w':	case 'x':	case 'y':	case 'z':
			c = p;
			j = 1;
			while (alnum_[*++p] != 0) ++j;
			i = hash(c, (int) j);
			idx_tbl[idxidx++] = tok_build_name(c, (uint8_t) i, j);		// the index array stores the string offset
			*(emit_ptr++) = TOK_NAME_IDX;							// emit a "name token"
			break;

		case '{':
			++level;
			if (def_flg < 0) def_flg = 1;		// parse a struct/union/enum def between curly brackets
			*(emit_ptr++) = TOK_OCURLY;
			++p;
			break;

		case '}':
			if (--level == 0 && def_flg != 0)
			{
				if (--def_flg == 0) goto def_term;
			}
			*(emit_ptr++) = TOK_CCURLY;
			++p;
			break;

		case ',':
		case ';':
			if (level == 0 && def_flg > 0)		// does this terminate a definition?
			{
def_term:
				// calculate the total length of this definition
				j = nxt_pass_info[TK_CUR_DEFLEN] + (emit_ptr - sp) + 1;
				// pass the *max* length of a defintion to the prototyping pass
				if (j > nxt_pass_info[TK_MAX_DEFLEN]) nxt_pass_info[TK_MAX_DEFLEN] = j;
				def_flg = 0;
			}
			// fall into tokenizing the op
		case '(':	case ')':	case '=':	case '<':
		case '>':	case '&':	case '|':	case '!':
		case '+':	case '-':	case '*':	case '/':
		case '%':	case '^':	case '~':	case '?':
		case '[':	case ']':	case ':':	case '.':
			j = (uint32_t) tokenize_op(p, emit_ptr, 0);
			p += j;
			++emit_ptr;
			// HIHI!! must copy this line of code into the doublequote section? Anywhere else?
			if (level == 0 && def_flg < 0) def_flg = 0;		// kill a potential struct def on anything other than an OCURLY
			break;

		case TOK_ENDOFBUF:
			// the only way to get here is to hit EOF -- so return;
			*(emit_ptr++) = TOK_ENDOFBUF;
			return;

		}		// end of switch on *p

		if ((uint32_t) (emit_ptr - wrksp) > wrk_rem - 30 * 1024)
		{
			if (def_flg != 0) nxt_pass_info[TK_CUR_DEFLEN] += emit_ptr - sp;
			handle_emit_overflow();
			sp = emit_ptr;
		}

		// guarantee there is always 16K minimum in the input buffer at all times
		if (infd >= 0 && (uint32_t) (p - wrksp) > wrk_rem - 16 * 1024) read_tok (&p);
	}		// infinite loop
}


// put all the tables of calculated info just below wrk_rem + the emit buffer
void finalize_tok()
{
	uint32_t i, j, m, x, e;
	uint8_t *p;
	// first, align wrk_rem on an 8b boundary
	wrk_rem &= ~7;
	// just below wrk_rem:
	// move the string table, the int table, the mantissa table, the exponent table, the idx table, and the emit buffer
	j = (namestr_len + 7) & ~7;
	i = int_cnt * 8;				// bytes in the int table
	m = flt_cnt * 8;				// bytes in the mantissa table
	x = (flt_cnt * 2 + 3) & ~3;		// bytes in the exponent table (rounded to 4b)
	e = emit_ptr - (wrksp + wrk_used_base);			// total size of tokenizer output remaining in memory
	num_toks = e;
// HIHI!! if (num_toks == 0) then get the total size of the output file

	// calculate the final destination pointer for the index buffer first
	p = wrksp + wrk_rem - (j + i + m + x + idxidx * 4);
	// then calculate the new emit buffer address, and move the buffer
	wrksp_top = p - e;
	memmove (wrksp_top, wrksp + wrk_used_base, e);
	// then the other buffers
	e = idxidx * 4;
	memmove (p, idx_tbl, e);
	idx_tbl = (uint32_t *) p;
	p += e;
	memmove (p, cexp_tbl, flt_cnt * 2);
	cexp_tbl = (uint16_t *) p;
	p += x;
	memmove (p, cman_tbl, m);
	cman_tbl = (uint64_t *) p;
	p += m;
	memmove (p, cint_tbl, i);
	cint_tbl = (uint64_t *) p;
	p += i;
	memmove (p, name_strings, j);
	name_strings = p;

	// and move down wrk_rem, to ABOVE the index table
	wrk_rem -= j + i + m + x;
}


// tokenize -- perhaps reading from a file and perhaps writing to a file -- save the length
// load tokenized code into upper half of workspace -- delete any processed tempfile
void tokenize()
{
	int32_t j, wrk_avail;

	// the input data is either in a file, or stored between wrksp_top and wrk_rem
	infd = -1;
	if (outf_exists != 0)
	{
		// open the file and read 30K in binary mode
		wrksp_top = wrksp + wrk_rem - 30 * 1024 - 4;
		infd = qcc_open_r("hi1", 1);
//		infd = qcc_open_r(inout_fnames[??], 1);
		j = qcc_read(infd, (char *) wrksp_top, 30 * 1024);
		wrksp_top[j] = TOK_ENDOFBUF;
		// since there was enough data to overflow into a file, leave half the workspace open for it
		wrk_avail = (wrk_rem - 30 * 1024) / 2;
		outf_exists = 0;							// reset the flag for this next pass
		outfd = -1;
	}
	else wrk_avail = (wrksp_top - wrksp) - 0x10000;			// 64K below the cpp output
	emit_ptr = wrksp + wrk_avail;
	wrk_used_base = wrk_avail;

	// the tokenizing pass needs 6 temporary arrays: a namehash table, strings, 64b longs,
	// floating mantissas, floating exponents, and an "index" array to keep track of everything
	// -- the preprocessor passed on a few numbers to estimate max allocations

	// total number of alphanumeric strings can be used to calculate the size of the hashed handle table
	j = 2048;
	while (j / 2 < (int32_t) nxt_pass_info[PP_STRING_CNT] && j < wrk_avail / 8) j *= 2;
	max_names_per_hash = (uint16_t) (j / 1024);
	wrk_avail -= j;

	// init the name hashes in the list to "unused"
	memset (wrksp, 0, j);

	// the raw name strings begin just above the name hash list (growing up)
	name_strings = wrksp + j;
	namestr_len = 0;
	j = (nxt_pass_info[PP_ALNUM_SIZE] + 1023) & ~1023;			// assume 1K more than suggested
	if (j > wrk_avail / 4) j = wrk_avail / 4;
	wrk_avail -= j;

	cint_tbl = (uint64_t *) (name_strings + j);
	j = (nxt_pass_info[PP_BIG_NUMS] + 255) & ~255;			// each potential number is getting 18 bytes of storage
	if (j * 16 > wrk_avail / 2) j = wrk_avail / 2;
	cman_tbl = cint_tbl + j;
	cexp_tbl = (uint16_t *) (cman_tbl + j);
	wrk_avail -= j * 18;

	// Max size is the sum of # of strings and total # of numbers.
	idx_tbl = (uint32_t *) (cexp_tbl + j);
//	j = (nxt_pass_info[1] + nxt_pass_info[PP_NUM_CNT]) * 4;
//	wrk_avail -= j;

	idxidx = 0;
	noname_cnt = 0;
	int_cnt = 0;
	flt_cnt = 0;
	tok_pass();

	if (outfd > 0)			// if the emit buffer overflowed into a file, finish off the file and close it
	{
		handle_emit_overflow();			// dump the tail end
		close (outfd);
		outfd = -1;
		outf_exists = 1;	// set a flag for the next pass, to let it know there is an input file
// HIHI!! flip the iof_in_toggle?
	}
	finalize_tok();
}
