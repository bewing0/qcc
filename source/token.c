// HIHI!! pass in an inf for error messages
// put *unique* strings into the string table
int32_t tok_build_name(uint8_t *name, uint8_t hash, uint32_t len)
{
	int32_t i, j, k;
	uint32_t *handle_tbl;
// HIHI!! verify that max_names_per_hash * 1k + namestr_len + len + MAX_STRING_SIZE won't overflow inf->llp
// -- if it would, do a major_copyup()
//	if (max_names_per_hash * 1024 + namestr_len + len + MAX_STRBUF_SIZE > the int storage array??)
//		i = 0;
	// copy the name into the string table
	alt_strncpy((char *) name_strings + namestr_len, (char *) name, len);

	// find the insertion point in the token list -- verify whether it's a duplicate
	handle_tbl = (uint32_t *) wrksp;
	i = get_name_idx(hash, &name, len, 1);

	if (i < 0)										// got a match -- name is already defined
		return handle_tbl[-i - 1];

	k = max_names_per_hash - 1;						// create a mask for the length of each hash list
	if ((i & k) == k || handle_tbl[i] != 0)			// is the insertion point free?
	{
		// find the end of the hash column
		j = i;								// start scanning from 'i' to find the end of the column
		while (handle_tbl[j] != 0) ++j;		// j = the current END of the column
		if ((j & k) == k)					// time to expand the handle table?
		{
//			double_handle_table_size();		-- need to make this routine more generic, so it can copyup a generic list of arrays
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

	// build the name handle -- namestr_len is the *offset* to the definition
// HIHI!! should I make it "len - 1"? And I may need to have a "size multiplier" in there, too?
	handle_tbl[i] = len | (namestr_len << 8);
	namestr_len += len + 1;
	return handle_tbl[i];
}


void tok_pass(struct pass_info *inf)
{
	uint8_t *p, *c, hsh_val;
	int32_t i;
	uint32_t j;
	uint64_t ll;
	double d;		// HIHI!! removing this later

	p = wrksp_top;
	while (1)
	{
		switch (*p)
		{
		case ESCCHAR_TOK:
			// parse for anonymous struct, union, and enum tokens
			if (*++p == TOK_ENDOFBUF) break;		// HIHI!! must unget 1 char and go read
			else if (*p == TOK_STRUCT || *p == TOK_UNION || *p == TOK_ENUM)
			{
				if (p[1] == TOK_ENDOFBUF) break;		// HIHI!! must unget 1 char and go read
				*(emit_ptr++) = *(p++);
				// the current byte must be either the beginning of a name, or an open curly bracket?
				if (alnum_[*p] == 0)
				{
					*(emit_ptr++) = TOK_NONAME_IDX;
					idx_tbl[idxidx++] = noname_cnt++;
				}
			}
			else *(emit_ptr++) = *(p++);
			break;

		case ' ':
			++p;		// the input only contains whitespace to guarantee separation between names
			break;		// -- but that is no longer needed, so discard it

		case ESCCHAR_LE7F:
			if (*++p == TOK_ENDOFBUF) break;		// HIHI!! must unget 1 char and go read
			i = *(p++) & 0x7f;
			*(emit_ptr++) = TOK_INT_CONST;				// emit a "int constant token"
			idx_tbl[idxidx++] = i;
			break;

		case ESCCHAR_GT7F:
			if (*++p == TOK_ENDOFBUF) break;		// HIHI!! must unget 1 char and go read
			// store the value verbatim as an int
			// -- the first 256 faked entries in the llp array have values equal to their index
			*(emit_ptr++) = TOK_INT_CONST;				// emit a "int constant token"
			idx_tbl[idxidx++] = *(p++);
			break;

		case '0':	case '1':		// numbers
		case '2':	case '3':
		case '4':	case '5':
		case '6':	case '7':
		case '8':	case '9':
			j = flt_cnt;
			i = num_parse(p, &p, &ll, &d, &cman_tbl[j], &cexp_tbl[j]);
			if (i == 0)
			{
				*(emit_ptr++) = TOK_INT_CONST;					// emit a "int constant token"
				if ((ll & ~0x7fffffff) == 0)
					idx_tbl[idxidx] = (uint32_t) ll;		// encode the value directly into the index tbl
				else
				{
					cint_tbl[int_cnt] = ll;		// store the value in the "int" storage array
					idx_tbl[idxidx] = int_cnt++ | 0x80000000;
				}
			}
			else						// the float values are already in ldmp and ldep
				*(emit_ptr++) = TOK_FP_CONST;			// emit a "float constant token"

			++idxidx;
			break;

		case '"':
			c = p;
			j = 1;
	// HIHI!!!! Problem with string lengths -- the table is set up to store values up to 255 only! Add one more bit, and multibyte resolution?
	// or maybe 2 bits? have cpp pass the length of the LONGEST string -- and adjust to that, dynamically?
			// 2 extra bits + 16 byte resolution gets me a 16K string, which is guaranteed to be enough!
			while (*++p != '"') ++j;				// strings are stored with any escape chars
			hsh_val = hash(c, (int) j);
			j = tok_build_name(c, hsh_val, j);
			idx_tbl[idxidx++] = j;					// the index array stores the packed string offset and length
			*(emit_ptr++) = TOK_NAME_IDX;				// emit a "name token"
			// all strings should either end on a space or a doublequote
			if (*p == '"') ++p;
			break;
			// If I really want to, I can keep track of the max len HERE, and ASSUME a multiplier of 16 or 64 initially,
			// and then pack it down to the correct size at the end (and fix the index table, yuck)

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
			while (alnum_[*++p] != 0) ++j;				// strings are stored with any escape chars
			hsh_val = hash(c, (int) j);
			j = tok_build_name(c, hsh_val, j);
			idx_tbl[idxidx++] = j;					// the index array stores the packed string offset and length
			*(emit_ptr++) = TOK_NAME_IDX;				// emit a "name token"
			break;

		case '(':	case ')':	case '=':	case '<':
		case '>':	case '&':	case '|':	case '!':
		case '+':	case '-':	case '*':	case '/':
		case '%':	case '^':	case '~':	case '?':
		case '[':	case ']':
		case '{':	case '}':	case ':':	case ';':
		case ',':	case '.':
			j = (uint32_t) tokenize_op(p, emit_ptr, 0);
			p += j;
			++emit_ptr;
			break;

		case '\n':
			i = 1;
			while ((*p == '\n' || *p == ' ') && i < 255)		// HIHI!! may be able to go to 257
			{
				if (*++p == '\n') ++i;			// count runs of newlines
			}
			inf->line_num += i;
			while (i > 2)			// compress long runs of newlines (happens often)
			{
				// emit a token for compressed newlines (subtract 2 from the count?)
				*(emit_ptr++) = TOK_RLL_NL;
				j = i;
				if (i > 258)
				{
					j = 258;
					if (i < 260) j = 250;		// preferentially, do 2 run-length compressed emits
				}
				i -= j;
				*(emit_ptr++) = (uint8_t) (j - 3);
			}
			if (i == 1 || i == 2)
			{
				*(emit_ptr++) = TOK_NL;
				if (--i > 0) *(emit_ptr++) = TOK_NL;
			}
			break;

		default:
	// show_error? -- somehow a garbage char ended up in the input stream!
			i = 0;			// HIHI!!! should never trigger!

		case TOK_ENDOFBUF:
			// if the input filedescriptor is non-negative, read more!
			if (inf->infd < 0) return;
	// generic_read(inf);
			break;

		case TOK_LINEFILE:
			// must prescan the line to make sure it is complete -- the logic cannot resume it properly
			c = p + 1;
			while (*c != 0 && *c != TOK_ENDOFBUF) ++c;
//			if (*c != 0) do a copydown on the line before reading the next chunk

			// reemit this whole directive back into the output stream
			// first 8 bytes is line number as a hex char string (1bcd0408),
			// then a NUL terminated filename
			i = 8;
			j = 0;
			while (--i >= 0)
			{
				*(emit_ptr++) = *++p;
				j += hex_lkup[*p] << (i * 4);
			}
			inf->line_num = j;
			inf->fname = ++p;
			while (*p != 0) *(emit_ptr++) = *(p++);
			*(emit_ptr++) = *(p++);						// include the NUL
		}		// end of switch on *p

		if ((emit_ptr - wrksp) > wrk_rem - 30 * 1024) handle_emit_overflow();

		// guarantee there is always 16K minimum in the input buffer at all times
		if ((p - wrksp) > wrk_rem - 16 * 1024)			// && inf->infd >= 0
			i = 0;
	}		// infinite loop
}


// put all the tables of calculated info just below wrk_rem + the emit buffer
void finalize_tok(struct pass_info *inf)
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
	e = emit_ptr - (wrksp + wrk_used_base);			// total size of tokenizer output

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
	struct pass_info inf;

	// the input data is either in a file, or stored between wrksp_top and wrk_rem
	inf.infd = -1;
	if (outf_exists != 0)
	{
		// open the file and read 30K in binary mode
		wrksp_top = wrksp + wrk_rem - 30 * 1024;
		inf.infd = qcc_open_r("hi1", 1);
//		inf_infd = qcc_open_r(inout_fnames[??], 1);
		j = qcc_read(inf.infd, (char *) wrksp_top, 30 * 1024);
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
	tok_pass(&inf);

	if (outfd > 0)			// if the emit buffer overflowed into a file, finish off the file and close it
	{
		handle_emit_overflow();			// dump the tail end
		close (outfd);
		outfd = -1;
		outf_exists = 1;	// set a flag for the next pass, to let it know there is an input file
// HIHI!! flip the iof_in_toggle?
	}
	finalize_tok(&inf);

// HIHI!! may also be trying to pass on the total number of tokens in all the struct/union/etc definitions
}
