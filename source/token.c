struct tok_stor
{
	uint64_t *llp;			// array for storage of all constant integer values
	uint64_t *ldmp;			// storage for all constant floating mantissas
	uint16_t *ldep;			// exponents
	uint32_t *idx;			// indexes into all the other arrays
	uint8_t *fname;
	uint32_t line_num;
	uint32_t idxidx;		// current index (empty) in the idx table
	int infd;
// noname_cnt
//

};

// HIHI!! pass in a filename ptr and a line number for error messages
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
	{
		j = handle_tbl[-i - 1] >> TOK_STROFF_SHFT;
		return j;
	}

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
	j = namestr_len;
	handle_tbl[i] = len | (namestr_len << 8);
	namestr_len += len + 1;
	return j;
}


void tok_pass(struct tok_stor *inf)
{
	uint8_t *p, *c, hsh_val;
	int32_t i;
	uint32_t j;

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
//					j = inf->noname_cnt++;
					inf->idx[inf->idxidx] = i;
					++(inf->idxidx);
				}
			}
			else *(emit_ptr++) = *(p++);
			break;

		case ' ':
			++p;		// the input only contains whitespace to guarantee separation between names
			break;		// -- but that is no longer needed, so discard it

		case ESCCHAR_LE7F:
			if (*++p == TOK_ENDOFBUF) break;		// HIHI!! must unget 1 char and go read
			i = *p & 0x7f;		// the first 256 faked entries in the llp array have values equal to their index
			*(emit_ptr++) = TOK_INT_CONST;				// emit a "int constant token"
			inf->idx[inf->idxidx] = i;
			++(inf->idxidx);
			break;

		case ESCCHAR_GT7F:
			if (*++p == TOK_ENDOFBUF) break;		// HIHI!! must unget 1 char and go read
			// store the value verbatim as an int
			// -- the first 256 faked entries in the llp array have values equal to their index
			*(emit_ptr++) = TOK_INT_CONST;				// emit a "int constant token"
			inf->idx[inf->idxidx] = *p;
			++(inf->idxidx);
			break;

		case '0':	case '1':		// numbers
		case '2':	case '3':
		case '4':	case '5':
		case '6':	case '7':
		case '8':	case '9':
			// do the scan for a '.' to select float/int
			// also test for hex/binary/octal, of course
			c = p;
			// HIHI this is basically the same code and logic as in eval_const_expr();
/*
		** convert this to a function NOW
			h = 0;			// assume int
			// skip a 0x or 0b, then look for a radix point
			s1 = s + 1;
			if (*s == '0' && ((*s1 | 0x20) == 'x' || (*s1 | 0x20) == 'b')) ++s1;
			while (hex_lkup[*s1] >= 0) ++s1;
			// the next byte MUST be a radix point, or it's not a float
			if (*s1 == '.') h = 1;
			-- if it's a float, build it as a 64b mantissa & 16b exp in 2 ints
			-- then I will need another function to convert the mantissa & exp into a **regular double** for using in eval_const_expr
			*/
//#define TOK_FP_CONST	125
			break;

		case '"':
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
			inf->idx[inf->idxidx] = j;					// the index array stores the string offset
			++(inf->idxidx);
			*(emit_ptr++) = TOK_NAME_IDX;				// emit a "name token"
			// all strings should either end on a space or a doublequote
			if (*p == '"') ++p;
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
				if (*(p++) == '\n') ++i;			// count runs of newlines
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
			i = 0;			// HIHI!!! should never trigger!

		case TOK_ENDOFBUF:
			// if the input filedescriptor is non-negative, read more!
			if (inf->infd < 0) return;
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

		// verify no overflow of emit_ptr here? emit_ptr < p?
	}		// infinite loop
}



// tokenize -- perhaps reading from a file and perhaps writing to a file -- save the length
// load tokenized code into upper half of workspace -- delete any processed tempfile
void tokenize()
{
	int32_t j, wrk_avail;
	struct tok_stor inf;

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

	inf.llp = (uint64_t *) (name_strings + j);
	j = (nxt_pass_info[PP_NUMS_GT255] + 255) & ~255;			// each potential number is getting 18 bytes of storage
	if (j * 16 > wrk_avail / 2) j = wrk_avail / 2;
	inf.ldmp = inf.llp + j;
	inf.ldep = (uint16_t *) (inf.ldmp + j);
	wrk_avail -= j * 18;

	// Max size is the sum of # of strings and total # of numbers.
	inf.idx = (uint32_t *) (inf.ldep + j);
//	j = (nxt_pass_info[1] + nxt_pass_info[PP_NUM_CNT]) * 4;
//	wrk_avail -= j;
	// XXX: there may be one more array needed -- size j, of bytes, to hold the type of each index in the idx table?

	inf.idxidx = 0;
	tok_pass(&inf);
}


// final scan of tokenized code for syntax
void syntax_check()
{
	// apply all the grammar rules with a state machine
	// finalize all ambiguous operators (eg: * for mult or deref)
	// simplify expressions as much as possible
}


// process typedefs, structs, unions, and enums
void typespecs()
{
}
