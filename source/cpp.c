// PASS #1 -- preprocess (with a little bit of extra functionality)

struct pp_recursion_info {
	int fd;						// source file descriptor
	int depth;					// recursion depth for nested included files
	int state;					// state machine info used only in read_compressed
	uint32_t line_num;			// current line number in file fname
	uint8_t *buf_top;			// a pointer to the terminating NUL + 1 in the input buffer
	uint8_t *u;					// storage for "ungetted" bytes in read_compressed
};


uint16_t off[256];			// just a little reusable array

int tokenize_op(uint8_t *s, uint8_t *d, int prep_flg);
int32_t calculate_expr(uint8_t *expr, uint64_t *llp, int32_t llcnt, double *ldp, int32_t ldblcnt);
uint8_t detect_c_keyword(uint8_t *name, uint32_t len);
void show_error(int level, char *str1, char *r, int how_far);
void handle_emit_overflow(uint32_t *i);
void major_copyup(uint32_t added, int depth);

// special preprocessor byte "tokens" -- but they aren't really tokens
#define ESCNL_TOK			0x18			// a standin for \ \n
#define ESCCHAR_LE7F		0x1b			// the next input byte must be considered as a literal, after the 0x80 bit is removed
#define ESCCHAR_GT7F		0x1c			// the next input byte must be considered as a literal (value >= 0x80)
#define PSEUDO_NL			0x1d			// this byte is treated as a newline by the preprocessor
#define LFILE_TOK			0x1e			// the next LINEFILE string in the buffer corresponds to this point
#define ESCCHAR_TOK			0x1f			// the next input byte has already been truly tokenized


// this function is only called if *p is pointing to a /x or /digit
int hexchar(uint8_t **p)
{
	uint8_t *c;
	int i, j;
	c = *p;
	// XXX: is there also a /b binary method for coding single chars? Can the /X be capitalized? Test!
	if (*c == 'x')					// HIHI is an x format *required* to be 2 hex digits??
	{
		if (*++c == 0) return 0;
		i = hex_lkup[*c];				// get the first hex digit
//		if (i < 0)					// HIHI!! error -- invalid char escape format
		if (*++c == 0) return 0;
		j = hex_lkup[*c];				// get second hex digit
		if (j >= 0)
		{
			i = j | (i << 4);
			++c;
		}
	}
	else
	{
//		if (*c == '8' || *c == '9')			// HIHI!! error -- invalid char escape format
		i = hex_lkup[*(c++)];				// get the first octal digit
		if (*c == 0) return 0;				// check second digit
		if (*c >= '0' && *c <= '7')
		{
			i = hex_lkup[*(c++)] | (i << 3);
			if (*c == 0) return 0;				// check third digit
			if (*c >= '0' && *c <= '7')
				i = hex_lkup[*c] | (i << 3);
		}
	}
	*p = --c;					// update p to point at the last valid char of the escape sequence
	*c = (uint8_t) i;			// and replace that char with the value
	return 1;
}

static char monthname[40] = "JanFebMarAprMayJunJulAugSepOctNovDec";

void date_time (int timeflg)
{
	time_t ti;
	struct tm *tm;

	time(&ti);
	tm = localtime(&ti);
	if (timeflg == 0)
	{
		strncpy ((char *) emit_ptr, monthname + tm->tm_mon * 3, 3);
		emit_ptr[3] = ' ';
		emit_ptr[4] = '0' + tm->tm_mday / 10;
		emit_ptr[5] = '0' + tm->tm_mday % 10;
		emit_ptr[6] = ' ';
		ntc (tm->tm_year + 1900, (char *) emit_ptr + 7);
		emit_ptr += 11;
		return;
	}
	*(emit_ptr++) = '0' + tm->tm_hour / 10;
	*(emit_ptr++) = '0' + tm->tm_hour % 10;
	*(emit_ptr++) = ':';
	*(emit_ptr++) = '0' + tm->tm_min / 10;
	*(emit_ptr++) = '0' + tm->tm_min % 10;
	*(emit_ptr++) = ':';
	*(emit_ptr++) = '0' + tm->tm_sec / 10;
	*(emit_ptr++) = '0' + tm->tm_sec % 10;
}

#define MAX_STRBUF_SIZE		(15 * 1024)			// softish limit for the number of bytes in a char constant string
#define BOL_STATE			0		// this value be *must* be 0
#define TENT_COMMENT_STATE	1
#define EOL_COMMENT_STATE	2
#define FBOX_COMMENT_STATE	3
#define CLOSEQ_STATE		4
#define CHAR_LITRL_STATE	5		// char literal and string literal states must be sequential
#define STRING_LITRL_STATE	6
#define ESCAPED_C_STATE1	7		// escaped char 1 and escaped char 2 states must be sequential
#define ESCAPED_C_STATE2	8
#define WHITESPACE_STATE	9


// read a file, compress out comments and whitespace
// cpydwn and cdsize is a partial line of unprocessed data to stick on the front of the newly read data
// the top of the final data destination is wrksp_top
void read_compressed(struct pp_recursion_info *inf, uint8_t *cpydwn, int32_t cdsize)
{
	uint8_t *p, *c, *sp, *cmp;
	int lines_processed, i, j, cdsz;

	// a little initting
	cdsz = cdsize;
	lines_processed = 0;
	cmp = emit_ptr + 16*1024;				// compressed output storage
	sp = cmp;
	p = cmp + 64 + cdsize;					// raw data read point
	// include a stopper in front of the beginning of the raw data
	p[-1] = 0;
	// copy in any raw ungetted bytes from the previous call to read_compressed
	i = inf->state >> 4;
	c = inf->u;
	while (--i >= 0) *(p++) = *(c++);

read_more:
	j = 30 * 1024 - cdsz;					// read the file in 30K gulps, starting at location p
	// standard read loop -- i is always initted to -1
	while (i != 0)
	{
		c = p;
		i = qcc_read (inf->fd, (char *) p, j);

		total_bytes += i;
		p += i;
		j -= i;
		if (i > 0)
		{
			// scan the "i" new chars for legality -- NUL, or chars with the 0x80 bit set are a problem
			// but having 0xe to 0x1f open is also useful, so make those completely illegal too
			while (--i >= 0)
			{
// HIHI!!! modify the declaration to return a boolean for fatal errors? Do a show_error, but I must calculate the line number!
				if ((int8_t) *(c++) < '\t' || (*c > '\r' && *c < ' ')) return;
			}
		}
		else i = 0;
	}
	if (j != 0)			// "i" went to 0 while there was still room in the buffer -- EOF!
	{
		*(p++) = '\n';
		qcc_close (inf->fd);
		inf->fd = -1;
	}

	*p = 0;
	c = sp;						// compressed data write point (it is important that this be sp!)
	sp = emit_ptr;				// 16K for string constant storage
	// copy in the bit of data that needs to be at the front of the buffer
	while (--cdsz >= 0) *(c++) = *(cpydwn++);
	p = c + 64;
	// 'j' is a boolean flag for whether the next incoming char is an escaped literal
	j = 0;
	i = inf->state & 0xf;

	// Parse everything that was read and compress out the window dressing:
	// This is being handled as a sort-of state machine,
	// because that makes it easy to resume reading the next input gulp.

	// There are 2 reasons for this next step:
	// 1) compressing the input (often by a factor of 3) makes it faster to re-parse,
	// 2) having the comments and whitespace suppressed, and the strays (\) preparsed,
		// simplifies the logic of the preprocessor below.
	while (*p != 0 || j != 0)
	{
		// note: all the "continue" statements in the rest of this function jump back to this point
		switch ((uint8_t) i)
		{
		case BOL_STATE:
			while (*p == '\n')					// process blank lines
			{
				*(c++) = '\n';
				++lines_processed;
				// suppress all whitespace at BOLs (not including returns or NULs)
				while (whtsp_lkup[*++p] != 0);
			}
			// then parse the interesting non-whitespace, up to some kind of stopper
			while (prep_src[*p] != 0) *(c++) = *(p++);		// emit a "sourcecode" byte
			if (whtsp_lkup[*p] != 0)						// did it end on more whitespace?
			{
				*(c++) = ' ';		// emit ONE space char, and discard any run of whitespace
				i = WHITESPACE_STATE;						// whitespace compression state
			}
			else if (*p == '/') i = TENT_COMMENT_STATE;		// potential comment state
			else if (*p == '\'') i = CHAR_LITRL_STATE;		// single literal char state
			else if (*p == '"') i = STRING_LITRL_STATE;		// string literal state
			else if (*p == '\\')
			{
				if (stoppers[*++p] != 0)		// 0 or \n
				{
					if (*p == 0) *(sp++) = '\\';		// unget the stray
					else
					{
						*(c++) = ESCNL_TOK;
						++lines_processed;
						++p;								// note: whitespace after an escaped nl must be *kept*
					}
				}
				else show_error (1, "unexpected \\ in input stream ignored", NULL, 1);	
			}
			if (i != BOL_STATE) ++p;
			break;

		case TENT_COMMENT_STATE:			// potential (tentative) comment
			i = BOL_STATE;
			if (*p == 0) i = TENT_COMMENT_STATE;			// remain in state 1, read more!
			else if (*p == '/') i = EOL_COMMENT_STATE;		// comment to EOL state
			else if (*p == '*') i = FBOX_COMMENT_STATE;		// flowerbox state
			else *(c++) = '/';				// wasn't a comment! just emit the sourcecode '/' char
			break;

		case EOL_COMMENT_STATE:				// EOL comment state
			while (stoppers[*p] == 0) ++p;	// discard to EOL or EOF -- XXX: MSVC does a discard to an *unescaped* EOL!
			if (*p != 0) i = BOL_STATE;
			break;

		case FBOX_COMMENT_STATE:			// flowerbox state
			stoppers['*'] = 1;				// temporarily add * as a char that will stop the looping
			while (i == FBOX_COMMENT_STATE)
			{
				while (stoppers[*++p] == 0);
				if (*p == '*')
				{
					if (p[1] == '/') i = BOL_STATE, p += 2;		// End of flowerbox? Or just a singlet star?
				}
				else if (*p == 0)
				{
					i = TENT_COMMENT_STATE;		// it's easiest to return to state 1 and unget a star
					*(sp++) = '*';
					if (p[-1] == '*' && p[-2] != '/') *(sp++) = '*';
				}
				else			// if (*p == '\n')
				{
					++lines_processed;
					*(c++) = '\n';
				}
			}
			stoppers['*'] = 0;			// repair the stoppers array
			break;

		case WHITESPACE_STATE:
			while (whtsp_lkup[*p] != 0) ++p;
			if (*p != 0) i = BOL_STATE;
			break;

		case CLOSEQ_STATE:				// close singlequote
			// inside quoted chars or strings, a newline escaped with a \ is considered a valid NO-OP
			i = BOL_STATE;
			if (*p == '\\' && stoppers[p[1]] != 0)
			{
				if (p[1] == '\n')		// escaped newline no-op
				{
					++lines_processed;
					*(c++) = ESCNL_TOK;
					i = CLOSEQ_STATE;		// continue waiting for the closequote
					p += 2;
				}
				else *(sp++) = *(p++);		// unget the \,  point at the 0
			}
			else if (*p != '\'') show_error(0,"missing closequote: saw ", (char *) p, 0);
			else ++p;					// step past the closequote
			break;

		case CHAR_LITRL_STATE:			// single char literal (singlequoted)
			if (*p == '\n' && j == 0)
			{
				show_error(0,"newline in char constant / missing closequote", NULL, 1);
				i = BOL_STATE;
				--p;
			}
			if (*p == '\\' && j == 0) i = ESCAPED_C_STATE1;	// go to escaped literal char state (returns to char literal state)
			else
			{
	// char constants are mapped up to the 0x80 to 0xff range, so they cannot be mistaken for text or special symbols (ie. #)
				j = 0;
				if ((int8_t) *p >= 0)
					*(c++) = ESCCHAR_LE7F;			// new escape char (for chars from 0 to 0x7f)
				else
					*(c++) = ESCCHAR_GT7F;			// new escape char (for chars from 0x80 to 0xff)
				*(c++) = *p | 0x80;			// map all escaped literal chars up to >= 0x80
				i = CLOSEQ_STATE;			// expecting a close singlequote
			}
			++p;
			break;

		case STRING_LITRL_STATE:			// string literal state
			if (j == 0)
			{
				stoppers['"'] = 1;
				stoppers['\\'] = 1;
				stoppers['#'] = 1;
				while (stoppers[*p] == 0) *(sp++) = *(p++);
				stoppers['#'] = 0;
				stoppers['"'] = 0;
				stoppers['\\'] = 0;
				// XXX: verify the length against MAX_STRBUF_SIZE
			}
			else
			{
				j = 0;						// if the code went through state 9, the incoming *p needs a new escape byte
				if ((int8_t) *p >= 0)
					*(sp++) = ESCCHAR_LE7F;			// new escape char (for chars from 0 to 0x7f)
				else
					*(sp++) = ESCCHAR_GT7F;			// new escape char (for chars from 0x80 to 0xff)
				*(sp++) = *(p++) | 0x80;	// add a 0x80 bitflag to all escaped literal bytes
				continue;					// and go back to parsing the string the normal way
			}
			if (*p == '"')		// found closequotes
			{
				// add a " to the beginning and end, and emit the entire saved string to *c
				*(c++) = '"';
				i = sp - emit_ptr;
				sp = emit_ptr;
				while (--i >= 0) *(c++) = *(sp++);
				sp = emit_ptr;						// reset the string area pointer for the next use
				*(c++) = '"';
				++p;
				i = BOL_STATE;
			}
			// the pp_bypass() function logic requires that sharp chars in strings be escaped
			else if (*p == '#')
			{
				*(sp++) = ESCCHAR_LE7F;
				*(sp++) = *(p++) | 0x80;
			}
			else if (*p == '\n')
			{
				i = BOL_STATE;
				// since this is a fatal compilation error, there is no need to emit the string -- discard it?
	// XXX: should two doublequotes be emitted to *c, though?
				sp = emit_ptr;
				show_error(0,"newline encountered inside string constant", NULL , 1);
			}
			else if (*p == '\\')
			{
				if (*++p == 0) *(sp++) = '\\';
				else i = ESCAPED_C_STATE2;			// change to escaped literal string char state
			}
			if ((sp - emit_ptr) > MAX_STRBUF_SIZE)
			{
				i = BOL_STATE;
				// since this is a fatal compilation error, there is no need to emit the string -- discard it?
	// XXX: should two doublequotes be emitted?
				sp = emit_ptr;
				show_error(0,"string constant too long to parse", NULL , 1);
			}
			break;

		case ESCAPED_C_STATE1:
		case ESCAPED_C_STATE2:						// \ escaped char or string literal states
			j = -1;
			// after processing this escaped char, return to the previous state -- 8->6, 7->5
			i -= ESCAPED_C_STATE1 - CHAR_LITRL_STATE;
			if (*p == '\n')				// if it's a newline after a \, emit it
			{
				++lines_processed;
				++p;
				*(c++) = ESCNL_TOK;
				j = 0;			// *p is NOT an escaped literal
				continue;		// the j==0 case below is for handling EOB cases -- jump around it
			}
			// replace the escape sequence (at p) with the actual value
			else if (*p == 'x' || (*p >= '0' && *p <= '9'))	j = hexchar(&p);
			else if (*p == 'n') *p = 0xa;
			else if (*p == 't') *p = 9;
			else if (*p == 'r') *p = 0xd;
			else if (*p == 'f') *p = 0xc;
			else if (*p == 'v') *p = 0xb;

			// the value in *p may be 0 -- but that's a legal char literal value!
			if (j != 0) continue;
			// hit a 0 = end of buffer -- unget a \ and all the stuff in p
			*(sp++) = '\\';
			while (*p != 0) *(sp++) = *(p++);
		}			// end of switch on i (state)

		if (*p == 0 && inf->fd >= 0)
		{
			inf->state = i;
			i = sp - emit_ptr;
			sp = emit_ptr;
			// is enough room left in the OUTPUT buffer to do another read?
			if (c - cmp < MAX_STRBUF_SIZE)
			{
				p = c + 64;
				while (--i >= 0) *(p++) = *(sp++);		// copy i chars from the unget buffer into p
				sp = c;
				goto read_more;
			}
			j = 0;		// force looping to end
		}
	}

	j = sp - emit_ptr;
	sp = emit_ptr;
	if (inf->fd < 0)		// theoretically, there could be chars in the unget buffer -- it should never happen in reality
	{
		// note: it's possible to do a trick to combine this while() with the last one in the else case -- but leave the code simple?
		while (--j >= 0) *(c++) = *(sp++);		// copy j chars from the unget buffer into c (output)
	}
	else
	{
		// if there is any "unget" info, save it for the next call to read_compressed
		p = wrksp_top - j;
		inf->u = wrksp_top = p;
		// note: a small amount of memory (the unget size) will get temporarily leaked,
		// but will be retrieved when the current recursion completes.
		inf->state = i | (j << 4);
		while (--j >= 0) *(p++) = *(sp++);
	}
	*c = 0;
	total_lines += lines_processed;

	// finally, do a copyup to the end of wrksp
	p = cmp;							// beginning of compressed output
	j = c - p + 1;						// get the compressed length (incl. terminator)
	wrksp_top -= j;
	memmove (wrksp_top, p, j);
}


// returns the number of chars copied into the destination string (or 1, if no copy is done)
uint32_t unescaped_EOL(uint8_t **s, uint8_t *dest, int do_copy, struct pp_recursion_info *inf)
{
	uint8_t *p;
	uint32_t len;
	len = 1;			// include the terminating NUL
	p = *s;
	// scan the defined string to an unescaped EOL as a "constant or macro expression" and store it
	while (*p != '\n' && *p != 0x1d && *p != 0)		// note: 0x1d is an unescapable pseudo-newline
	{
		if (*p == ESCNL_TOK)
		{
			++inf->line_num;			// skip escaped newlines
			++p;
			*(emit_ptr++) = '\n';
		}
		else if (do_copy != 0)
		{
			++len;
			*(dest++) = *(p++);
		}
		else ++p;
	}
	if (do_copy != 0) *(dest++) = 0;
	*s = p;								// the caller wants to know where the actual EOL is
	return len;
}


void doublesharp()
{
	int32_t i;
	i = 0;
	// HIHI!!! stub
}


// sadly, escaped newlines and spaces can get intermixed as whitespace
uint32_t pp_whitespace (uint8_t **p, struct pp_recursion_info *inf)
{
	uint8_t r, *c;
	c = *p;
	r = 0;
	while (1)
	{
		if (*c == ESCNL_TOK)
		{
			++inf->line_num;
			*(emit_ptr++) = '\n';
		}
		else if (*c != ' ') break;
		++r;
		++c;
	}
	*p = c;
	return r;			// length of whitespace
}


// skip over a block of code that failed a preprocessor test (eg. #if 0)
uint8_t *pp_bypass(uint8_t *p, uint16_t type, struct pp_recursion_info *inf)
{
	uint16_t cur_type, recur_depth;
	int32_t i;
	uint32_t j;
	uint8_t *c = p;				// avoid compiler "uninitted" warning
	recur_depth = 0;
	cur_type = type;			// if "type" is 1, then only stop at an #endif
	// for type 0, stop at any #else or #elif -- finding #else and #elif at top level are errors for type 1

	// discard to an unescaped EOL -- there may be # chars on the line that shouldn't be processesed
	unescaped_EOL (&p, NULL, 0, inf);
	while (1)
	{
		while (*p != '#' && *p != 0 && *p != '\n' && *p != ESCNL_TOK) ++p;
		if (*p == '\n' || *p == ESCNL_TOK)
		{
			++p;
			*(emit_ptr++) = '\n';		// even during bypassing, the newlines must still be emitted
			++inf->line_num;			// they also need their line numbers counted
			continue;
		}

		// verify there is at least one unescaped newline -- hitting a non-terminal end-of-buffer will mess up the logic
		c = p;
		while (*c != '\n' && *c != 0) ++c;			// do the verification *without* processing ESCNL tokens
		if (*c == 0)
		{
			if (inf->fd >= 0)
			{
				// note: wrksp_top is the final load point for the new data
				// (the second argument to read_compressed is the copydown pointer)
				wrksp_top = inf->buf_top;
				read_compressed(inf, p, (int32_t) (wrksp_top - p) - 1);
				p = wrksp_top;
				// whether the bypass loop stopped because p == 0, or == #, it causes no harm to check again
				continue;
			}
			else if (*p == 0) return p;
		}
	// HIHI!!! if (c - p > 15k) show_error (0, "line is too long to preprocess", NULL, 1);		?? and return?
		// note: at this point, logically, p must equal '#'
		++p;
		pp_whitespace (&p, inf);
		if (*c == 0) return p;			// this is tricky, but it's correct
		// do a simhash and parse for all the possible kinds of "#if"s & family
		j = 0;
		i = 1;
		while (*p >= 'a' && *p <= 'w' && i < TWENTY2EXP7 - 1)		// stop looping after 7 characters
		{
			j += (*p - 'a') * i;
			i *= 22;
			++p;
		}
		c = p;
		unescaped_EOL (&c, NULL, 0, inf);		// c was already at the EOL, but this call will update the line number
		if (j == SIMHASH_IF)
		{
			// descend a recursion layer -- look for the #endif for this #if
			cur_type = 1;
			++recur_depth;
		}
		else if (j == SIMHASH_ELIF)
		{
			if (recur_depth == 0)
			{
				if (type == 0) break;
				else show_error(0, "#elif after #else", NULL, 1);
			}
			// continue looking for the #endif for this #if/elif
		}
		else if (j == SIMHASH_IFDEF)
		{
			// descend a recursion layer -- look for the #endif for this #ifdef
			cur_type = 1;
			++recur_depth;
		}
		else if (j == SIMHASH_IFNDEF)
		{
			// descend a recursion layer -- look for the #endif for this #ifndef
			cur_type = 1;
			++recur_depth;
		}
		else if (j == SIMHASH_ELSE)
		{
			if (recur_depth == 0)
			{
				if (type == 0) break;
				else show_error(0, "another #else after #else", NULL, 1);
			}
			// continue looking for the #endif for this #if/else
		}
		else if (j == SIMHASH_ENDIF)
		{
			if (recur_depth == 0) break;
			else
			{
				if (--recur_depth == 0) cur_type = type;
			}
		}
		p = c;					// discard to an unescaped EOL after a preprocessor command
	}
	while (*--p != '#');		// return a ptr to the preprocessor *command* that ended the bypass
	++p;						// so back up, and then go forward again
	while (*p == ' ' || *p == ESCNL_TOK) ++p;		// but do not reprocess ESCNLs through pp_whitespace!
	return p;
}


// the source code is attempting to define a macro with arguments -- #define MAC(x,y) ...
// -- count the arguments, and escape and pretokenize each occurrence of an argument
// (because that makes it a million times easier to parse later, when it's used)
int32_t eval_fn_macro(uint8_t *rawdef, uint8_t *stor, struct pp_recursion_info *inf)
{
	int32_t i, j, k;
	uint8_t *p, *outp, *sp, *c;
	sp = rawdef;
	// find the unescaped EOL and put a 0 there as a stopper
	while (*sp != '\n' && *sp != 0) ++sp;
	if (*sp == 0)
		show_error (0, "function-like macro too long to parse", NULL, 1);
	*sp = 0;

	p = rawdef + 1;									// the first byte of rawdef is always the TOK_OPAREN
	while (*p == ' ' || *p == ESCNL_TOK) ++p;
	outp = emit_ptr;						// create a workspace pointer
	i = 0;
	// count the number of arguments up to the close paren and get their names
	while (*p != ')' && *p != 0)
	{
// XXX: if (*p == ',') return an "empty argument" error? a negative value? or just do a show_error and only copy a NUL byte?
		off[i++] = p - rawdef;				// off[] is a 256 entry reusable short array
		while (alnum_[*p] != 0) ++p;		// argument names *must* be alphanumeric
		while (*p == ' ' || *p == ESCNL_TOK) ++p;		// whitespace before comma
		if (*p == ',')
		{
			++p;
			while (*p == ' ' || *p == ESCNL_TOK) ++p;	// whitespace after comma
		}
		else if (*p != ')') show_error (0, "macro arguments syntax error (no close paren found)", NULL, 1);		// XXX: return?
	}
	if (i > 255) show_error (0,"too many arguments in macro (max: 255)", NULL, 1);		// XXX: return now?
	// reemit the 4 and the arg count
	*(outp++) = TOK_O_PAREN;
	*(outp++) = (uint8_t) i;

	// then parse and output the definition string -- find and 'tokenize' all occurrences of all arguments
	if (*p != 0) ++p;
	while (*p == ' ' || *p == ESCNL_TOK) ++p;
	off[i] = 0;

	while (*p != 0)
	{
		j = 0;
		sp = p;
		while (alnum_[*p] != 0) ++j, ++p;
		if (j != 0)
		{
			// match the args & emit
			k = i;
			while (--k >= 0)
			{
				c = rawdef + off[k];
				if (strncmp ((const char *) c,(const char *) sp, j) == 0 && alnum_[c[j]] == 0)
				{
					*(outp++) = TOK_O_PAREN;			// flag char for an argument
					*(outp++) = (uint8_t) k + 1;		// arg num
					k = j = 0;							// kill the loop and flag a match
				}
			}
			if (j > 0)			// no match -- emit the raw string
			{
				while (--j >= 0) *(outp++) = *(sp++);
			}
		}
		while (alnum_[*p] == 0 && *p != 0)
		{
			if (*p == ESCNL_TOK) *(outp++) = ' ';
			else *(outp++) = *p;
			++p;
		}
	}
	*p = '\n';

	p = emit_ptr;
	i = (int32_t) (outp - p);
	j = i;
	while (--j >= 0) *(stor++) = *(p++);
	*(stor++) = 0;				// the def string must end in a NUL
	return i + 1;				// return the number of chars copied into *stor (including the NUL)
}


// build the function-like macro argument pointer array
// p should be pointing at the open paren of a function-like macro with up to argcnt arguments
// return the length to the close paren, for updating p
// XXX: for error messages, it would be nice to have a pointer to the macro name
uint32_t parse_fn_macro_inputs(uint8_t *p, uint8_t argcnt, struct pp_recursion_info *inf)
{
	uint32_t len, i;
	uint16_t j;
	uint8_t *sp;
	memset (off, 0, 512);

	// find the unescaped EOL and limit the first loop to that single line
// HIHI!!! combine this loop with the next one -- doing both is silly!
	sp = p;
	while (*sp != '\n' && *sp != 0) ++sp;
	j = MAX_MACRO_STRING;
	if (sp - p < j) j = sp - p;

	// find the close paren -- get the length (including the paren)
	sp = p + 1;
	len = 1;
	while (*p != ')' && len <= j) ++p, ++len;

	if (*p != ')')
	{
		show_error(0, "could not find close paren for function-like macro", NULL, 1);
		return len;
	}
	i = 0;
	j = 1 + (uint16_t) pp_whitespace (&sp, inf);
	while (*sp != ')')
	{
		off[i++] = j;							// place the offsets of each arg into off[]
		while (*sp != ',' && *sp != ')')
		{
			if (*sp == ESCNL_TOK) ++inf->line_num, *(emit_ptr++)= '\n', *sp = ' ';
			++j;
			++sp;
		}
		if (*sp == ',') ++j, *(sp++) = 0;		// replace commas & close paren with NUL chars
//		j += pp_whitespace(&sp, inf);		-- need to replace the ESCNLs with either a real space, or maybe a NOOP
// HIHI!!! can you break a number in two with an escaped NL??? Should I allow sourcecode to break NAMES in half? GAH!
		while (*sp == ' ' || *sp == ESCNL_TOK)
		{
			if (*sp == ESCNL_TOK) ++inf->line_num, *(emit_ptr++)= '\n', *sp = ' ';
			++j;
			++sp;
		}
	}
	*p = 0;
	if (i > argcnt)
		show_error(0, "too many macro arguments", NULL, 1);
	else
	{
		while (i < (uint32_t) argcnt) off[i++] = j;			// finish out all argcnt entries of off
	}
	return len;				// return the length just past the final NUL = close paren
}


// this routine expands the table that lives at the bottom of memory --
// expanding it will cause a cascade effect, moving up one or more of the tables above it
void double_handle_table_size(uint8_t *hashtbl_base)
{
	int32_t i;
	uint32_t j, k;
	k = max_names_per_hash * 256 * 4 * 2;		// new size of the handle table
	major_copyup(k / 2, -1);					// move up all other tables necessary

	// move up the top 255 arrays of handles (array 0 doesn't move)
	i = 256;
	k = max_names_per_hash * 4;					// old number of bytes per array
	while (--i > 0)
	{
		j = k * i;								// old offset to handle entries for this hash
		// double their old offset, into the first half of the expanded array
		memmove (hashtbl_base + j * 2, hashtbl_base + j, k);		// note: alt_memmove might be nice
		// must zero out the top half of each expanded array
		memset (hashtbl_base + (j * 2) + k, 0, k);
	}
	memset (hashtbl_base + k, 0, k);			// the new top half of array 0 must also be cleared
	// double the nph value
	max_names_per_hash *= 2;
}


// code is requesting a known macro name to be undefined
void delete_name_hash(int32_t idx)
{
	uint32_t *nametbl, j, max, mask;
	nametbl = (uint32_t *) wrksp;
	j = -idx;						// since the name was found, idx = -(actual_index + 1)
	mask = max_names_per_hash - 1;
	max = (j - 1 + max_names_per_hash) & ~mask;
	// then copy down the hashes on top of idx
	while (j < max && nametbl[j] != 0)
	{
		nametbl[j - 1] = nametbl[j];
		++j;
	}
	nametbl[j - 1] = 0;			// and stomp on the last duplicate
}


// locating a name index in the table -- either for lookup, or for insertion
// Notes: lookups and insertions are *opposites* -- an insertion should FAIL on a match
// if insert_flag is 0 (doing a successful lookup) symname is updated to point at the definition string
int32_t get_name_idx(uint8_t hashval, uint8_t **symname, int32_t len, int insert_flag, uint32_t *nametbl)
{
	int32_t i, j, k;
	uint8_t *p, *c;
	// find the insertion point in the name list -- verify whether it's a duplicate
	j = max_names_per_hash;
	i = j * hashval;
	// within this hash value, names are sorted by length -- loop on finding the insertion point
	while (j > 0 && nametbl[i] != 0)
	{
		k = (uint8_t) nametbl[i];
		// if the length is greater, then this is definitely the insertion point
		if (k > len) return i;
		if (k == len)			// if the length is the same, compare the strings for equality
		{
			c = name_strings + (nametbl[i] >> TOK_STROFF_SHFT);
			p = *symname;
			while (k > 0 && *p == *c) --k, ++c, ++p;
			if (k == 0)
			{
				*symname = c + 1;
				return -i - 1;			// negative value for a return on a match
			}
		}
		++i;
	}
	// falling through means no match -- i should be just below a nph boundary
	return i;
}


// if a macro is being redefined, only complain if the def is *different*
int compare_macros(uint8_t *m1, uint8_t *m2, struct pp_recursion_info *inf)
{
	// note: m1 is the stored definition -- check to see if m2 points to EOL
	if (*m2 == '\n') m2 = m1cstr;		// if it was an EOL, point at a "-1" string
	while (*m1 == *m2 && *m1 != 0) ++m1, ++m2;
	if (*m1 == 0) return 1;
	return 0;
}


// attempting to #define something -- put it all in the name hash and string tables
void pp_build_name(uint8_t *mname, uint8_t *p, uint8_t hash, int32_t len, struct pp_recursion_info *inf)
{
	uint8_t *c, *s;
	int32_t i, j, k;
	uint32_t *handle_tbl;
	major_copyup (len + MAX_STRBUF_SIZE, 0);			// check whether memory needs rearranging

	// copy the name into the string table
	c = name_strings + namestr_len;
	s = mname;
	i = len;
	while (--i >= 0) *(c++) = *(s++);
	*(c++) = 0;					// need this pointer later for saving the definition string
	s = mname;

	// find the insertion point in the hash list -- verify whether it's a duplicate
	handle_tbl = (uint32_t *) wrksp;
	i = get_name_idx(hash, &s, len, 1, handle_tbl);

	if (i < 0)					// got a potentially illegal match -- name is already defined
	{
		if (compare_macros(s, p, inf) == 0)
			show_error(0, "redefinition of macro ", (char *) mname, 1);
		return;
		// warning "benign redefinition of type" -- for typedefs (except I'm not looking at typedefs here anymore)
	}
	k = max_names_per_hash - 1;						// create a mask for the length of each hash list
	if ((i & k) == k || handle_tbl[i] != 0)			// is the insertion point free?
	{
		// find the end of the hash column
		j = i;								// start scanning from 'i' to find the end of the column
		while (handle_tbl[j] != 0) ++j;		// j = the current END of the column
		if ((j & k) == k)					// time to expand the handle table?
		{
			double_handle_table_size(wrksp);
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
	handle_tbl[i] = len | (namestr_len << 8);

	// store the (possibly updated) macro definition just after the name
	s = p;
	if (*p == TOK_O_PAREN)			// parse this "function-like macro"
		namestr_len += len + 1 + eval_fn_macro(s, c, inf);

	else			// standard #define -- store it verbatim to EOL (unless it's empty)
	{
		if (*s == '\n')
		{
			*--p = '1';			// empty define = macro set to "-1"
			*--p = '-';
		}
		i = 0;
		while (*p != 0 && *p != '\n') *(c++) = *(p++), ++i;
		*c = 0;
		namestr_len += len + 2 + i;
	}
}


// detect preprocessor intrinsic types and functions
uint8_t detect_prep_keyword(uint8_t *s, int32_t j)
{
	switch (*s)
	{
	// check for typespecs, and the intrinsic keywords sizeof and defined
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
	case 'c':
		if (j == 4)
		{
			if (strncmp((const char *) s+1, keywords[TOK_CHAR_T- KEYWORDS_OFF] + 1, 3) == 0) return TOK_CHAR_T;
		}
		return 0;
	case 'd':
		if (j == 6)
		{
			if (strncmp((const char *) s+1, keywords[TOK_DBL_T- KEYWORDS_OFF] + 1, 5) == 0) return TOK_DBL_T;
		}
		else if (j == 7)
		{
			// "defined" is a temporary preprocessor keyword, but it is not stored in keywords[]
			if (strncmp((const char *) s+1, "efined", 6) == 0) return TOK_DEFINED;
		}
		return 0;
	case 'f':
		if (j == 5)
		{
			if (strncmp((const char *) s+1, keywords[TOK_FLOAT_T- KEYWORDS_OFF] + 1, 4) == 0) return TOK_FLOAT_T;
		}
		return 0;
	case 'i':
		if (j == 3)
		{
			if (strncmp((const char *) s+1, keywords[TOK_INT_T- KEYWORDS_OFF] + 1, 2) == 0) return TOK_INT_T;
		}
		return 0;
	case 'l':
		if (j == 4)
		{
			if (strncmp((const char *) s+1, keywords[TOK_LONG_T- KEYWORDS_OFF] + 1, 3) == 0) return TOK_LONG_T;
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
			// yes, I am being an evil person and extending the standard to include sizeof as a preprocessor intrinsic, THE WAY IT SHOULD BE
			if (strncmp((const char *) s+1, keywords[TOK_SIZEOF- KEYWORDS_OFF] + 1, 5) == 0) return TOK_SIZEOF;
		}
		return 0;
	case 'u':
		if (strncmp((const char *) s+1, keywords[TOK_UNSGN_T- KEYWORDS_OFF] + 1, 7) == 0 && j == 8) return TOK_UNSGN_T;
	}
	return 0;
}


// since this is the PRE-processor, no compilation has been done yet
// -- so no pointer math, symbols, structs, typedefs, or labels are valid!
int32_t eval_const_expr(uint8_t **p, struct pp_recursion_info *inf)
{
	uint8_t *c[2], *s, *d, *s1, toggle, h;
	int32_t i, j;
	uint64_t *llp;
	double *ldp;

	*c = emit_ptr + MAX_MACRO_STRING;			// make two buffers in the work area (64K is available)
	// 3 bytes are needed below *c, but emit_ptr can *move up* during unescaped_EOL, if there are ESCNLs!
	c[1] = *c + MAX_MACRO_STRING;
	// copy raw input up to an unescaped EOL into c[0]
	j = unescaped_EOL (p, *c, 1, inf);

	// macro substitution loop
	toggle = 0;				// source string index
	i = 1;
	while (i != 0)
	{
		i = 0;				// number of macros substituted in this pass
		s = c[toggle];
		toggle ^= 1;
		d = c[toggle];
		while (*s != 0)
		{
			// *s is either a number, a hashable (alphanumeric + '_') macro or intrinsic name, or an operator
			// recopy the numbers, operators, and char constants -- lookup and substitute the macros into the dest string
			while (prep_ops[*s] != 0)
			{
				*d = *(s++);
				// handle the special case of escaped char constants -- copy *both* bytes
				if (*d == ESCCHAR_LE7F || *d == ESCCHAR_GT7F || *d == '$') *++d = *(s++);
				++d;
			}
			
			if (alnum_[*s] != 0)
			{
				if (*s >= '0' && *s <= '9')
				{
					// recognize a float or int value -- 0x1.6a09e667f3d5dp-44, 0b1101, 0x42, etc.
					// no error checking -- values that are supposedly decimal could still contain A to F, octal can contain a '9', etc.
					if (*s == '0')
					{
						*(d++) = '0';
						if ((*++s | 0x20) == 'x') *(d++) = *(s++);
						else if ((*s | 0x20) == 'b') *(d++) = *(s++);
					}
					while (hex_lkup[*s] >= 0 || *s == '.' || (*s | 0x20) == 'p' || *s == '+' || *s == '-') *(d++) = *(s++);
				}
				else
				{
					s1 = s;
					j = 0;
					while (alnum_[*s1] != 0) ++s1, ++j;
					h = detect_prep_keyword(s, j);			// look for casts, and 2 keywords
					if (h != 0)
					{
						if (h == TOK_DEFINED)				// evaluate a defined(MACRO) keyword into a boolean
						{
							j = h = 0;
							s = s1;
							while (*s == ' ') ++s;
							if (*s == '(')
							{
								h = 1;						// syntax flag that a paren was seen
								while (*++s == ' ');
							}
							s1 = s;
							while (alnum_[*s1] != 0) ++s1, ++j;
							while (*s1 == ' ') ++s1;
							if (h != 0)
							{
								if (*s1 != ')')
								{
									show_error(0, "preprocessor 'defined()' keyword requires close parentheses", NULL, 1);
									return 0;
								}
								++s1;
							}
							h = hash (s, (int) j);
							j = get_name_idx (h, &s, j, 0, (uint32_t *) wrksp);
							if (j < 0)			// was the macro found?
							{
								*(d++) = '-';	// defined() is true
								*(d++) = '1';
							}
							else *(d++) = '0';	// or false
						}
						else
						{
							*(d++) = '$';		// escape char -- the next byte is an intrinsic function token
							*(d++) = h;
						}
					}
					else
					{
						h = hash (s, (int) j);
						j = get_name_idx (h, &s, j, 0, (uint32_t *) wrksp);
						if (j < 0)			// was the macro found?
						{
							while (*s != 0) *(d++) = *(s++);
						}
						else					// otherwise, replace this undefined macro with a no-op (this is important!)
						{
							*(d++) = '$';		// escape char -- the next byte is an intrinsic function token
							*(d++) = TOK_NO_OP;
						}
						++i;						// increment the macro substitution count
// HIHI!!! actually, I want to just copy everything out to EOL and restart from BOL!! It should simplify one loop layer and reduce "number scanning"
					}
					s = s1;						// point back at the input string
				}
			}
			if (*s == ESCNL_TOK)
				*(d++) = ' ';
		}
		*d = 0;
	}

	// all macros have now been substituted away -- only numbers and operators should remain
	// so "tokenize" what is there
	s = c[toggle];
	// HIHI!! could put d at emit_ptr to regain some memory!
	d = --*c;				// the tokenized string is always stored in *c - 1
	// note: the -1 is there to give one byte of room to copy down, in case toggle == 0
	*c -= 2;				// need 2 more stopper bytes on the front
	d[-2] = 0;				// put an illegal token on the front, and parens around the expression
	d[-1] = c_ops['('];
	// get an aligned pointer just above c[1] (inside wrksp) for an array of 64b ints
	llp = (uint64_t *)wrksp + (MAX_MACRO_STRING + c[1] - wrksp)/8;
	ldp = (double *)llp + (200 * 8) / sizeof(double);
	// the first entries in both tables are required to be a constant 0
	*llp = 0;
//	*ldp = 0.0;		HIHI -- should this be [255]?

	// The point of all this is that the tokens up to NUM_PP_TOKENS are all used for operators, and all the
	// values after that (up to 255) are used as indexes into either the "int" or "double" value arrays.
	// there are about 60 operator tokens, which puts a strict limit on the length of an expression (255 - 60)
	i = NUM_PP_TOKENS + 2;				// first token that is available to use as an index in the preprocessor
	j = 255;
	while (*s != 0)
	{
		if (*s >= '0' && *s <= '9')		// numbers always must start with a digit (minus signs are treated as operators)
		{
			h = num_parse(s, &s, &llp[i - NUM_PP_TOKENS - 1], &ldp[j - NUM_PP_TOKENS - 1], NULL, NULL);
			if (h == 0)
				*(d++) = (uint8_t) i++;
			else
				*(d++) = (uint8_t) j--;
		}
		else if (c_ops[*s] != 0)
		{
			s += tokenize_op(s, d, 1);
	// HIH!!! check *d for validity -- there are some from 20 to 27 that are illegal (and : is only legal after a ?)
			++d;
		}
		else
		{
			if (*s == '$')						// was this a keyword that was pretokenized above?
				*(d++) = *++s, ++s;				// copy the next literal byte (and move beyond it)
			else if (*s == ESCCHAR_LE7F)		// char literal (type 1) -- eliminate the 0x80 bit
			{
				llp[++i - NUM_PP_TOKENS - 1] = (uint64_t) (*++s & 0x7f);
				*(d++) = (uint8_t) i;
				++s;						// convert the next literal byte into a variable (and move beyond it)
			}
			else if (*s == ESCCHAR_GT7F)		// char literal (type 2)
			{
				llp[++i - NUM_PP_TOKENS - 1] = (uint64_t) *++s;
				*(d++) = (uint8_t) i;
				++s;						// convert the next literal byte into a variable (and move beyond it)
			}
//			else
				// illegal character in constant expression (*s)
		}
	}
	*(d++) = c_ops[')'];			// put a tokenized ) on the end of the expression
	*d = 0;
	i -= NUM_PP_TOKENS;

	i = calculate_expr(*c + 1, llp, i, ldp, 256 - j);
	return i;
}


int32_t name_lookup(uint8_t *p, struct pp_recursion_info *inf)
{
	uint8_t *c;
	int32_t i;
	c = p;
	i = 0;						// create a token out of the defined name
	while (alnum_[*p] != 0) ++p, ++i;
	// look it up in the token list -- a match returns a NEGATIVE index!
	return get_name_idx(hash (c, i), &c, i, 0, (uint32_t *) wrksp);
}


void cpp_parse(struct pp_recursion_info *pinfo, uint8_t *lfile_strs, uint32_t *lstrs_len, uint8_t *fname)
{
	uint32_t j, k;
	int32_t i;
	uint8_t *p, *c, *sp, hsh_val, bypass_flg;
	struct pp_recursion_info ninfo;

	cur_fname = fname;			// only used by show_error
	// Each layer of nested include files requires a 60K allocation for line number info,
	// plus a 30K read buffer -- so there has to be a depth limit related to the minimum
	// allocation size of the workspace. It is a user-modifiable value in config.h.
	if (pinfo->depth > MAX_NESTED_INCLUDES)
	{
		if (pinfo->fd > 0) qcc_close (pinfo->fd);
		pinfo->fd= -1;
		return;
	}
	// init one item in the ninfo struct from the parent -- ninfo is used when including nested files
	ninfo.depth = pinfo->depth + 1;
	p = wrksp_top;

	// loop over the source file until it is fully processed
	while (1)
	{
		bypass_flg = 0;
		while (*p == ' ') ++p;

		switch (*p)
		{

		// process a preprocessor directive
		case '#':
			if (*++p == '#') doublesharp();
			else
			{
				// whitespace is legal after the #
				pp_whitespace(&p, pinfo);
				sp = p;						// save a pointer
	// parse occurrences of #if, #ifdef, #ifndef, #endif, #elif, #else, #pragma, #include, #define, #warning, #error, #line, #undef
	// -- by using a simple perfect hash, and the fact that the highest letter in that list is 'w'
bypass_done:
				j = 0;
				i = 1;
				while (*p >= 'a' && *p <= 'w' && i < TWENTY2EXP7 - 1)		// stop hashing after 7 characters
				{
					j += (*p - 'a') * i;
					i *= 22;
					++p;
				}

				// the #include_next command is too long to hash (it overflows) -- handle it as a special case
				if (*p == '_' && j == SIMHASH_INCLUDE)
				{
					sp = p;
					unescaped_EOL (&sp, NULL, 0, pinfo);
					// finish detecting the "next" part
					// if (strncmp(p, "next", 4) != 0)
					goto unrecognized;
					// p += 4, verify whitespace, and do the rest of it -- XXX
				}
				else if (*p != ' ' && *p != '\n' && *p != ESCNL_TOK && *p != PSEUDO_NL)  goto unrec2;
// Note: the SIMHASH values are too big for a switch statement to handle efficiently -- it would just produce this same elseif block
				else if (j == SIMHASH_INCLUDE)
				{
					pp_whitespace (&p, pinfo);
					c = p;
					unescaped_EOL (&c, NULL, 0, pinfo);			// process all the escaped newlines to EOL
					if (*p != '"' && *p != '<') goto unrecognized;
					c = p++;						// note: a 0 EOB byte should be impossible
					if (*(c++) == '"')				// normal include file?
					{
						while (*p != '\"' && *p != '\n') ++p;
						if (*p != '"') goto unrecognized;
						sp = da_buffers[INCLUDE_PATHS];
					}
					else
					{
						while (*p != '>' && *p != '\n') ++p;			// system include?
						if (*p != '>') goto unrecognized;
						sp = (uint8_t *) SYS_INCLUDE_PATHS;
					}
	
					j = p - c;								// length of filename
	// HIHI!!! stick a fake token in the output stream to match against lfile_strs
					alt_strncpy (lfile_strs + *lstrs_len, (uint8_t *) "00000001", 8);
					*lstrs_len += 8;
					ninfo.u = lfile_strs + *lstrs_len;				// cheat and temporarily save the filename pointer here
					// note: this filename may also be displayed in error messages, and it is used to open the file
					ninfo.fd = i = -1;
					while (ninfo.fd < 0 && i != 0)
					{
						// sp points to a directory to search for the specified include file
						k = alt_strcpy(ninfo.u, sp);
						sp += k + 1;
						alt_strncpy(ninfo.u + k, c, j);
						ninfo.fd = qcc_open_r((char *) ninfo.u, 0);
						if (*sp == 0) i = 0;
					}
					sp = ninfo.u;		// filename ptr (with full path)
					// failed after looking in ALL the supplied directories?
					if (ninfo.fd < 0)
					{
						*p = 0;			// p is pointing at the closequote on the fname
						show_error(0, "File not found, trying to include file ", (char *) c, 1);
						*p = '"';
						goto discard;
					}
					*lstrs_len += strlen ((char *) sp) + 1;			// include the NUL on the filename
					*(emit_ptr++) = LFILE_TOK;
	// make a pretty display on the screen with an informational message about each included file?
	//					strncpy ((char *) ???, "          ", ninfo.depth);		// make a pretty indention to show nested includes
		// HIHI if being verbose? -- write (1, emit_ptr, strlen(emit_ptr)); -- and then a \n?
					wrksp_top = p + 1;								// the new file gets read below wrksp_top
					// read in the included file -- suppress comments and whitespace
					ninfo.buf_top = wrksp_top;
					ninfo.state = 0;
					ninfo.line_num = 1;

					read_compressed (&ninfo, NULL, 0);
					cpp_parse(&ninfo, lfile_strs, lstrs_len, sp);				// recurse
					// note: the final NUL byte of the recursed file was written on top of p
					++p;

					// output a "line directive" pointing back to the parent file
					c = lfile_strs + *lstrs_len;
					i = 8;
					k = pinfo->line_num;
					while (--i >= 0) *(c++)= hexout[(k >> (i * 4)) & 0xf];
					*lstrs_len += 8 + alt_strcpy (c, fname) + 1;
					*(emit_ptr++) = LFILE_TOK;
					cur_fname = fname;			// only used by show_error
				}
				else if (j == SIMHASH_DEFINE)
				{
					pp_whitespace (&p, pinfo);
					c = p;
					unescaped_EOL (&c, NULL, 0, pinfo);		// process escaped newlines to EOL
					c = p;
					i = 0;
					while (alnum_[*p] != 0) ++p, ++i;
					j = detect_c_keyword(c, i);				// verify that the symname does not match any keyword
					if (j == 0)
					{
						hsh_val = hash (c, i);
						// if the name ends on an open paren WITH NO WHITESPACE, it's a "function-like" macro
						if (*p == '(') *p = TOK_O_PAREN;		// flag it!
						// otherwise, verify that the defined name ended in whitespace
						else if (*p != ' ' && *p != '\n' && *p != ESCNL_TOK) goto unrecognized;
						while (*p == ' ' || *p == ESCNL_TOK) ++p;
						pp_build_name(c, p, hsh_val, i, pinfo);
					}
					else
						show_error(0, "attempt to redefine a reserved keyword: ", (char *) c, 1);
				}
				else if (j == SIMHASH_IF)
				{
					// evaluate -- if the result is 0 (false), bypass to the next matched elif, else, or endif
					i = eval_const_expr(&p, pinfo);
					if (i == 0)
					{
						bypass_flg = 1;
						p = pp_bypass(p, 0, pinfo);
						goto bypass_done;
					}
				}
				else if (j == SIMHASH_IFDEF)
				{
					pp_whitespace (&p, pinfo);
					// get the token name and try to find it
					i = name_lookup(p, pinfo);
					// if it's not there, bypass to the next matched elif, else, or endif
					if (i >= 0)
					{
						bypass_flg = 1;
						p = pp_bypass(p, 0, pinfo);
						goto bypass_done;
					}
					unescaped_EOL (&p, NULL, 0, pinfo);
				}
				else if (j == SIMHASH_IFNDEF)
				{
					pp_whitespace (&p, pinfo);
					// get the token name and try to find it
					i = name_lookup(p, pinfo);
					// if it's there, bypass to the next matched elif, else, or endif
					if (i < 0)
					{
						bypass_flg = 1;
						p = pp_bypass(p, 0, pinfo);
						goto bypass_done;
					}
					unescaped_EOL (&p, NULL, 0, pinfo);
				}
				else if (j == SIMHASH_ELIF)
				{
					// if we did not reach here through a bypass, then bypass
					if (bypass_flg == 0)
					{
						while (*p == ' ') ++p;
						i = eval_const_expr(&p, pinfo);
						if (i == 0)
						{
							bypass_flg = 1;
							p = pp_bypass(p, 0, pinfo);
							goto bypass_done;
						}
					}
					unescaped_EOL (&p, NULL, 0, pinfo);
				}
				else if (j == SIMHASH_ELSE)
				{
					// if we did not reach here through a bypass, then bypass to the endif
					if (bypass_flg == 0)
					{
						bypass_flg = 1;
						p = pp_bypass(p, 1, pinfo);
						goto bypass_done;
					}
					unescaped_EOL (&p, NULL, 0, pinfo);
				}
				// #endif is really just a placeholder no op -- do nothing
				else if (j == SIMHASH_ENDIF)
					unescaped_EOL (&p, NULL, 0, pinfo);
				else if (j == SIMHASH_UNDEF)
				{
					pp_whitespace (&p, pinfo);
					i = name_lookup(p, pinfo);
					if (i < 0) delete_name_hash(i);
					unescaped_EOL (&p, NULL, 0, pinfo);
				}
				else if (j == SIMHASH_WARNING)
				{
					c = p;												// print to eol as a compiler warning
					unescaped_EOL (&p, NULL, 0, pinfo);
					write (2, (char *) c, p - c);
					write (2, "\n", 1);
					++total_warns;
				}
				else if (j == SIMHASH_ERROR)
				{
					c = p;												// print to eol as a compiler error
					unescaped_EOL (&p, NULL, 0, pinfo);
					write (2, (char *) c, p - c);
					write (2, "\n", 1);
					++total_errs;
				}
				else if (j == SIMHASH_LINE)
				{
					unescaped_EOL (&p, NULL, 0, pinfo);
					i = 0;		// HIHI!! this directive provides a line number (and an optional filename) (but what is the exact format?)
					// first 8 bytes is line number as a hex char string (1bcd0408), then a NUL terminated filename
					// -- which must replace pinfo->fname, and pinfo->line_num
				}
				else if (j == SIMHASH_PRAGMA)
				{
					unescaped_EOL (&p, NULL, 0, pinfo);
					i = 0;		// the only one is "pack"? Or are "push" and "pop" also valid?  HIHI!!
					// pack overrides any automatic alignment in structs
				}
				else
				{
unrec2:
					unescaped_EOL (&p, NULL, 0, pinfo);		// must handle any escaped newlines to EOL
unrecognized:					// unrecognized preprocessor command
					show_error (0, "Preprocessor command can not be parsed #", (char *) sp, -1);
				}
discard:
				// the 0x1d character is treated like a special newline for preprocessor commands only
				while (*p != '\n' && *p != PSEUDO_NL && *p != 0) ++p;		// preprocessor commands always discard to an unescaped EOL
				if (*p == PSEUDO_NL) ++p;

			}		// end of doublesharp if-else
			break;										// end of preprocessor commands section (#)

		case ESCCHAR_LE7F:			// "escape" byte flags for literal char constants -- emit two raw bytes
		case ESCCHAR_GT7F:
			// note: literal bytes are guaranteed to have values less than 256, so they are never counted in nxt_pass_info[PP_BIG_NUMS]
			++nxt_pass_info[PP_NUM_CNT];			// but are always counted as numeric values
			*(emit_ptr++) = *(p++);
			// cheat and fall through to copy the second byte

		case '(':	case ')':	case '=':	case '<':
		case '>':	case '&':	case '|':	case '!':
		case '+':	case '-':	case '*':	case '/':
		case '%':	case '^':	case '~':	case '?':
		case '[':	case ']':	case ';':
		case '{':	case '}':	case ':':
		case ',':	case '.':
			if (emit_ptr[-1] == '\n' || emit_ptr[-2] == '\n')
				++nxt_pass_info[PP_LINE_CNT];			// try to estimate the number of compressed lines
			*(emit_ptr++) = *(p++);
			break;

		case '"':						// all escaped newlines have already been removed from inside string constants
			if (emit_ptr[-1] == '\n') ++nxt_pass_info[PP_LINE_CNT];
			sp = p;
			*(emit_ptr++) = *(p++);
			while (*p != '"' && *p != '\n' && *p != 0) *(emit_ptr++) = *(p++);
			if (*p == '\n') show_error(0,"newline in string constant", NULL, 1);
			else if (*p == 0) show_error(0,"unmatched doublequotes at EOF", NULL, 1);
			else *(emit_ptr++) = *(p++);
			nxt_pass_info[PP_ALNUM_SIZE] += p - sp;
			break;

		case '0':	case '1':			// emit pure numbers raw
		case '2':	case '3':
		case '4':	case '5':
		case '6':	case '7':
		case '8':	case '9':
			if (emit_ptr[-1] == '\n') ++nxt_pass_info[PP_LINE_CNT];			// try to count the number of compressed lines
			++nxt_pass_info[PP_NUM_CNT];			// total count of all numeric values
			// overestimate a total count of numeric constants with values bigger than 255 (just do a quickie test)
			// -- if there are more than 2 digits in the number or it's a float, then "it's bigger than 255"
			c = p + 1;
			if (*p == '0' && (p[1] | 0x20) == 'x') c += 2;			// HIHI!!!!! add in 0b!
			if ((hex_lkup[*c] >= 0 || *c == '.') && (hex_lkup[c[1]] >= 0 || c[1] == '.'))
				++nxt_pass_info[PP_BIG_NUMS];
			while (alnum_[*p] != 0) *(emit_ptr++) = *(p++);			// emit the raw text into the output buffer
			// note: if there is alpha crud on the end of the number, it will give a syntax error later
			break;

		case '_':		// parse occurrences of ___LINE__, ___FILE__, ___DATE__, ___TIME__, ___FUNCTION__, ___VA_ARGS__
			if (p[1] == '_' && p[2] == '_')
			{
				if (emit_ptr[-1] == '\n') ++nxt_pass_info[PP_LINE_CNT];			// try to count the number of compressed lines
				sp = p;
				p += 3;
				j = 0;
				i = 1;
				while (*p >= 'A' && *p <= 'V' && i < TWENTY2EXP7 - 1)		// stop looping after 7 characters
				{
					j += (*p - 'A') * i;
					i *= 22;
					++p;
					if (j == SIMHASH_VA && *p == '_') ++p;
				}
				if (j == SIMHASH_FUNCTIO && *p == 'N') ++p;
				if (*p != '_' || p[1] != '_') goto bad_3_;
				p += 2;
				if (j == SIMHASH_LINE)				// emit the line number as a char string
				{
					i = ntc (pinfo->line_num, (char *) emit_ptr);
					emit_ptr += i;
					break;
				}
				else if (j == SIMHASH_FILE)			// emit the full pathname as a quoted string
				{
					i = strlen((char *) fname);
					*(emit_ptr++) = '"';
					c = fname;
					while (--i >= 0) *(emit_ptr++) = *(c++);
					*(emit_ptr++) = '"';
					break;
				}
				else if (j == SIMHASH_DATE)			// emit the date "Mmm dd yyyy" as a quoted string
				{
					//  -- Mmm being Feb, for example
					*(emit_ptr++) = '"';
					date_time (0);
					*(emit_ptr++) = '"';
					break;
				}
				else if (j == SIMHASH_TIME)			// emit 24 hr time "hh:mm:ss" as a quoted string
				{
					*(emit_ptr++) = '"';
					date_time (1);
					*(emit_ptr++) = '"';
					break;
				}
				else if (j == SIMHASH_FUNCTIO)		// XXX: if this value is not a macro, then exactly how does it get set?
				{
					// note: this is gcc-specific, so supporting it is completely optional
					i = 0;
				}
				else if (j == SIMHASH_VAARGS)		// XXX: haven't looked into this one at all
				{
					i = 0;
				}
				else
				{
bad_3_:
					show_error(0, "unknown preprocessor macro: ", (char *) sp, 1);
					// HIHI -- should there be some sort of discarding here? To *where*??
					break;
				}
				// note: do not discard after handling these macros -- they are in the middles of strings!
				if (p - emit_ptr < 0x10000) handle_emit_overflow(NULL);
			}
			// if it wasn't a preprocessor directive, fall through into the alphanumeric case

		// "names" must start with an alpha char, or an '_'
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
			if (emit_ptr[-1] == '\n') ++nxt_pass_info[PP_LINE_CNT];			// try to count the number of compressed lines
			c = p;			// keyword, macro, variable, constant, function name, etc.
			sp = emit_ptr;
			j = 0;
			while (alnum_[*p] != 0) *(emit_ptr++) = *(p++), ++j;			// tentatively emit the raw text into the output buffer
			i = detect_c_keyword(c, j);
			if (i == 0)
			{
				hsh_val = hash(c, (int) j);
				i = get_name_idx(hsh_val, &c, j, 0, (uint32_t *) wrksp);
				if (i < 0)							// is it a known macro?
				{
					emit_ptr = sp;					// remove the raw text from the output buffer
					// c points at the definition string now
					if (*c == TOK_O_PAREN)			// test for a "function-like" macro
					{
						// the next byte in "c" is an argument count
						j = parse_fn_macro_inputs(p, *++c, pinfo);		// creates an array of info in off[]
						while (*++c != 0)
						{
							if (*c == TOK_O_PAREN)
							{
								i = *++c;			// get the argument number (+1)
								sp += alt_strcpy (sp, p + off[i - 1]);
							}
							else *(sp++) = *c;
						}
						*sp = 0;
						c = emit_ptr;
						p += j;			// point p at the end of the macro (just past the close paren)
					}
					j = strlen((char *) c);
					p -= j;
					strncpy((char *) p, (char *) c, j);		// copy the macro output back into the input, and parse it some more
				}
				else
				{
					*(emit_ptr++) = ' ';								// keep a whitespace between alphanumeric "words" for now
					nxt_pass_info[PP_ALNUM_SIZE] += emit_ptr - sp;		// accumulate a total length
					++nxt_pass_info[PP_STRING_CNT];						// accumulate an overestimate count of unique alphanumeric strings
				}
			}
			else		// tokenize all recognized keywords
			{
				emit_ptr = sp;					// remove the raw text from the output buffer
				*(emit_ptr++) = ESCCHAR_TOK;
				*(emit_ptr++) = (uint8_t) i;
			}
			break;

		case '\n':
		case ESCNL_TOK:
			while (*p == '\n' || *p == ESCNL_TOK)
			{
				*(emit_ptr++) = '\n';
				++pinfo->line_num;
				++p;
			}
			break;

		case 0:
			return;

		default:
			show_error(0, "Unrecognized symbol ", (char *) p, 0);
			i = *(p++);		// unrecognized symbol! (`@$)

		}		// end of switch on *p

		// verify that emit_ptr isn't close to overflowing (soft 64k limit)
		if (p - emit_ptr < 0x10000) handle_emit_overflow(NULL);

		// make sure there is always at least 16K in the input buffer, unless the file is closed
		// -- that way, running into the EOB is always just an error
		if (pinfo->fd >= 0 && pinfo->buf_top - p < 0x4000)
		{
			wrksp_top = pinfo->buf_top;
			read_compressed(pinfo, p, (int32_t)(wrksp_top - p - 1));
			p = wrksp_top;
		}
		// verify that at least one complete line is still loaded -- to prevent infinite loops
		c = p;
		while (*c != '\n' && *c != PSEUDO_NL && *c != 0) ++c;
		if (*c == 0 && *p != 0) show_error(0, "line is too long to process", NULL, 1);
	}
}


// fd is an open sourcefile that needs preprocessing for: includes, defines, macros, and #ifs
int preprocess(int fd, uint8_t *fname)
{
	int32_t i;
	uint32_t lstrs_len;
	uint8_t *lfile_strs;
	struct pp_recursion_info inf;

	// prepare to read the first 30K gulp of the main source file
	inf.depth = 0;
	inf.state = 0;
	inf.fd = fd;
	inf.buf_top = wrksp_top;
	inf.line_num = 1;

	// In the preprocessor, "names" consist of #defined macros.
	// The preprocessor does all the necessary substitutions on all the names,
	// througout the main source file and all included files. The final preprocessed
	// output is (of course) passed on to the next compilation stage, and all the
	// name info from the preprocessor is deleted to save memory.

	// wrksp memory allocation in the preprocessor:
	// at the top of memory down to wrk_rem is long-term storage (for the da_bufs)
	// just below that is 300K of whitespace-compressed output from read_compressed (with a soft size limit)
	// -- that 300K assumes a depth of 10 included files, and is never reached in reality
	// -- the bottom of this data area is stored in wrksp_top, once it is filled
	// at the bottom of memory is a 32K namehash table
	// just above that is a variable sized space for storing macro names and definitions (with a 2M limit)
	// just above that is a 10K space for storing filename and line number strings
	// just above that is the emit storage buffer for the preprocessor -- the main output buffer

	// SO: create the pointers, and calculate the offsets for setting up all those workspace buffers
	// the name table is initially allocated containing 32 empty name handles per hash value
	// -- if any hash value gets filled up, the size is doubled and memory gets rearranged
	max_names_per_hash = 32;
	i = max_names_per_hash * 4 * 256;		// this is 32K, which seems to be a good size

	// init the name hashes in the list to "unused"
	memset (wrksp, 0, i);

	// the macro name strings begin just above the name hash list (growing up)
	name_strings = wrksp + i;
	namestr_len = 0;

	// calculate the position of the next buffer (above name_strings) -- the LINEFILE strings
	i = ((wrk_rem / 2) + 3) & ~3;
	// initially limit the name strings to a 2M max
	if (i > 2 * 1024 * 1024) i = 2 * 1024 * 1024;
	lfile_strs = wrksp + i;									// set the pointer
	i += 10240;												// allocate 10K for the LINEFILE strings
	emit_base = wrksp + i;									// and set the base for the emit buffer
	emit_ptr = emit_base;
	read_compressed(&inf, NULL, 0);

	// then enter all these base pointers and lengths into the copyup control arrays
	*base_ptrs = &name_strings;
	*cur_usage = &namestr_len;
	*tshft = 0;
	base_ptrs[1] = &lfile_strs;
	cur_usage[1] = &lstrs_len;
	tshft[1] = 0;
	base_ptrs[2] = &emit_base;
	cur_usage[2] = &emit_ptr;
	tshft[2] = 0x90;				// flag that emit_ptr is a ptr (not a length) and to use special overflow handler #1
	base_ptrs[3] = &wrksp_top;
	cur_usage[3] = NULL;

	// copy in the global predefines now, for processing -- as text, prepended to the source file
	i = da_tot_entrylen[PREDEFINES];
	memmove (wrksp_top - i, da_buffers[PREDEFINES], i);
	memset (nxt_pass_info, 0, 20);
	outfd = -1;

	alt_strncpy (lfile_strs, (uint8_t *) "00000001", 8);						// must pre-emit the first LINEFILE for the input file
	lstrs_len = 8 + alt_strcpy (lfile_strs + 8, (uint8_t *) fname) + 1;			// include the NUL on the filename
	*(emit_ptr++) = LFILE_TOK;
	// process the input source file to completion, and recursively descend into any included files
	cpp_parse(&inf, lfile_strs, &lstrs_len, fname);

	// two buffers get handed to the next pass -- the emit buffer, and the lfile_strs buffer (renamed to name_strings)
	*(emit_ptr++) = TOK_ILLEGAL;		// (must increment the pointer to calculate the correct total buffer length)
	wrksp_top = wrksp + wrk_rem;
	if (outfd > 0)			// if the emit buffer overflowed into a file, finish off the file and close it
	{
		handle_emit_overflow(NULL);			// dump the tail end
		qcc_close (outfd);
		outfd = -1;
		outf_exists = 1;	// set a flag for the next pass, to let it know there is an input file
// HIHI!! flip the iof_in_toggle
		emit_ptr = emit_base = wrksp;
	}

	// name_strings must be stored ABOVE the emit buffer -- so there may be a little mem rearranging needed
// HIHI! I may change my mind about that again and delete this swapping code -- maybe it's better to put name strings below line_nums in token()
	i = emit_ptr - emit_base;						// total size of preprocessor output
	name_strings = wrksp_top - lstrs_len;
	if (name_strings < emit_ptr)					// will name_strings already fit above emit buf?
	{
		// will copying the emit buffer DOWN make enough room?
		if (name_strings < emit_ptr - (emit_base - lfile_strs - lstrs_len))
		{
			// must move the lfile strings down before anything else can move -- but then it will work
			memmove (wrksp, lfile_strs, lstrs_len);
			lfile_strs = wrksp;
		}
		memmove (lfile_strs + lstrs_len, emit_base, i);			// pack the emit buffer down
		emit_base = lfile_strs + lstrs_len;
	}
	memmove (name_strings, lfile_strs, lstrs_len);		// move the name strings up to the top of the workspace
	namestr_len = lstrs_len;

	wrksp_top = name_strings - i;						// move the emit buffer up to end at name_strings
	memmove (wrksp_top, emit_base, i);
	return 0;
}


void dump_cpp_output()
{
	FILE *out;
	uint8_t *p, in_str;
	char *c;
	// create outfile for writing as a text file
	out = fopen (outfile, "w");
	// XXX: for now, ignore the case where the emit buffer has been written to disk
	p = wrksp_top;
	in_str = 0;
	while (1)
	{
		if (*p == ESCCHAR_LE7F)
		{
			if (in_str == 0) putc ('\'', out);
			// emit \x (*++p & 0x7f) as hex
			putc ('\\', out);
			putc ('x', out);
			putc (hexout[(*++p >> 4) & 7], out);
			putc (hexout[*p & 0xf], out);
			if (in_str == 0) putc ('\'', out);
		}
		else if (*p == ESCCHAR_GT7F)
		{
			if (in_str == 0) putc ('\'', out);
			// emit \x *++p as hex
			putc ('\\', out);
			putc ('x', out);
			putc (hexout[*++p >> 4], out);
			putc (hexout[*p & 0xf], out);
			if (in_str == 0) putc ('\'', out);
		}
		else if (*p == '"')
		{
			in_str ^= 1;
			putc (*p, out);
		}
		else if (*p == ESCCHAR_TOK)			// turn tokenized keywords back into text
		{
			c = (char *) keywords[*++p - KEYWORDS_OFF];
			fwrite (c, strlen (c), 1, out);
			putc (' ', out);
		}
		else if (*p == LFILE_TOK) ;			// linefile tokens get discarded
		else if (*p == TOK_ILLEGAL) break;	// EOF, for now, until this routine gets expanded to handle reading
		else putc (*p, out);				// any other byte gets dumped as-is
		++p;
	}
	fclose (out);
}
