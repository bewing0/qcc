// possible mods:
// turn max_names_per_hash into a shift? -- it would eliminate 3 rare mults

struct pp_recursion_info {
	int depth;					// recursion depth for nested included files
	int fd;						// source file descriptor
	uint32_t line_num;
	int state;					// state machine info used only in read_compressed
	char *fname;				// the current source file being parsed (can be modified by #line)
	uint8_t *buf_top;			// a pointer to the terminating NUL + 1 in the input buffer
	uint8_t *unget;				// storage for "ungetted" bytes in read_compressed
	struct pp_recursion_info *parent_ptr;
};


int tokenize_op(uint8_t *s, uint8_t *d, int prep_flg);
int32_t calculate_expr(uint8_t *expr, uint64_t *llp, int32_t llcnt, long double *ldp, int32_t ldblcnt);
uint8_t detect_c_keyword(uint8_t *name, uint32_t len);

static uint16_t off[256];			// just a little reusable array


// special preprocessor byte "tokens"
#define ESCNL_TOK			0x18			// a standin for \ \n
#define ESCCHAR_LE7F		0x1b			// the next input byte must be considered as a literal, after the 0x80 bit is removed
#define ESCCHAR_GT7F		0x1c			// the next input byte must be considered as a literal (value >= 0x80)
#define PSEUDO_NL			0x1d			// this byte is treated as a newline by the preprocessor
#define ESCCHAR_TOK			0x1f			// the next input byte has already been tokenized


// how_far indicates the amount of data to show from *r
// 0 = show one byte, 1 = show one "word", -1 = show two "words"
void show_error(int level, char *str1, char *r, int how_far, struct pp_recursion_info *inf)
{
	char *p;
	int len;
// HIHI!! dump the tree of includes and line numbers
	if (level == 0)
	{
		write (2, "Error: ", 7);
		++total_errs;
	}
	else if (level > 0)
	{
		write (2, "Warning: ", 9);
		++total_warns;
	}
// else write (??, "info: ", 6); ?? -- If I send it to a non-2 fd, I have to do it *everywhere*
	write (2, str1, strlen(str1));
	if (r != NULL)
	{
		if (how_far == 0) len = 1;
		else
		{
			len = 0;
			p = r;
			while (*p != ' ' && *p != '\n' && *p != 0) ++p, ++len;
			if (how_far < 0 && *p == ' ')
			{
				++p;
				++len;
				while (*p != ' ' && *p != '\n' && *p != 0) ++p, ++len;
			}
		}
// HIHI!! if len == 1 and *r is non-printable, then do some cute display of it -- or should I do that for ALL non-printables in r?
		write (2, r, len);
	}
	write (2, "\n", 1);
}


// this code is only called if *p is pointing to a /x or /digit
int hexchar(uint8_t **p)
{
	uint8_t *c;
	int i, j;
	c = *p;
	// XXX: is there also a /b binary method for coding chars? Can the /X be capitalized? Test!
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
#define BOL_STATE			0		// it is reasonably important that this value be 0 (for efficiency)
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
void read_compressed(struct pp_recursion_info *inf, uint8_t *cpydwn, int32_t cdsize)
{
	uint8_t *p, *c, *sp;
	int lines_processed, i, j;

	// a little initting
	lines_processed = 0;
	sp = emit_ptr + 16*1024;				// compressed output storage
	p = sp + 64 + cdsize;					// raw data read point
	// include a stopper in front of the beginning of the raw data
	p[-1] = 0;
	// copy in any raw ungetted bytes from the previous call to read_compressed
	i = inf->state >> 4;
	c = inf->unget;
	while (--i >= 0) *(p++) = *(c++);

read_more:
	j = (30 * 1024) - cdsize;				// read the file in 30K gulps, starting at location p
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
	c = sp;						// compressed data write point
	sp = emit_ptr;				// 16K for string constant storage
	// copy in the bit of data that needs to be at the front of the buffer
	while (--cdsize >= 0) *(c++) = *(cpydwn++);
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
				*(c++) = '\n';					// newlines need to remain in the input for doing error messages
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
					else *(c++) = ESCNL_TOK, ++p;		// emit an escaped NL, skip the \n
				}
				else show_error (1, "unexpected \\ in input stream ignored", NULL, 1, inf);	
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
			while (stoppers[*p] == 0) ++p;	// discard to EOL or EOF
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
			else if (*p != '\'') show_error(0,"missing closequote: saw ", (char *) p, 0, inf);
			else ++p;					// step past the closequote
			break;

		case CHAR_LITRL_STATE:			// single char literal (singlequoted)
			if (*p == '\n' && j == 0)
			{
				show_error(0,"newline in char constant / missing closequote", NULL, 1, inf);
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
				show_error(0,"newline encountered inside string constant", NULL , 1, inf);
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
				show_error(0,"string constant too long to parse", NULL , 1, inf);
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
			// note: there is an additional 16k string buffer at 'emit_ptr' to take into account
			if ((c - emit_ptr) < 16 * 1024 + MAX_STRBUF_SIZE)
			{
				p = c + 64;
				while (--i >= 0) *(p++) = *(sp++);		// copy i chars from the unget buffer into p
				sp = c;
				goto read_more;
			}
			j = 0;		// force looping to end
		}
	}

	// there can be a large number of trailing blank lines on a file
	// (because of comments that were removed) -- compress them out
	if (inf->fd < 0)		// eliminate all trailing blank lines on the file
	{
		// theoretically, there could be chars in the unget buffer -- it should never happen in reality
		j = sp - emit_ptr;
		sp = emit_ptr;
		while (--j >= 0) *(c++) = *(sp++);		// copy j chars from the unget buffer into c

		if (c[-1] == '\n')
		{
			while (c[-1] == '\n') --c;		// *then* compress out excess newlines
			++c;					// put ONE newline back at the end
		}
	}
	else
	{
		// if there is any unget info in the string buffer, save it for the next call to read_compressed
		p = wrksp_top - i;
		inf->unget = wrksp_top = p;
		inf->state |= i << 4;
		while (--i >= 0) *(p++) = *(sp++);
	}
	*c = 0;
	total_lines += lines_processed;

	// finally, do a copyup to the end of wrksp
	// -- the buffer is usually big enough to deserve a call to memmove()
	p = emit_ptr + 16*1024;						// beginning of compressed output
	j = c - p + 1;								// get the compressed length (incl. terminator)
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
		if (*p == ESCNL_TOK) ++inf->line_num, ++p;		// skip escaped newlines
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


// skip over a block of code that failed a preprocessor test (eg. #if 0)
uint8_t *pp_bypass(uint8_t *p, uint16_t type, struct pp_recursion_info *inf)
{
	uint16_t cur_type, recur_depth;
	uint32_t i, j;
	uint8_t *c = p;				// avoid compiler "uninitted" warning
	recur_depth = 0;
	cur_type = type;			// if "type" is 1, then only stop at an #endif
	// for type 0, stop at any #else or #elif -- finding #else and #elif at top level are errors for type 1

	// note: #if and #elif have already parsed all the way to EOL and discarded
	while (*p != '\n' && *p != 0 && *p != ESCNL_TOK) ++p;		// but some of the others still need it
	while (1)
	{
		while (*p != '#' && *p != '\n' && *p != 0 && *p != ESCNL_TOK) ++p;
		while (*p == '\n' || *p == ESCNL_TOK)
		{
			++(inf->line_num);
			++p;
		}
		// MUST prescan a line -- hitting a non-terminal end-of-buffer will mess up the logic
		c = p;
		while (*c != '\n' && *c != 0) ++c;
		if (*c == 0)
		{
			if (inf->fd >= 0)
			{
				wrksp_top = c + 1;
				read_compressed(inf, p, (int32_t) (c - p));
				p = wrksp_top;
			}
			if (*p == 0) break;
		}
		if (*p == '#')
		{
			while (*++p == ' ');
			c = p;
			// do a simhash and parse for all the possible kinds of "#if"s & family
			j = 0;
			i = 1;
			while (*p >= 'a' && *p <= 'w' && i < TWENTY2EXP7 - 1)		// stop looping after 7 characters
			{
				j += (*p - 'a') * i;
				i *= 22;
				++p;
			}
			if (j == SIMHASH_IF)
			{
				// descend a recursion layer -- look for the #endif for this #if
				cur_type = 1;
				++recur_depth;
				// discard to an unescaped EOL
				unescaped_EOL(&p, NULL, 0, inf);
			}
			else if (j == SIMHASH_ELIF)
			{
				if (recur_depth == 0)
				{
					if (type == 0) break;
					else show_error(0, "#elif after #else", NULL, 1, inf);
				}
				// continue looking for the #endif for this #if/elif -- discard to an unescaped EOL
				unescaped_EOL(&p, NULL, 0, inf);
			}
			else if (j == SIMHASH_IFDEF)
			{
				// descend a recursion layer -- look for the #endif for this #ifdef
				cur_type = 1;
				++recur_depth;		// discard normally
			}
			else if (j == SIMHASH_IFNDEF)
			{
				// descend a recursion layer -- look for the #endif for this #ifndef
				cur_type = 1;
				++recur_depth;		// discard normally
			}
			else if (j == SIMHASH_ELSE)
			{
				if (recur_depth == 0)
				{
					if (type == 0) break;
					else show_error(0, "another #else after #else", NULL, 1, inf);
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
			// skip over any sharp (#) chars out to EOL after a preprocessor command
			while (*p != '\n' && *p != 0 && *p != ESCNL_TOK) ++p;
		}
	}

	return c;
}


// escaped newlines are still annoying special cases inside preprocessor commands -- skip them and spaces
int pp_whitesp(uint8_t **p, struct pp_recursion_info *inf)
{
	if (**p != ' ' && **p != ESCNL_TOK) return 0;

	if (**p == ESCNL_TOK)
		++inf->line_num;
	++*p;
	return 1;
}


// the source code is attempting to define a macro with arguments -- #define MAC(x,y) ...
// -- count the arguments, and escape and pretokenize each occurrence of an argument
// (because that makes it a million times easier to parse later, when it's used)
int32_t eval_fn_macro(uint8_t **rawdef, uint8_t *stor, struct pp_recursion_info *inf)
{
	int32_t i, j, k;
	uint8_t *p, *outp, *sp, *c;
	p = *rawdef + 1;
	while (pp_whitesp(&p, inf) != 0);
	outp = emit_ptr;					// create a workspace pointer
	i = 0;
	// count the number of arguments up to the close paren and get their names
	while (*p != ')' && *p != '\n' && *p != 0)
	{
// if (*p == ',') return an error? a negative value? or just do a show_error and only copy a NUL byte? HIHI!!
		off[i++] = p - *rawdef;				// off[] is a 256 entry reusable short array
		while (alnum_[*p] != 0) ++p;		// argument names *must* be alphanumeric
		while (pp_whitesp(&p, inf) != 0);
		if (*p == ',')
		{
			++p;
			while (pp_whitesp(&p, inf) != 0);
		}
//		else if (*p != ')') show_error  HIHI!!
	}
	// reemit the 2 and the arg count -- HIHI!! also verify argcnt < 255!
	*(outp++) = 2;
	*(outp++) = (uint8_t) i;

	// then parse and output the definition string -- find all occurrences of all arguments
	++p;
	while (pp_whitesp(&p, inf) != 0);
	off[i] = 0;
	while (*p != '\n' && *p != 0)
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
				c = *rawdef + off[k];
				if (strncmp ((const char *) c,(const char *) sp, j) == 0 && alnum_[c[j]] == 0)
				{
					*(outp++) = 2;			// flag char for an argument
					*(outp++) = (uint8_t) k + 1;		// arg num
					k = j = 0;				// kill the loop and flag a match
				}
			}
			if (j > 0)			// no match -- emit the raw string
			{
				while (--j >= 0) *(outp++) = *(sp++);
			}
		}
		while (alnum_[*p] == 0 && *p != '\n')
		{
			if (*p == ESCNL_TOK)
			{
				*(outp++) = ' ';
				++p;
				inf->line_num++;
			}
			else
				*(outp++) = *(p++);
		}
	}
	*rawdef = p;				// output a ptr to the EOL

	p = emit_ptr;
	*(outp++) = 0;				// the def string must end in a NUL
	i = (int32_t) (outp - p);
	j = i;
	while (--j >= 0) *(stor++) = *(p++);
	return i;		// return the number of chars copied into *stor (including the NUL)
}


// build the function-like macro argument pointer array
// p should be pointing at the open paren of a function-like macro with up to argcnt arguments
uint32_t parse_fn_macro_inputs(uint8_t *p, uint8_t argcnt, struct pp_recursion_info *inf)
{
	uint32_t len, i;
	uint16_t j;
	uint8_t *sp;
	sp = p;
	len = 1;
	memset (off, 0, 512);
	// prescan the input once for escaped newlines -- they must not be fed back into the input stream!
	while (*p != ')' && *p != 0 && *p != '\n' && len < MAX_MACRO_STRING)
	{
		// note: all the escaped newlines inside strings have already been parsed out
		// -- the only ones left are whitespace between arguments
		if (*p == ESCNL_TOK)
		{
			*p = ' ';
			++inf->line_num;
		}
		++p;
		++len;
	}
	if (*p != ')')
	{
		show_error(0, "could not find close paren for function-like macro", NULL, 1, inf);
		return len;
	}
	i = 0;
	j = 1;
	while (*++sp == ' ') ++j;
	while (*sp != ')')
	{
		off[i++] = j;							// place the offsets of each arg into off[]
		while (*sp != ',' && *sp != ')') ++j, ++sp;
		if (*sp == ',') ++j, *(sp++) = 0;		// replace commas & close paren with NUL chars
		while (*sp == ' ') ++j, ++sp;
	}
	*p = 0;
	while (i < (uint32_t) argcnt) off[i++] = j;			// finish out all argcnt entries of off
	return len;				// return the length just past the final NUL = close paren
}


// this routine expands the table that lives at the bottom of memory --
// expanding it can cause a cascade effect, moving up one or more of the tables above it ...
// theoretically even requiring a realloc of the entire workspace
void double_handle_table_size()
{
	int32_t i;
	uint32_t j, k;
	uint8_t *p;
	k = max_names_per_hash * 256 * 4 * 2;				// new size of the handle table = offset to string table

	if (k + namestr_len > wrk_used_base)			// would the moved string table overwrite the emit buffer?
	{
		// pick a new value for wrk_used_base -- round up to 256K above what's needed
		j = (k + namestr_len + 0x3ffff) & ~0x3ffff;
		p = wrksp + j;
		i = emit_ptr - (wrksp + wrk_used_base);						// amount of data currently in buffer
		if (j + i < wrk_rem - 30 * 1024 * MAX_NESTED_INCLUDES)		// would the emit buffer overflow the workspace?
			memmove(p, wrksp + wrk_used_base, i);			// no, everything still fits -- move up the emit buffer

//		else					// yes, it would overflow -- write all the emitted data to disk -- HIHI!!!! OYOY!!
//		{
//			if (j > wrk_rem - 30 * 1024 * MAX_NESTED_INCLUDES)		// would it overflow the workspace even with no data?
//					expand_wrksp();
			// if (outfd < 0) open it -- I need to have a filename prefigured?
			// write (outfd, wrksp + wrk_used_base, i);
		//	i = 0;
//		}
		emit_ptr = p + i;
		wrk_used_base = j;
	}

	// move up the string table
	p = wrksp + k;
	memmove (p, name_strings, namestr_len);
	name_strings = p;

	// move up the top 255 arrays of handles (array 0 doesn't move)
	i = 256;
	k = max_names_per_hash * 4;		// old number of bytes per array
	while (--i > 0)
	{
		j = k * i;					// old offset to handle entries for this hash
		// double their old offset, into the first half of the expanded array
		memmove (wrksp + j * 2, wrksp + j, k);		// note: alt_memmove might be nice
		// must zero out the top half of each expanded array
		memset (wrksp + (j * 2) + k, 0, k);
	}
	memset (wrksp + k, 0, k);		// the new top half of array 0 must also be cleared
	// double the nph value
	max_names_per_hash *= 2;
}


// code is requesting a known macro name to be undefined -- HIHI!! verify it's not a typedef before deleting? But I have no flag for that anymore ....
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
int32_t get_name_idx(uint8_t hashval, uint8_t **symname, int32_t len, int insert_flag)
{
	int32_t i, j, k;
	uint32_t *nametbl;
	uint8_t *p, *c;
	// find the insertion point in the name list -- verify whether it's a duplicate
	j = max_names_per_hash;
	i = j * hashval;
	// within this hash value, names are sorted by length
	nametbl = (uint32_t *) wrksp;
	// loop on finding the insertion point
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


// attempting to #define something -- put it all in the name hash and string tables
uint8_t *pp_build_name(uint8_t *mname, uint8_t *p, uint8_t hash, int32_t len, struct pp_recursion_info *inf)
{
	uint8_t *c;
	int32_t i, j, k;
	uint32_t *handle_tbl;
// HIHI!! verify that max_names_per_hash * 1k + namestr_len + len + MAX_STRING_SIZE won't overflow
// -- if it would, either copy up or write out the emitted data!
	if (max_names_per_hash * 1024 + namestr_len + len + MAX_STRBUF_SIZE > wrk_used_base)
		i = 0;
	// copy the name into the string table
	c = name_strings + namestr_len;
	alt_strncpy((char *) c, (char *) mname, len);
	c += len + 1;			// need this pointer for saving the definition string

	// find the insertion point in the token list -- verify whether it's a duplicate
	handle_tbl = (uint32_t *) wrksp;
	i = get_name_idx(hash, &mname, len, 1);

	if (i < 0)			// got an illegal match -- name is already defined
	{
		show_error(0, "redefinition of macro ", (char *) mname, 1, inf);
		// possible different error "(benign) redefinition of type" -- for typedefs  HIHI
		unescaped_EOL(&p, NULL, 0, inf);
		return p;
	}
	k = max_names_per_hash - 1;						// create a mask for the length of each hash list
	if ((i & k) == k || handle_tbl[i] != 0)			// is the insertion point free?
	{
		// find the end of the hash column
		j = i;								// start scanning from 'i' to find the end of the column
		while (handle_tbl[j] != 0) ++j;		// j = the current END of the column
		if ((j & k) == k)					// time to expand the handle table?
		{
			double_handle_table_size();
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
	if (*p == TOK_O_PAREN)			// parse this "function-like macro"
		namestr_len += len + 1 + eval_fn_macro(&p, c, inf);

	else			// standard #define -- store it verbatim
// HIHI!!!! if the definition is EMPTY, it should be replaced with a -1 (string)!!
		namestr_len += len + 1 + unescaped_EOL(&p, c, 1, inf);

	return p;
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
	long double *ldp;

	*c = emit_ptr + 4;						// make two buffers in the work area
	c[1] = *c + MAX_MACRO_STRING;
	j = unescaped_EOL(p, *c, 1, inf);		// raw string is copied into c[0]

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
									show_error(0, "preprocessor 'defined()' keyword requires close parentheses", NULL, 1, inf);
									return 0;
								}
								++s1;
							}
							h = hash (s, (int) j);
							j = get_name_idx (h, &s, j, 0);
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
						j = get_name_idx (h, &s, j, 0);
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
		}
		*d = 0;
	}

	// all macros have now been substituted away -- only numbers and operators should remain
	// so "tokenize" what is there
	s = c[toggle];
	d = --*c;				// the tokenized string is always stored in *c - 1
	*c -= 2;
	d[-2] = 0;				// put an illegal token on the front, and parens around the expression
	d[-1] = c_ops['('];
	// get an aligned pointer just above c[1] (inside wrksp) for an array of 64b ints
	llp = (uint64_t *)wrksp + (MAX_MACRO_STRING + c[1] - wrksp)/8;
// HHI!!! actually, I think I need to start back at wrksp and add ALL of it?? Because this only starts on an 8b alignment, not a ldbl alignment.
// HIHI!!! actually(2) -- I think I'm going to be building all the long doubles BY HAND if they are not supported, and then doing all the float
// comparisons by hand, too! Ugh! But if they really have a 64bit mantissa, then that obviously fits nicely in a qword, and the rest in a short.
// So I would keep TWO arrays, and 8b alignment would be just fine.
	ldp = (long double *)llp + (200 * 8) / sizeof(long double);
	// the first entries in both tables are required to be a constant 0
	*llp = 0;
//	*ldp = 0.0;		HIHI -- should this be [255]?

	i = TOK_SIZEOF + 1;			// first token that is available to use as an index in the preprocessor
	j = 256;
	while (*s != 0)
	{
		if (*s >= '0' && *s <= '9')				// numbers always must start with a digit (minus signs are treated as operators)
		{
			h = 0;			// assume int
			// skip a 0x or 0b, then look for a radix point
			s1 = s + 1;
			if (*s == '0' && ((*s1 | 0x20) == 'x' || (*s1 | 0x20) == 'b')) ++s1;
			while (hex_lkup[*s1] >= 0) ++s1;
			// the next byte MUST be a radix point, or it's not a float
			if (*s1 == '.') h = 1;

			// HIHI!!! since I'm about to take control of both floatscan and intscan -- combine them, and have them figure out for
			// themselves whether the value is int or float?
			if (h == 0)
			{
				llp[++i - TOK_SIZEOF - 1] = strtoull((const char *) s, (char **) &s, 0);
				*(d++) = (uint8_t) i;
			}
			else
			{
				ldp[--j - TOK_SIZEOF - 1] = strtold((const char *) s, (char **) &s);
				*(d++) = (uint8_t) j;
			}
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
				llp[++i - TOK_SIZEOF - 1] = (uint64_t) (*++s & 0x7f);
				*(d++) = (uint8_t) i;
				++s;						// convert the next literal byte into a variable (and move beyond it)
			}
			else if (*s == ESCCHAR_GT7F)		// char literal (type 2)
			{
				llp[++i - TOK_SIZEOF - 1] = (uint64_t) *++s;
				*(d++) = (uint8_t) i;
				++s;						// convert the next literal byte into a variable (and move beyond it)
			}
//			else
				// illegal character in constant expression (*s)
		}
	}
	*(d++) = c_ops[')'];			// put a tokenized ) on the end of the expression
	*d = 0;
	i -= TOK_SIZEOF;

	i = calculate_expr(*c + 1, llp, i, ldp, 256 - j);
	return i;
}


// HIHI!! move this fn out of cpp -- it's generic! into libc_compat, perhaps?
void handle_emit_overflow()
{
	int i;
	char *b = (char *) wrksp + wrk_used_base;
	i = (char *) emit_ptr - b;
	if (outfd < 0)
	{
		outfd = qcc_create("hi1");
//		outfd = qcc_create(inout_fnames[iof_in_toggle ^ 1]);		// HIHI need to init inout_fnames, still
// HIHI and what happens if the open fails? show_error? fatal error and die? It can't be a show_error if I make it generic.
	}
	write (outfd, b, i);			// dump out the entire emit buffer
	emit_ptr = (uint8_t *) b;		// and reset the emit ptr back to base
}


int32_t name_lookup(uint8_t *p, struct pp_recursion_info *inf)
{
	uint8_t *c;
	int32_t i;
	c = p;
	i = 0;						// create a token out of the defined name
	while (alnum_[*p] != 0) ++p, ++i;
	// look it up in the token list -- a match returns a NEGATIVE index!
	return get_name_idx(hash (c, i), &c, i, 0);
}


void cpp_parse(uint8_t *p, struct pp_recursion_info *pinfo)
{
	uint32_t j, k;
	int32_t i;
	uint8_t *c, *sp, hsh_val, bypass_flg;
	struct pp_recursion_info ninfo;

	// Because of the "nested" (recursive) style of includes, and the fact
	// that there is a limit to the number of files that can be open at any
	// one time, there must be a "maximum recursion depth" limit imposed.
	// It is a user-modifiable value in config.h.
	if (pinfo->depth > MAX_NESTED_INCLUDES)
// XXX: make this a limit on the max number of open files, just so long as there is some room left in the reading workspace?
	{
		if (pinfo->fd > 0) close (pinfo->fd);
		pinfo->fd= -1;
		return;
	}

	// init some parts of the ninfo struct from the parent -- ninfo is used when including nested files
	ninfo.depth = pinfo->depth + 1;
	ninfo.parent_ptr = pinfo;

	// loop over the source file until it is fully processed
	while (*p != 0)
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
				while (pp_whitesp(&p, pinfo) != 0);
				sp = p;				// save a pointer for error message display
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
					// finish detecting the "next" part
					// if (strncmp(p, "next", 4) != 0)
					goto unrecognized;
					// p += 4, verify whitespace, and do the rest of it -- XXX
				}
				else if (pp_whitesp(&p, pinfo) == 0 && *p != '\n')  goto unrecognized;
// Note: the SIMHASH values are too big for a switch statement to handle efficiently -- it would just produce this same elseif block
				else if (j == SIMHASH_INCLUDE)
				{
					while (pp_whitesp(&p, pinfo) != 0);
					if (*p != '"' && *p != '<') goto unrecognized;
					c = p++;						// remember: the line was already prescanned for 'no 0'
					j = 0;
					if (*(c++) == '"')				// normal include file?
					{
						while (*p != '\"' && *p != '\n') ++p, ++j;
						if (*p != '"') goto unrecognized;
						sp = (uint8_t *) da_buffers[INCLUDE_PATHS];
					}
					else
					{
						while (*p != '>' && *p != '\n') ++p, ++j;			// system include?
						if (*p != '>') goto unrecognized;
						sp = (uint8_t *) SYS_INCLUDE_PATHS;
					}
						// emit a "comment" to the output stream with the included filename
					// -- this filename may also be displayed in error messages, and it is used to open the file
	// HIHI!!! first 8 bytes is line number as a hex char string (1bcd0408), then a NUL terminated filename
					*(emit_ptr++) = TOK_LINEFILE;
					i = 7;
					while (--i >= 0) *(emit_ptr++) = '0';					// set line number to 1
					*(emit_ptr++) = '1';
//					strncpy ((char *) ???, "          ", ninfo.depth);		// make a pretty indention to show nested includes
					ninfo.fname = (char *) emit_ptr;						// permanently save the filename pointer
					ninfo.fd = -1;
					while (ninfo.fd < 0 && i != 0)
					{
						// sp points to a directory to search for the specified include file
						k = alt_strcpy(ninfo.fname, (char *) sp);
						sp += k + 1;
						alt_strncpy(ninfo.fname + k, (char *) c, j);
						ninfo.fd = qcc_open_r((char *) ninfo.fname, 0);
						if (*sp == 0) i = 0;
					}
					// failed after looking in ALL the supplied directories?
					if (ninfo.fd < 0)
					{
						*p = 0;			// p is pointing at the closequote on the fname
						show_error(0, "File not found, trying to include file ", (char *) c, 1, pinfo);
						*p = '"';
						goto discard;
					}
		// HIHI if being verbose? -- write (1, emit_ptr, strlen(emit_ptr)); -- and then a \n?
					while (*emit_ptr != 0) ++emit_ptr;				// emit_ptr contains a good NUL-terminated "comment"
					++emit_ptr;
					while (*p != '\n' && *p != ESCNL_TOK) ++p;		// discard to EOL, in order to create a good return point
					wrksp_top = p;									// -- and to calculate a good value for wrksp_top
					// read in the included file -- suppress comments and whitespace
					ninfo.buf_top = p;
					ninfo.state = 0;
					ninfo.line_num = 1;
					read_compressed (&ninfo, NULL, 0);
					// prepare to recurse
					cpp_parse(wrksp_top, &ninfo);
	// HIHI!!! emit another 0x1a comment with the current filename and line number!
				}
				else if (j == SIMHASH_DEFINE)
				{
					while (pp_whitesp(&p, pinfo) != 0);
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
						else if (pp_whitesp(&p, pinfo) == 0 && *p != '\n') goto unrecognized;
						p = pp_build_name(c, p, hsh_val, i, pinfo);
					}
					else
						show_error(0, "attempt to redefine a reserved keyword: ", (char *) c, 1, pinfo);
				}
				else if (j == SIMHASH_IF)
				{
					while (pp_whitesp(&p, pinfo) != 0);
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
					while (pp_whitesp(&p, pinfo) != 0);
					// get the token name and try to find it
					i = name_lookup(p, pinfo);
					// if it's not there, bypass to the next matched elif, else, or endif
					if (i >= 0)
					{
						bypass_flg = 1;
						p = pp_bypass(p, 0, pinfo);
						goto bypass_done;
					}
				}
				else if (j == SIMHASH_IFNDEF)
				{
					while (pp_whitesp(&p, pinfo) != 0);
					// get the token name and try to find it
					i = name_lookup(p, pinfo);
					// if it's there, bypass to the next matched elif, else, or endif
					if (i < 0)
					{
						bypass_flg = 1;
						p = pp_bypass(p, 0, pinfo);
						goto bypass_done;
					}
				}
				else if (j == SIMHASH_ELIF)
				{
					// if we did not reach here through a bypass, then bypass
					if (bypass_flg == 0)
					{
						while (pp_whitesp(&p, pinfo) != 0);
						i = eval_const_expr(&p, pinfo);
						if (i == 0)
						{
							bypass_flg = 1;
							p = pp_bypass(p, 0, pinfo);
							goto bypass_done;
						}
					}
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
				}
				// #endif is really just a placeholder no op -- do nothing
				else if (j == SIMHASH_ENDIF) ;
				else if (j == SIMHASH_UNDEF)
				{
					while (pp_whitesp(&p, pinfo) != 0);
					i = name_lookup(p, pinfo);
					if (i < 0) delete_name_hash(i);
				}
				else if (j == SIMHASH_WARNING)
				{
					c = p;
					i = 1;		// print to eol as a compiler warning? What about escaped NLs?
					while (*p != '\n') ++p, ++i;
					write (2, c, i);
					++total_warns;
				}
				else if (j == SIMHASH_ERROR)
				{
					c = p;
					i = 1;		// print to eol as a compiler error?
					while (*p != '\n') ++p, ++i;
					write (2, c, i);
					++total_errs;
				}
				else if (j == SIMHASH_LINE)
				{
					i = 0;		// HIHI!! this directive provides a line number (and an optional filename) (but what is the exact format?)
					// first 8 bytes is line number as a hex char string (1bcd0408), then a NUL terminated filename
					// -- which must replace pinfo->fname, and pinfo->line_num
					// -- perhaps the emitted comment should say "#line directive reset:" ?? And completely suppress the verbose screen display?
/*						alt_strcpy ((char *) emit_ptr, "\x1aincluding file: ");				// 0x1a = COMMENT_TOK in the emitted bytestream
						pinfo->fname = (char *) emit_ptr + 17;				// permanently save the filename pointer
						alt_strncpy(pinfo->fname, (char *) directive_filename, i);
						pinfo->line_num = directive_linenum;
			// HIHI if being verbose? -- write (1, emit_ptr, strlen(emit_ptr)); -- and then a \n? -- but if I'm using strlen, then I can eliminate the next while loop
						while (*emit_ptr != 0) ++emit_ptr;		// emit_ptr contains a good NUL-terminated "comment"
						++emit_ptr;		*/
				}
				else if (j == SIMHASH_PRAGMA)
				{
					i = 0;		// the only one is "pack"? Or are "push" and "pop" also valid?  HIHI!!
					// pack overrides any automatic alignment in structs
				}
				else
unrecognized:
					// unrecognized preprocessor command
					show_error (0, "Preprocessor command can not be parsed #", (char *) sp, -1, pinfo);
discard:
				// the 0x1d character is treated like a special newline for preprocessor commands only
				while (*p != '\n' && *p != 0x1d && *p != ESCNL_TOK && *p != 0) ++p;		// preprocessor commands always discard to EOL
				if (*p == 0x1d) ++p;
			}		// end of doublesharp if-else
			if (p - emit_ptr < 0x10000) handle_emit_overflow();
			break;										// end of preprocessor commands section (#)

		case '(':	case ')':	case '=':	case '<':
		case '>':	case '&':	case '|':	case '!':
		case '+':	case '-':	case '*':	case '/':
		case '%':	case '^':	case '~':	case '?':
		case '[':	case ']':
		case '{':	case '}':	case ':':
		case ',':	case '.':
			*(emit_ptr++) = *(p++);
			break;

		case ESCCHAR_LE7F:			// "escape" byte flags for literal char constants -- emit two raw bytes
		case ESCCHAR_GT7F:
			// note: literal bytes are guaranteed to have values less than 256, so they are never counted in nxt_pass_info[2]
			++nxt_pass_info[3];			// but are always counted as numeric values
			*(emit_ptr++) = *(p++);
			// cheat and fall through to copy the second byte

		case ';':
			*(emit_ptr++) = *(p++);
			break;

		case '"':						// all escaped newlines have already been removed from inside string constants
			sp = p;
			*(emit_ptr++) = *(p++);
			while (*p != '\"' && *p != '\n' && *p != 0) *(emit_ptr++) = *(p++);
			if (*p == '\n') show_error(0,"newline in string constant", NULL, 1, pinfo);
			else if (*p == 0) show_error(0,"unmatched doublequotes at EOF", NULL, 1, pinfo);
			else *(emit_ptr++) = *(p++);
			*nxt_pass_info += p - sp;
			break;

		case '0':	case '1':			// emit pure numbers raw
		case '2':	case '3':
		case '4':	case '5':
		case '6':	case '7':
		case '8':	case '9':
			++nxt_pass_info[3];			// total count of all numeric values
			// overestimate a total count of numeric constants with values bigger than 255 (just do a quickie test)
			// -- if there are more than 2 digits in the number or it's a float, then "it's bigger than 255"
			c = p + 1;
			if (*p == '0' && (p[1] | 0x20) == 'x') c += 2;
			if ((hex_lkup[*c] >= 0 || *c == '.') && (hex_lkup[c[1]] >= 0 || c[1] == '.'))
				++nxt_pass_info[2];
			while (alnum_[*p] != 0) *(emit_ptr++) = *(p++);			// emit the raw text into the output buffer
			// note: if there is alpha crud on the end of the number, it will give a syntax error later
			break;

		case '_':		// parse occurrences of ___LINE__, ___FILE__, ___DATE__, ___TIME__, ___FUNCTION__, ___VA_ARGS__
			if (p[1] == '_' && p[2] == '_')
			{
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
					i = ntc(pinfo->line_num, (char *) emit_ptr);
					emit_ptr += i;
					break;
				}
				else if (j == SIMHASH_FILE)			// emit the full pathname as a quoted string
				{
					i = strlen(pinfo->fname);
					*(emit_ptr++) = '"';
					c = (uint8_t *) pinfo->fname;
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
bad_3_:
					show_error(0, "unknown preprocessor macro: ", (char *) sp, 1, pinfo);
				if (p - emit_ptr < 0x10000) handle_emit_overflow();
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
			c = p;			// keyword, macro, variable, constant, function name, etc.
			sp = emit_ptr;
			j = 0;
			while (alnum_[*p] != 0) *(emit_ptr++) = *(p++), ++j;			// tentatively emit the raw text into the output buffer
			i = detect_c_keyword(c, j);
			if (i == 0)
			{
				hsh_val = hash(c, (int) j);
				i = get_name_idx(hsh_val, &c, j, 0);
				if (i < 0)				// is it a known macro?
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
								sp += alt_strcpy ((char *) sp, (char *) p + off[i - 1]);
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
					*(emit_ptr++) = ' ';					// keep a whitespace between alphanumeric "words" for now
					*nxt_pass_info += emit_ptr - sp;		// accumulate a total length
					++nxt_pass_info[1];						// accumulate an overestimate count of unique alphanumeric strings
				}
			}
			else		// tokenize all recognized keywords
			{
				emit_ptr = sp;					// remove the raw text from the output buffer
				*(emit_ptr++) = ESCCHAR_TOK;
				*(emit_ptr++) = (uint8_t) i;
			}
			break;

		case ESCNL_TOK:					// treat an escaped newline just like a regular newline
			*(emit_ptr++) = '\n';
			++p;
			++pinfo->line_num;			// then fall through

		case '\n':
			while (*p == '\n') *(emit_ptr++) = *(p++), 	++pinfo->line_num;
			// verify that emit_ptr isn't close to overflowing (soft 64k limit)
			if (p - emit_ptr < 0x10000) handle_emit_overflow();

			// make sure there is always at least 16K in the input buffer, unless the file is closed
			// -- that way, running into the EOB is always just an error
			if (pinfo->fd >= 0 && pinfo->buf_top - p < 16 * 1024)
			{
				wrksp_top = pinfo->buf_top;
				read_compressed(pinfo, p, (int32_t)(wrksp_top - p - 1));
				p = wrksp_top;
			}
			// verify that at least one complete line is still loaded -- to prevent infinite loops
			c = p;
			while (*c != '\n' && *c != 0) ++c;
			if (*c == 0 && *p != 0) show_error(0, "line is too long to process", NULL, 1, pinfo);

		case 0:
			break;

		default:
			show_error(0, "Unrecognized symbol ", (char *) p, 0, pinfo);
			i = *(p++);		// unrecognized symbol! (`@$)

		}		// end of switch on *p

	}
	if (p - emit_ptr < 0x10000) handle_emit_overflow();
}

// a zero-terminated file is stored at wrksp_top,
// and it needs preprocessing for: includes, defines, macros, #ifs, typedefs
int preprocess(int fd, char *fname)
{
	int32_t i;
	struct pp_recursion_info inf;

	// read a few 30K gulps of the main source file -- compress out C comments and whitespace
	inf.depth = 0;
	inf.fname = fname;
	inf.line_num = 1;
	inf.parent_ptr = NULL;
	inf.state = 0;
	inf.fd = fd;
	inf.buf_top = wrksp_top;
	emit_ptr = wrksp;					// read_compressed needs a valid workspace ptr in emit_ptr
	read_compressed(&inf, NULL, 0);

	// In the preprocessor, "names" consist of #defined macro names and typedefs.
	// The preprocessor does all the necessary substitutions on all the names,
	// througout the main source file and all included files. The final preprocessed
	// output is (of course) passed on to the next compilation stage, and all the
	// name info from the preprocessor is deleted to save memory.

	// the name table is initially allocated containing 32 empty name handles per hash value
	// -- if any hash value gets filled up, the size is doubled and memory gets rearranged
	max_names_per_hash = 32;
	i = max_names_per_hash * 4 * 256;

	// init the name hashes in the list to "unused"
	memset (wrksp, 0, i);

	// the raw name strings begin just above the name hash list (growing up)
	name_strings = wrksp + i;
	namestr_len = 0;

	// put the preprocessed output stream in the middle of the remaining workspace, growing up
	i = wrk_rem / 2;
	// don't allocate more than 2M for the names, initially
	if (i > 2 * 1024 * 1024) i = 2 * 1024 * 1024;
	wrk_used_base = i;
	emit_ptr = wrksp + i;

	// copy in the global predefines now, for processing -- as text, prepended to the source file
	i = da_tot_entrylen[PREDEFINES];
	memmove (wrksp_top - i, da_buffers[PREDEFINES], i);

	memset (nxt_pass_info, 0, 16);
	outfd = -1;

	*(emit_ptr++) = TOK_LINEFILE;			// must pre-emit the first "line" directive for the input file
	i = 7;
	while (--i >= 0) *(emit_ptr++) = '0';					// set line number to 1
	*(emit_ptr++) = '1';
	emit_ptr += alt_strcpy ((char *) emit_ptr, fname) + 1;		// include the NUL on the filename

	// process the input source file to completion, and recursively descend into any included files
	cpp_parse(wrksp_top - i, &inf);
	*(emit_ptr++) = TOK_ENDOFBUF;		// (must increment the pointer to calculate the correct total buffer length)

	if (outfd > 0)			// if the emit buffer overflowed into a file, finish off the file and close it
	{
		handle_emit_overflow();			// dump the tail end
		close (outfd);
		outfd = -1;
		outf_exists = 1;	// set a flag for the next pass, to let it know there is an input file
// HIHI!! flip the iof_in_toggle?
	}
	else
	{
		// all the emitted data managed to fit into memory
		i = emit_ptr - (wrksp + wrk_used_base);		// total size of preprocessor output
		wrksp_top = wrksp + wrk_rem - i;			// copy it all up to end at wrksp + wrk_rem
		memmove (wrksp_top, wrksp + wrk_used_base, i);
	}

	return 0;
}
