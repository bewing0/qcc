// this file is used to try to get a little extra compatibility across compilers and various libc implementations
// -- it provides wrapper functions and simple builtins that hide platform ugliness from the rest of the program
// (such as MSVC constantly trying to deprecate standard libc functions,
// or refusing to compile them because they are "unsafe",
// or dealing with ascii/widechar/unicode issues in system calls)

// So almost all ifdefs to deal with OS- Compiler- Libc- or whatever-specific issues should be confined to this file,
// if at all possible.

// macros to handle bigendianness issues are also contained here

#include <fcntl.h>
#include <sys/stat.h>

// HIHI can I get rid of these includes?
#include <math.h>
#include <float.h>


// strcpy that returns the length of the string
int alt_strcpy(uint8_t *to, uint8_t *from)
{
	int i;
	i = 0;
	while (*from != 0)
	{
		*(to++) = *(from++);
		++i;
	}
	*to = 0;
	return i;
}


// strncpy that adds a 0 to the end of the *to string
void alt_strncpy(char *to, char *from, int n)
{
	while (--n >= 0) *(to++) = *(from++);
	*to = 0;
}


// open a file for reading
int qcc_open_r(char *fname, int bin_flag)
{
	int i;
	if (bin_flag != 0)
		i = open (fname, _O_BINARY);
	else
		i = open (fname, 0);
	return i;
}

//int qcc_open_w(char *fname)
//{
//}

// open a file for writing, whether it already exists or not
int qcc_create(char *fname)
{
	int i;
	i = open (fname, _O_BINARY | _O_WRONLY | _O_CREAT, _S_IWRITE);
	return i;
}

void qcc_close(int fd)
{
	close (fd);
}

int qcc_read(int fd, char *buf, int len)
{
	int i;
	i = read (fd, buf, len);
	return i;
}

void qcc_write(int fd, char *buf, int len)
{
	write(fd, buf, len);
}


void read_tok(uint8_t **p, struct pass_info *inf)
{
	int32_t i;
	uint8_t *c, *s;
	s = wrksp_top;	// if an input file exists, then wrksp_top always points to a 30K read buffer
	c = *p;			// perform the copydown, get the length
	i = 0;
	while (*c != TOK_ENDOFBUF)
	{
		*(s++) = *(c++);
		++i;
	}
	i = read (inf->infd, s, 30 * 1024 - i);
	wrksp_top[i] = TOK_ENDOFBUF;
	if (i == 0)
	{
		close (inf->infd);
		inf->infd = -1;
	}
	*p = wrksp_top;
}


// convert a number to a decimal char string
int ntc(int32_t i, char *p)
{
	long j;
	int r;
	r = 0;
	if (i < 0){
		i = -i;
		r = 1;
		*(p++) = '-';
	}
	if (i>999999999) j=1000000000;
	else if (i>99999999) j=100000000;
	else if (i>9999999) j=10000000;
	else if (i>999999) j=1000000;
	else if (i>99999) j=100000;
	else if (i>9999) j=10000;
	else if (i>999) j=1000;
	else if (i>99) j=100;
	else if (i>9) j=10;
	else j = 1;
	while (j != 0){
		++r;
		*(p++)= (i/j)%10 + '0';
		j/=10;
	}
	*p = 0;
	return r;
}



#ifdef WIN32
#include <sys/timeb.h>

int64_t microseconds()
{
	struct _timeb tb;
	_ftime(&tb);
	return ((int64_t) tb.time * 1000 + tb.millitm) * 1000;
}

#else
int64_t microseconds()
{
	struct timeval tv;
	gettimeofday(&tv, NULL);
	return (int64_t)tv.tv_sec * 1000000 + tv.tv_usec;
}
#endif


uint8_t dfloat_parse (uint8_t *instr, uint8_t **endstr, double *dval, uint64_t *iman, uint16_t *iexp)
{
// HIHI!! if dval is NULL, then always put the value in iman/iexp
	return 0;
}


// parse a char string as either a 64bit int, a double, or a long double
uint8_t num_parse (uint8_t *instr, uint8_t **endstr, uint64_t *ival, double *dval, uint64_t *iman, uint16_t *iexp)
{
	uint8_t c, *s1;
	int8_t h;
	uint32_t x;
	uint64_t y;

	// skip a 0x or 0b, then look for a radix point
	s1 = instr + 1;
	if (*instr == '0' && ((*s1 | 0x20) == 'x' || (*s1 | 0x20) == 'b')) ++s1;
	while (hex_lkup[*s1] >= 0) ++s1;
	// the next byte MUST be a radix point, or it's not a float
	if (*s1 == '.')
		return dfloat_parse(instr, endstr, dval, iman, iexp);

	// parse as an int, into ival -- do the easy part using 32b, then switch to 64b
	x = 0;
	if (*instr == '0')			// parse as something other than decimal?
	{
		s1 = instr + 1;
		if ((*s1 | 0x20) == 'x')			// parse as hex
		{
			h = 8;
			c = hex_lkup[*++s1];
			while (--h >= 0 && (int8_t) c >= 0)
			{
				x = (x << 4) | c;
				c = hex_lkup[*++s1];
			}
			y = x;
			h = 8;
			while (--h >= 0 && (int8_t) c >= 0)
			{
				y = (y << 4) | c;
				c = hex_lkup[*++s1];
			}
		}
		else if ((*s1 | 0x20) == 'b')		// parse as binary
		{
			h = 32;
			c = *++s1 - '0';
			while (--h >= 0 && c < 2)
			{
				x = (x << 1) | c;
				c = *++s1 - '0';
			}
			y = x;
			h = 32;
			while (--h >= 0 && c < 2)
			{
				y = (y << 1) | c;
				c = *++s1 - '0';
			}
		}
		else		// parse as octal
		{
			h = 10;
			c = *s1 - '0';
			while (--h >= 0 && c < 8)
			{
				x = (x << 3) | c;
				c = *++s1 - '0';
			}
			y = x;
			while (c < 8 && y <= (~ (uint64_t) 0) >> 3)
			{
				y = (y << 3) | c;
				c = *++s1 - '0';
			}
		}
	}
	else			// parse as decimal
	{
		s1 = instr;
		c = *s1 - '0';
		while (c < 10 && x <= (0xffffffff / 10) -1)
		{
			x = x*10 + c;
			c = *++s1 - '0';
		}
		y = x;
		while (c < 10 && y <= (~ (uint64_t) 0) / 10 && 10*y <= (~ (uint64_t) 0) - c)
		{
			y = y*10 + c;
			c = *++s1 - '0';
		}
	}
	*ival = y;
	*endstr = s1;
	return 0;
}


#if 0

// the following code uses the 0x1.0pxxx format, which *won't compile under MSVC!*  So I have to replace it all somehow!!
// -- maybe the horrible stuff in tcc.c is really what I need after all.

#if LDBL_MANT_DIG == 53 && LDBL_MAX_EXP == 1024

#define LD_B1B_DIG 2
#define LD_B1B_MAX 9007199, 254740991
#define KMAX 128

#else /* LDBL_MANT_DIG == 64 && LDBL_MAX_EXP == 16384 */

#define LD_B1B_DIG 3
#define LD_B1B_MAX 18, 446744073, 709551615
#define KMAX 2048

#endif

#define MASK (KMAX-1)

#define CONCAT2(x,y) x ## y
#define CONCAT(x,y) CONCAT2(x,y)

static int64_t scanexp(FILE *f, int pok)
{
	int c;
	int x;
	int64_t y;
	int neg = 0;

	c = shgetc(f);
	if (c=='+' || c=='-') {
		neg = (c=='-');
		c = shgetc(f);
		if (c-'0'>=10U && pok) shunget(f);
	}
	if (c-'0'>=10U) {
		shunget(f);
		return LLONG_MIN;
	}
	for (x=0; c-'0'<10U && x<INT_MAX/10; c = shgetc(f))
		x = 10*x + c-'0';
	for (y=x; c-'0'<10U && y<LLONG_MAX/100; c = shgetc(f))
		y = 10*y + c-'0';
	for (; c-'0'<10U; c = shgetc(f));
	shunget(f);
	return neg ? -y : y;
}


static long double decfloat(FILE *f, int c, int bits, int emin, int sign, int pok)
{
	uint32_t x[KMAX];
	static const uint32_t th[] = { LD_B1B_MAX };
	int i, j, k, a, z;
	int64_t lrp=0, dc=0;
	int64_t e10=0;
	int lnz = 0;
	int gotdig = 0, gotrad = 0;
	int rp;
	int e2;
	int emax = -emin-bits+3;
	int denormal = 0;
	long double y;
	long double frac=0;
	long double bias=0;
	static const int p10s[] = { 10, 100, 1000, 10000,
		100000, 1000000, 10000000, 100000000 };

	j=0;
	k=0;

	/* Don't let leading zeros consume buffer space */
	for (; c=='0'; c = shgetc(f)) gotdig=1;
	if (c=='.') {
		gotrad = 1;
		for (c = shgetc(f); c=='0'; c = shgetc(f)) gotdig=1, lrp--;
	}

	x[0] = 0;
	for (; c-'0'<10U || c=='.'; c = shgetc(f)) {
		if (c == '.') {
			if (gotrad) break;
			gotrad = 1;
			lrp = dc;
		} else if (k < KMAX-3) {
			dc++;
			if (c!='0') lnz = dc;
			if (j) x[k] = x[k]*10 + c-'0';
			else x[k] = c-'0';
			if (++j==9) {
				k++;
				j=0;
			}
			gotdig=1;
		} else {
			dc++;
			if (c!='0') x[KMAX-4] |= 1;
		}
	}
	if (!gotrad) lrp=dc;

	if (gotdig && (c|32)=='e') {
		e10 = scanexp(f, pok);
		if (e10 == LLONG_MIN) {
			if (pok) {
				shunget(f);
			} else {
				shlim(f, 0);
				return 0;
			}
			e10 = 0;
		}
		lrp += e10;
	} else if (c>=0) {
		shunget(f);
	}
	if (!gotdig) {
		errno = EINVAL;
		shlim(f, 0);
		return 0;
	}

	/* Handle zero specially to avoid nasty special cases later */
	if (!x[0]) return sign * 0.0;

	/* Optimize small integers (w/no exponent) and over/under-flow */
	if (lrp==dc && dc<10 && (bits>30 || x[0]>>bits==0))
		return sign * (long double)x[0];
	if (lrp > -emin/2) {
		errno = ERANGE;
		return sign * LDBL_MAX * LDBL_MAX;
	}
	if (lrp < emin-2*LDBL_MANT_DIG) {
		errno = ERANGE;
		return sign * LDBL_MIN * LDBL_MIN;
	}

	/* Align incomplete final B1B digit */
	if (j) {
		for (; j<9; j++) x[k]*=10;
		k++;
		j=0;
	}

	a = 0;
	z = k;
	e2 = 0;
	rp = lrp;

	/* Optimize small to mid-size integers (even in exp. notation) */
	if (lnz<9 && lnz<=rp && rp < 18) {
		if (rp == 9) return sign * (long double)x[0];
		if (rp < 9) return sign * (long double)x[0] / p10s[8-rp];
		int bitlim = bits-3*(int)(rp-9);
		if (bitlim>30 || x[0]>>bitlim==0)
			return sign * (long double)x[0] * p10s[rp-10];
	}

	/* Align radix point to B1B digit boundary */
	if (rp % 9) {
		int rpm9 = rp>=0 ? rp%9 : rp%9+9;
		int p10 = p10s[8-rpm9];
		uint32_t carry = 0;
		for (k=a; k!=z; k++) {
			uint32_t tmp = x[k] % p10;
			x[k] = x[k]/p10 + carry;
			carry = 1000000000/p10 * tmp;
			if (k==a && !x[k]) {
				a = (a+1 & MASK);
				rp -= 9;
			}
		}
		if (carry) x[z++] = carry;
		rp += 9-rpm9;
	}

	/* Upscale until desired number of bits are left of radix point */
	while (rp < 9*LD_B1B_DIG || (rp == 9*LD_B1B_DIG && x[a]<th[0])) {
		uint32_t carry = 0;
		e2 -= 29;
		for (k=(z-1 & MASK); ; k=(k-1 & MASK)) {
			uint64_t tmp = ((uint64_t)x[k] << 29) + carry;
			if (tmp > 1000000000) {
				carry = tmp / 1000000000;
				x[k] = tmp % 1000000000;
			} else {
				carry = 0;
				x[k] = tmp;
			}
			if (k==(z-1 & MASK) && k!=a && !x[k]) z = k;
			if (k==a) break;
		}
		if (carry) {
			rp += 9;
			a = (a-1 & MASK);
			if (a == z) {
				z = (z-1 & MASK);
				x[z-1 & MASK] |= x[z];
			}
			x[a] = carry;
		}
	}

	/* Downscale until exactly number of bits are left of radix point */
	for (;;) {
		uint32_t carry = 0;
		int sh = 1;
		for (i=0; i<LD_B1B_DIG; i++) {
			k = (a+i & MASK);
			if (k == z || x[k] < th[i]) {
				i=LD_B1B_DIG;
				break;
			}
			if (x[a+i & MASK] > th[i]) break;
		}
		if (i==LD_B1B_DIG && rp==9*LD_B1B_DIG) break;
		/* FIXME: find a way to compute optimal sh */
		if (rp > 9+9*LD_B1B_DIG) sh = 9;
		e2 += sh;
		for (k=a; k!=z; k=(k+1 & MASK)) {
			uint32_t tmp = x[k] & (1<<sh)-1;
			x[k] = (x[k]>>sh) + carry;
			carry = (1000000000>>sh) * tmp;
			if (k==a && !x[k]) {
				a = (a+1 & MASK);
				i--;
				rp -= 9;
			}
		}
		if (carry) {
			if ((z+1 & MASK) != a) {
				x[z] = carry;
				z = (z+1 & MASK);
			} else x[z-1 & MASK] |= 1;
		}
	}

	/* Assemble desired bits into floating point variable */
	for (y=i=0; i<LD_B1B_DIG; i++) {
		if ((a+i & MASK)==z) x[(z=(z+1 & MASK))-1] = 0;
		y = 1000000000.0L * y + x[a+i & MASK];
	}

	y *= sign;

	/* Limit precision for denormal results */
	if (bits > LDBL_MANT_DIG+e2-emin) {
		bits = LDBL_MANT_DIG+e2-emin;
		if (bits<0) bits=0;
		denormal = 1;
	}

	/* Calculate bias term to force rounding, move out lower bits */
	if (bits < LDBL_MANT_DIG) {
		bias = copysignl(scalbn(1, 2*LDBL_MANT_DIG-bits-1), y);
		frac = fmodl(y, scalbn(1, LDBL_MANT_DIG-bits));
		y -= frac;
		y += bias;
	}

	/* Process tail of decimal input so it can affect rounding */
	if ((a+i & MASK) != z) {
		uint32_t t = x[a+i & MASK];
		if (t < 500000000 && (t || (a+i+1 & MASK) != z))
			frac += 0.25*sign;
		else if (t > 500000000)
			frac += 0.75*sign;
		else if (t == 500000000) {
			if ((a+i+1 & MASK) == z)
				frac += 0.5*sign;
			else
				frac += 0.75*sign;
		}
		if (LDBL_MANT_DIG-bits >= 2 && !fmodl(frac, 1))
			frac++;
	}

	y += frac;
	y -= bias;

	if ((e2+LDBL_MANT_DIG & INT_MAX) > emax-5) {
		if (fabs(y) >= CONCAT(0x1p, LDBL_MANT_DIG)) {
			if (denormal && bits==LDBL_MANT_DIG+e2-emin)
				denormal = 0;
			y *= 0.5;
			e2++;
		}
		if (e2+LDBL_MANT_DIG>emax || (denormal && frac))
			errno = ERANGE;
	}

	return scalbnl(y, e2);
}

static long double hexfloat(FILE *f, int bits, int emin, int sign, int pok)
{
	uint32_t x = 0;
	long double y = 0;
	long double scale = 1;
	long double bias = 0;
	int gottail = 0, gotrad = 0, gotdig = 0;
	int64_t rp = 0;
	int64_t dc = 0;
	int64_t e2 = 0;
	int d;
	int c;

	c = shgetc(f);

	/* Skip leading zeros */
	for (; c=='0'; c = shgetc(f)) gotdig = 1;

	if (c=='.') {
		gotrad = 1;
		c = shgetc(f);
		/* Count zeros after the radix point before significand */
		for (rp=0; c=='0'; c = shgetc(f), rp--) gotdig = 1;
	}

	for (; c-'0'<10U || (c|32)-'a'<6U || c=='.'; c = shgetc(f)) {
		if (c=='.') {
			if (gotrad) break;
			rp = dc;
			gotrad = 1;
		} else {
			gotdig = 1;
			if (c > '9') d = (c|32)+10-'a';
			else d = c-'0';
			if (dc<8) {
				x = x*16 + d;
			} else if (dc < LDBL_MANT_DIG/4+1) {
				y += d*(scale/=16);
			} else if (d && !gottail) {
				y += 0.5*scale;
				gottail = 1;
			}
			dc++;
		}
	}
	if (!gotdig) {
		shunget(f);
		if (pok) {
			shunget(f);
			if (gotrad) shunget(f);
		} else {
			shlim(f, 0);
		}
		return sign * 0.0;
	}
	if (!gotrad) rp = dc;
	while (dc<8) x *= 16, dc++;
	if ((c|32)=='p') {
		e2 = scanexp(f, pok);
		if (e2 == LLONG_MIN) {
			if (pok) {
				shunget(f);
			} else {
				shlim(f, 0);
				return 0;
			}
			e2 = 0;
		}
	} else {
		shunget(f);
	}
	e2 += 4*rp - 32;

	if (!x) return sign * 0.0;
	if (e2 > -emin) {
		errno = ERANGE;
		return sign * LDBL_MAX * LDBL_MAX;
	}
	if (e2 < emin-2*LDBL_MANT_DIG) {
		errno = ERANGE;
		return sign * LDBL_MIN * LDBL_MIN;
	}

	while (x < 0x80000000) {
		if (y>=0.5) {
			x += x + 1;
			y += y - 1;
		} else {
			x += x;
			y += y;
		}
		e2--;
	}

	if (bits > 32+e2-emin) {
		bits = 32+e2-emin;
		if (bits<0) bits=0;
	}

	if (bits < LDBL_MANT_DIG)
		bias = copysignl(scalbn(1, 32+LDBL_MANT_DIG-bits-1), sign);

	if (bits<32 && y && !(x&1)) x++, y=0;

	y = bias + sign*(long double)x + sign*y;
	y -= bias;

	if (!y) errno = ERANGE;

	return scalbnl(y, e2);
}

long double __floatscan(FILE *f, int prec, int pok)
{
	int sign = 1;
	int i;
	int bits;
	int emin;
	int c;

	switch (prec) {
	case 0:
		bits = FLT_MANT_DIG;
		emin = FLT_MIN_EXP-bits;
		break;
	case 1:
		bits = DBL_MANT_DIG;
		emin = DBL_MIN_EXP-bits;
		break;
	case 2:
		bits = LDBL_MANT_DIG;
		emin = LDBL_MIN_EXP-bits;
		break;
	default:
		return 0;
	}

	while (isspace((c=shgetc(f))));

	if (c=='+' || c=='-') {
		sign -= 2*(c=='-');
		c = shgetc(f);
	}

	for (i=0; i<8 && (c|32)=="infinity"[i]; i++)
		if (i<7) c = shgetc(f);
	if (i==3 || i==8 || (i>3 && pok)) {
		if (i!=8) {
			shunget(f);
			if (pok) for (; i>3; i--) shunget(f);
		}
		return sign * INFINITY;
	}
	if (!i) for (i=0; i<3 && (c|32)=="nan"[i]; i++)
		if (i<2) c = shgetc(f);
	if (i==3) {
		return NAN;
	}

	if (i) {
		shunget(f);
		errno = EINVAL;
		shlim(f, 0);
		return 0;
	}

	if (c=='0') {
		c = shgetc(f);
		if ((c|32) == 'x')
			return hexfloat(f, bits, emin, sign, pok);
		shunget(f);
		c = '0';
	}

	return decfloat(f, c, bits, emin, sign, pok);
}

#endif
