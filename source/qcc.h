/*
 *
 *  Copyright (c) 2001-2004 Fabrice Bellard
 *  Copyright (c) 2006-2007 Rob Landley
 *  Copyright (c) 2015 Bruce Ewing
 *
 *  Licensed under 2-clause BSD, see file LICENSE in this tarball
 */

#include "config.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

#ifndef WIN32
#include <stdint.h>
// #include <sys/time.h>	-- this was copied from TCC, but it doesn't exist in musl! -- if GCC needs it, then it needs its own ifdef?
//#include <sys/mman.h>
#endif




// HIHI!! many of these tccg things are single bits -- put those into a single flag_bits with masks?
// uint8_t host_bigendian;
int tccg_warn_unsupported, tccg_warn_write_strings, tccg_warn_error, tccg_warn_implicit_function_declaration;
int tccg_char_is_unsigned, tccg_nocommon, tccg_leading_underscore, tccg_verbose;

// HIHI!! are these 6 also single bit flags that should be in qccg_flag_bits?
int multiple_files;
int print_search_dirs;
int reloc_output;

//int do_bounds_check = 0;
int do_debug = 0;
int do_bench = 0;

// #########################


// token_info is an array of packed token information
// token_num_map maps a token index in the array to a unique token number -- malloced as a local variable in the tokenizing routine!
// both arrays are sorted first on hash, and second on token length
// contains type (2 bits), len (6 bits), and str offset (24 bits) (len is at the bottom)
//uint32_t *token_info;

//uint32_t hash_to_tokid[256], num_toks, cur_tokid;

uint8_t *wrksp, *wrksp_top, *name_strings, host_bigendian, *emit_ptr;
uint8_t stoppers[128], prep_src[128], prep_ops[256], alnum_[256], c_ops[256], whtsp_lkup[128];
int8_t hex_lkup[256], hexout[16];
uint32_t wrk_size, wrk_rem, wrk_used_base, namestr_len, nxt_pass_info[4];
uint16_t total_errs, total_warns, max_names_per_hash;


int32_t da_entry_count[7], da_tot_entrylen[7];
uint8_t *da_buffers[7];
char *outfile, *cur_fname;

uint64_t *cint_tbl;			// array for storage of all constant integer values
uint64_t *cman_tbl;			// storage for all constant floating mantissas
uint16_t *cexp_tbl;			// exponents
uint32_t *idx_tbl;			// indexes into all the other arrays
uint32_t idxidx;			// index into the idx table
uint32_t noname_cnt;		// number of anonymous structs/unions/enums
uint32_t int_cnt;			// entries in the cint_tbl
uint32_t flt_cnt;			// entries in cman/cexp tables

char inout_fnames[2][64];			// filenames for 2 temporary files
int8_t iof_in_toggle;				// index of the inout_fname that is INPUT
int8_t outf_exists;
int outfd;

// Benchmark info
int32_t total_lines;
int32_t total_bytes;
int32_t tok_ident;				// HIHI!! is this a total token count? (including operators & keywords, I think?) -- I already have num_toks.


// Note on Boolean implementation:
// It is typical to have boolean false be 0, and boolean true be 1 "or anything else that's not 0".
// This "true" value is a huge mistake. Then the "!" operator requires a conditional test, which is
// bad in any implementation. It also is a separate, special operator.
// You can get a significant computational improvement if you set the "true" value to -1 exclusively.
// Then the boolean "!" operator becomes identical to the bitwise ~ operator, so there are no conditionals.

#define BOOL_TRUE		-1

// for masking casts
const uint64_t size_mask[3] =
{
	0xff,
	0xffff,
	0xffffffff
};

uint8_t m1cstr[4] = "-1";		// string for default defines

struct pass_info
{
	uint8_t *fname;
	uint32_t line_num;
	int infd;
};



// the preprocessor needs a limit on string constants, macros, and macro inputs
// -- and it must be less than half the chunk size (30k) of read_compressed
#define MAX_MACRO_STRING	10240

// buffer types
#define INCLUDE_PATHS		0
#define SOURCE_FNAMES		1
#define LIB_FNAMES			2
#define OBJ_FNAMES			3
#define LIB_PATHS			4
#define PREDEFINES			5
#define PRE_UNDEFS			6
// do I also need to save the output filename?

#define WD_ALL 0x0001		/* warning is activated when using -Wall */
#define FD_INVERT 0x0002	/* invert value before storing */


#define QCC_ERR_FNOTFOUND	42
#define QCC_ERR_ILLCHAR		1


#define TOK_TYPE_MASK		0xc0		// may be adding one more bit?
#define TOK_LEN_MASK		0x3f
#define TOK_STROFF_SHFT		8

//#define TOK_TYPE_DEFINE		0		// HIHI there is an open "type" here now -- defines are no longer tokens
#define TOK_TYPE_LABEL		0x40
#define TOK_TYPE_STRUCT		0x80
#define TOK_TYPE_IDENT		0xc0

#define TWENTY2EXP7			0x94ace180
#define SIMHASH_INCLUDE		0x1c3d5ed6
#define SIMHASH_DEFINE		0x016a5bdf
#define SIMHASH_IF			0x76
#define SIMHASH_IFDEF		0x00128bd2
#define SIMHASH_IFNDEF		0x019812f2
#define SIMHASH_ERROR		0x003f2bfe
#define SIMHASH_ENDIF		0x001332de
#define SIMHASH_ELIF		0xe00e
#define SIMHASH_WARNING		0x2ca95ed2
#define SIMHASH_ELSE		0xc95e
#define SIMHASH_LINE		0xbfaf
#define SIMHASH_UNDEF		0x00128c8e
#define SIMHASH_PRAGMA		0x002bdfd5
#define SIMHASH_VA			0x15
#define SIMHASH_FILE		0xbbe1
#define SIMHASH_DATE		0xca4f
#define SIMHASH_TIME		0xbdd3
#define SIMHASH_FUNCTIO		0x61560231
#define SIMHASH_VAARGS		0x59fb24d


// definitions of nxt_pass_info subscripts -- in the preprocessor
#define PP_ALNUM_SIZE		0			// total length of all the alphanumeric strings
#define PP_STRING_CNT		1			// total count of alphanumeric strings
#define PP_BIG_NUMS			2			// total count of numeric constants with "big" values
#define PP_NUM_CNT			3			// total count of numeric constants


struct Sym {
	uint32_t blah;			// HIHI!! getting rid of this? Is it how I'll be keeping track of types, register numbers, etc?
};



typedef struct FlagDef {
 int *var;
 uint16_t flags;
 char *name;
} FlagDef;


static FlagDef warning_defs[] = {
 { &tccg_warn_unsupported, 0, "unsupported" },
 { &tccg_warn_write_strings, 0, "write-strings" },
 { &tccg_warn_error, 0, "error" },
 { &tccg_warn_implicit_function_declaration, WD_ALL,
   "implicit-function-declaration" },
};

static FlagDef flag_defs[] = {
 { &tccg_char_is_unsigned, 0, "unsigned-char" },
 { &tccg_char_is_unsigned, FD_INVERT, "signed-char" },
 { &tccg_nocommon, FD_INVERT, "common" },
 { &tccg_leading_underscore, 0, "leading-underscore" },
};


#define TCC_OPTION_HAS_ARG 0x0001
#define TCC_OPTION_NOSEP   0x0002		// MUST not have space between option and arg

typedef struct TCCOption {
 char *name;
 uint16_t index;
 uint16_t flags;
} TCCOption;

enum {
 TCC_OPTION_HELP,
 TCC_OPTION_I,
 TCC_OPTION_D,
 TCC_OPTION_E,
 TCC_OPTION_U,
 TCC_OPTION_L,
 TCC_OPTION_B,
 TCC_OPTION_l,
 TCC_OPTION_bench,
 TCC_OPTION_b,
 TCC_OPTION_g,
 TCC_OPTION_c,
 TCC_OPTION_static,
 TCC_OPTION_shared,
 TCC_OPTION_o,
 TCC_OPTION_r,
 TCC_OPTION_Wl,
 TCC_OPTION_W,
 TCC_OPTION_O,
 TCC_OPTION_m,
 TCC_OPTION_f,
 TCC_OPTION_nostdinc,
 TCC_OPTION_nostdlib,
 TCC_OPTION_print_search_dirs,
 TCC_OPTION_rdynamic,
 TCC_OPTION_run,
 TCC_OPTION_v,
 TCC_OPTION_w,
 TCC_OPTION_pipe,
};


