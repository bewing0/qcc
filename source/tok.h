/*  QCC - Tiny C Compiler V2
 * 
 *  Copyright (c) 2015 Bruce Ewing
 *  Licensed under 2-clause BSD, see file LICENSE in this tarball
 */

// Warning: the exact order of tokens 0 to 46 is vital

#define TOK_ILLEGAL		0
#define TOK_NO_OP		1
// note: ENDOFBUF and LINEFILE tokens must coexist with both ASCII and tokens -- so pick the values with a little care
#define TOK_ENDOFBUF	2
#define TOK_LINEFILE	3			// current line number and filename -- for error message processing
#define TOK_O_PAREN		4
// HIHI make another define with the same value as TOK_O_PAREN, to be for the function-like macro kludge bytes?
#define TOK_C_PAREN		5
#define TOK_ASSIGN		6
#define TOK_B_LT		7
#define TOK_B_GT		8
#define TOK_AND			9
#define TOK_OR			10
#define TOK_B_NOT		11
#define TOK_ADD			12
#define TOK_SUB			13
#define TOK_MULT		14
#define TOK_DIV			15
#define TOK_MOD			16
#define TOK_XOR			17
#define TOK_NOT			18
#define TOK_QMARK		19
#define TOK_SQUOTE		20
#define TOK_DQUOTE		21					// doublequotes are invalid in preprocessor expressions, but valid in C
#define TOK_DEFINED		21					// the "defined" keyword is valid in the preprocessor, but not valid in C
#define TOK_OSQUARE		22
#define TOK_CSQUARE		23
#define TOK_OCURLY		24
#define TOK_CCURLY		25
#define TOK_COLON		26
#define TOK_SEMIC		27
#define TOK_COMMA		28
#define TOK_DOT			29
#define TOK_B_EQ		30					// then the two symbol combos that are double chars
#define TOK_SHL			31
#define TOK_SHR			32
#define TOK_B_AND		33
#define TOK_B_OR		34
#define TOK_ARROW		35					// (this one happens to fit in a gap)
#define TOK_INC			36					// ++
#define TOK_DEC			37
#define TOK_B_LE		38					// then combos that end in '='
#define TOK_B_GE		39
#define TOK_ANDEQ		40
#define TOK_OREQ		41
#define TOK_B_NE		42
#define TOK_ADDEQ		43
#define TOK_SUBEQ		44
#define TOK_MULEQ		45
#define TOK_DIVEQ		46
#define TOK_MODEQ		47
#define TOK_XOREQ		48
// tokens after this point are associated with keywords, in the same order, in an array
#define TOK_BOOL_T		49
#define TOK_CHAR_T		50
#define TOK_DBL_T		51
#define TOK_FLOAT_T		52
#define TOK_INT_T		53
#define TOK_LONG_T		54
#define TOK_SHORT_T		55
#define TOK_SGND_T		56
#define TOK_UNSGN_T		57
#define TOK_QWORD_T		58
#define KEYWORDS_OFF			TOK_BOOL_T
#define TOK_TYPESPEC_START		TOK_BOOL_T			// all typespecs must be together
#define TOK_TYPESPEC_END		TOK_QWORD_T
#define TOK_SIZEOF		59
#define NUM_PP_TOKENS	TOK_SIZEOF
// only the above tokens are used in the preprocessor
#define TOK_OFFSET		60
#define TOK_COUNT		61
#define TOK_TYPEDEF		62
// additional keywords associated with varaible and function declarations
#define TOK_CONST		63
#define TOK_REGISTER	64
#define TOK_RESTRICT	65
#define TOK_STATIC		66
#define TOK_STRUCT		67			// struct, union, and enum must be sequential
#define TOK_UNION		68
#define TOK_ENUM		69
#define TOK_VOID		70
#define TOK_VOLATILE	71
// all calling conventions must be in this group, too!
// fastcall, cdecl, stdcall, dllexport?
#define VAR_KEYW_START	TOK_CONST
#define VAR_KEYW_END	TOK_VOLATILE
#define TOK_ALLOCA		72
#define TOK_ASM			73
#define TOK_BREAK		74
#define TOK_CASE		75
#define TOK_CONTINUE	76
#define TOK_DEFAULT		77
#define TOK_DO			78
#define TOK_ELSE		79
#define TOK_EXTERN		80
#define TOK_FOR			81
#define TOK_GOTO		82
#define TOK_IF			83
#define TOK_INLINE		84
#define TOK_RETURN		85
#define TOK_SWITCH		86
#define TOK_WHILE		87

#define TOK_NOIDX_OP	89			// a token that used to point to a name, and is now a no-op

// HIHI whem I'm certain that this is all of them, pack these final tokens down with the others
// -- definitely need a few more operators though -- addressof, deref, ellipsis, others???
#define TOK_FUNCT_PTR	111			// HIHI actually a function pointer must be a typespec?
// one more idx type? global variable declarations?
#define TOK_FN_PROT_IDX	113
#define TOK_ASTRUCT_IDX	114			// anonymous struct/union/enum tokens
#define TOK_AUNION_IDX	115
#define TOK_AENUM_IDX	116
#define TOK_STRUCT_IDX	117			// named struct/union/enums
#define TOK_UNION_IDX	118
#define TOK_ENUM_IDX	119
#define TOK_FUNCT_IDX	120			// function name index
#define TOK_TYPEDEF_IDX	121
#define TOK_NONAME_IDX	122			// for structs etc., where the name is left blank
#define TOK_NAME_IDX	123			// for strings, types, names, etc.
#define TOK_INT_CONST	124			// integer constant
#define TOK_FP_CONST	125			// floating point constant
#define FIRST_IDX_TOK	TOK_FUNCT_PTR + 1

static const char *keywords [] =
{
	"_Bool",
	"char",
	"double",
	"float",
	"int",
	"long",
	"short",
	"signed",
	"unsigned",
	"__int64",
	"sizeof",
	"offsetof",
	"countof",
	"typedef",
	"const",
	"register",
	"restrict",
	"static",
	"struct",
	"union",
	"enum",
	"void",
	"volatile",
	"alloca",
	"asm",
	"break",
	"case",
	"continue",
	"default",
	"do",
	"else",
	"extern",
	"for",
	"goto",
	"if",
	"inline",
	"return",
	"switch",
	"while"
};




// more keywords?
//	DEF(TOK_TYPEOF1, "typeof")
//	DEF(TOK_UNUSED1, "unused")
//	DEF(TOK_NORETURN1, "noreturn")
