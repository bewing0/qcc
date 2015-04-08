// Warning: the exact order of tokens 0 to 46 is vital

#define TOK_ILLEGAL		0
#define TOK_NO_OP		1
#define TOK_O_PAREN		2
#define TOK_C_PAREN		3
#define TOK_ASSIGN		4
#define TOK_B_LT		5
#define TOK_B_GT		6
#define TOK_AND			7
#define TOK_OR			8
#define TOK_B_NOT		9
#define TOK_ADD			10
#define TOK_SUB			11
#define TOK_MULT		12
#define TOK_DIV			13
#define TOK_MOD			14
#define TOK_XOR			15
#define TOK_NOT			16
#define TOK_QMARK		17
#define TOK_SQUOTE		18
#define TOK_DQUOTE		19					// doublequotes are invalid in preprocessor expressions, but valid in C
#define TOK_DEFINED		19					// the "defined" keyword is valid in the preprocessor, but not valid in C
#define TOK_OSQUARE		20
#define TOK_CSQUARE		21
#define TOK_OCURLY		22
#define TOK_CCURLY		23
#define TOK_COLON		24
#define TOK_SEMIC		25
#define TOK_COMMA		26
#define TOK_DOT			27
#define TOK_B_EQ		28					// then the two symbol combos that are double chars
#define TOK_SHL			29
#define TOK_SHR			30
#define TOK_B_AND		31
#define TOK_B_OR		32
#define TOK_ARROW		33					// (this one happens to fit in a gap)
#define TOK_INC			34					// ++
#define TOK_DEC			35
#define TOK_B_LE		36					// then combos that end in '='
#define TOK_B_GE		37
#define TOK_ANDEQ		38
#define TOK_OREQ		39
#define TOK_B_NE		40
#define TOK_ADDEQ		41
#define TOK_SUBEQ		42
#define TOK_MULEQ		43
#define TOK_DIVEQ		44
#define TOK_MODEQ		45
#define TOK_XOREQ		46
// tokens after this point are associated with keywords, in the same order, in an array
#define KEYWORDS_OFF	47
#define TOK_TYPESPEC_START		47			// all typespecs must be together
#define TOK_BOOL_T		47
#define TOK_CHAR_T		48
#define TOK_DBL_T		49
#define TOK_FLOAT_T		50
#define TOK_INT_T		51
#define TOK_LONG_T		52
#define TOK_SHORT_T		53
#define TOK_SGND_T		54
#define TOK_UNSGN_T		55
#define TOK_QWORD_T		56
#define TOK_TYPESPEC_END		56
#define TOK_SIZEOF		57
// only the above tokens are used in the preprocessor
#define TOK_OFFSET		58
#define TOK_COUNT		59
#define TOK_TYPEDEF		60
#define VAR_KEYW_START	61		// additional keywords associated with varaible and function declarations
#define TOK_CONST		61
#define TOK_ENUM		62
#define TOK_REGISTER	63
#define TOK_RESTRICT	64
#define TOK_STATIC		65
#define TOK_STRUCT		66
#define TOK_UNION		67
#define TOK_VOID		68
#define TOK_VOLATILE	69
// all calling conventions must be in this group, too!
// fastcall, cdecl, stdcall, dllexport?
#define VAR_KEYW_END	69
#define TOK_ALLOCA		70
#define TOK_ASM			71
#define TOK_BREAK		72
#define TOK_CASE		73
#define TOK_CONTINUE	74
#define TOK_DEFAULT		75
#define TOK_DO			76
#define TOK_ELSE		77
#define TOK_EXTERN		78
#define TOK_FOR			79
#define TOK_GOTO		80
#define TOK_IF			81
#define TOK_INLINE		82
#define TOK_RETURN		83
#define TOK_SWITCH		84
#define TOK_WHILE		85

#define TOK_NONAME_IDX	122			// for structs etc., where the name is left blank
#define TOK_NAME_IDX	123			// for strings, types, names, etc.
#define TOK_INT_CONST	124			// integer constant
#define TOK_FP_CONST	125			// floating point constant
#define TOK_NL			126			// single newline -- to count line numbers, for display only
#define TOK_RLL_NL		127			// run of newlines (followed by the run length count - 3)
// note: the ENDOFBUF and LINEFILE tokens must coexist with both ASCII and tokens, so their value must be >= 127
#define TOK_ENDOFBUF	128
#define TOK_LINEFILE	129			// current line number and filename -- for error message processing

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
	"enum",
	"register",
	"restrict",
	"static",
	"struct",
	"union",
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
