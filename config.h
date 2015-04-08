// for efficiency, the compiler should be built for a specific target CPU

#define QCC_TARGET_I586

//#define QCC_TARGET_TCG
// if the tcg target is selected, then it needs a sub-target of its own:
//#define TCG_TARGET_I686
//#define TCG_TARGET_Ix86_64
//#define TCG_TARGET_ARM

// select the target operating system's native executable/object format

#define QCC_FORMAT_ELF

// to create a "cross compiler" requires building a new compiler executable,
// with different choices for target CPU and file format


// XXX: allow some defines in here to switch on compiler options by default, such as -fomit_frame_pointer??


#define MAX_NESTED_INCLUDES		10

#ifdef _WIN32
// stdint support:
// if a compiler does not provide a good copy of stdint.h, provide it here
typedef signed char			int8_t;
typedef unsigned char		uint8_t;
typedef signed short		int16_t;
typedef unsigned short		uint16_t;
typedef signed long			int32_t;
typedef unsigned long		uint32_t;
typedef signed __int64		int64_t;
typedef unsigned __int64	uint64_t;
#endif
// also need a known typedef for 80 bit floats
typedef long double			ldbl_t;

// also need widechars?
typedef unsigned short		qcc_wchar;

// this is a list of directories, ending in /, 0-separated, containing system include files <*.h>
// -- maybe it would be nicer to have this in a .rc file or something, but for now this is convenient as a kludge
#define SYS_INCLUDE_PATHS	"c:/ccrap/brewc/brewc/include/\0"
