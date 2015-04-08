/*
 *  TCC - Tiny C Compiler
 * 
 *  Copyright (c) 2001-2004 Fabrice Bellard
 *  Copyright (c) 2006-2007 Rob Landley
 *
 *  Licensed under 2-clause BSD, see file LICENSE in this tarball
 */

#include "tinycc.h"
#include "tcc.h"

// This stuff is used by the code generation backend.

static int gen_ind; /* output code index */
static int loc; /* local variable index */
static Section *cur_text_section; /* current section where function code is generated */
static SValue *vtop;
static Section *symtab_section;
static CType func_vt; /* current function return type (used by return instruction) */
static int func_vc;
static Section *lbounds_section; /* contains local data bound description */
// Predefined types
static CType char_pointer_type;
static CType func_old_type;

/* compile with built-in memory and bounds checker */
static int do_bounds_check = 0;

#ifdef TINYCC_TARGET_I386
#include "i386/gen.c"
#endif

//#ifdef TINYCC_TARGET_ARM			added by patch 295 Glockner  OYOY
//#include "arm/gen.c"
//#endif

//#ifdef TINYCC_TARGET_C67			added by patch 305 TK  OYOY
//#include "c67-gen.c"
//#endif

/* parser */
static struct BufferedFile *file;
static int fch, tok;
static CValue tokc;
static CString tokcstr; /* current parsed string, if any */
/* additional informations about token */
static int tok_flags, next_tok_flags;

static int *macro_ptr, *macro_ptr_allocated;
static int *unget_saved_macro_ptr;
static int unget_saved_buffer[TOK_MAX_SIZE + 1];
static int unget_buffer_enabled;
static int parse_flags;
static Section *text_section, *data_section, *bss_section; /* predefined sections */
#ifdef CONFIG_TCC_ASM
static Section *last_text_section; /* to handle .previous asm directive */
#endif

/* bound check related sections */
static Section *bounds_section; /* contains global data bound description */
/* symbol sections */
static Section *strtab_section;

/* debug sections */
static Section *stab_section, *stabstr_section;

/*
   rsym: return symbol
   anon_sym: anonymous symbol index
*/
static int rsym, anon_sym;
/* expression generation modifiers */
static int const_wanted; /* true if constant wanted */
static int global_expr;  /* true if compound literals must be allocated
                            globally (used during initializers parsing */
static int last_line_num, last_ind, func_ind; /* debug last line number and pc */
static int tok_ident;
static TokenSym **table_ident;
static TokenSym *hash_ident[TOK_HASH_SIZE];
static char token_buf[STRING_MAX_SIZE + 1];
static char *funcname;
static Sym *global_stack, *local_stack;
static Sym *define_stack;
static Sym *global_label_stack, *local_label_stack;
/* symbol allocator */
static Sym *sym_free_first;

static SValue vstack[VSTACK_SIZE];
/* some predefined types */
static CType int_type;
/* Whether each char is a C identifier and/or number */
static unsigned char isidnum_table[256];

/* compile with debug symbol (and use them if error during execution) */
extern int do_debug;

/* display benchmark infos */
static int total_lines;
static int total_bytes;

/* use GNU C extensions */
static int gnu_ext = 1;

/* use Tiny C extensions */
static int tcc_ext = 1;

/* XXX: get rid of this ASAP */
static struct TCCState *tcc_state;

/* give the path of the compiler's libraries */
char *tinycc_path;


/********************************************************/

/* we use our own 'finite' function to avoid potential problems with
   non standard math libs */
/* XXX: endianness dependent */
int ieee_finite(double d)
{
    int *p = (int *)&d;
    return ((unsigned)((p[1] | 0x800fffff) + 1)) >> 31;
}

/* copy a string and truncate it. */
char *pstrcpy(char *buf, int buf_size, char *s)
{
    char *q, *q_end;
    int c;

    if (buf_size > 0) {
        q = buf;
        q_end = buf + buf_size - 1;
        while (q < q_end) {
            c = *s++;
            *q++ = c;
            if (!c) break;
        }
        *q = '\0';
    }
    return buf;
}

/* strcat and truncate. */
static char *pstrcat(char *buf, int buf_size, char *s)
{
    int len;
    len = strlen(buf);
    if (len < buf_size) 
        pstrcpy(buf + len, buf_size - len, s);
    return buf;
}

// If str starts with val, return 1 and move ptr right after val in str.
// Otherwise return 0
int strstart(char *str, char *val, char **ptr)
{
    while (*val) {
        if (*str != *val) return 0;
        str++;
        val++;
    }
    if (ptr) *ptr = str;
    return 1;
}

void *xmalloc(unsigned long size)
{
    void *ptr = malloc(size);
    if (!ptr && size) error("memory full");
    return ptr;
}

static void *xzmalloc(unsigned long size)
{
    void *ptr = xmalloc(size);
    memset(ptr, 0, size);
    return ptr;
}

static inline void *xrealloc(void *ptr, unsigned long size)
{
    void *ptr1 = realloc(ptr, size);
    if (!ptr1 && size) error("memory full");
    return ptr1;
}

void dynarray_add(void ***ptab, int *nb_ptr, void *data)
{
    int nb, nb_alloc;
    void **pp;
    
    nb = *nb_ptr;
    pp = *ptab;
    /* every power of two we double array size */
    if ((nb & (nb - 1)) == 0) {
        if (!nb)
            nb_alloc = 1;
        else
            nb_alloc = nb * 2;
        pp = xrealloc(pp, nb_alloc * sizeof(void *));
        *ptab = pp;
    }
    pp[nb++] = data;
    *nb_ptr = nb;
}

/* Allocate a batch of new symbol structures. */
static Sym *__sym_malloc(void)
{
    Sym *sym_pool, *sym, *last_sym;
    int i;

    sym_pool = xmalloc(SYM_POOL_NB * sizeof(Sym));

    last_sym = sym_free_first;
    sym = sym_pool;
    for(i = 0; i < SYM_POOL_NB; i++) {
        sym->next = last_sym;
        last_sym = sym;
        sym++;
    }
    sym_free_first = last_sym;
    return last_sym;
}

// Allocate a symbol structure by popping most recently freed one off a linked
// list, and xmalloc() a bunch of new ones at once if the list is empty.
static inline Sym *sym_malloc(void)
{
    Sym *sym;
    sym = sym_free_first;
    if (!sym)
        sym = __sym_malloc();
    sym_free_first = sym->next;
    return sym;
}

// Push symbol onto the free list.

static inline void sym_free(Sym *sym)
{
    sym->next = sym_free_first;
    sym_free_first = sym;
}

Section *new_section(TCCState *s1, char *name, int sh_type, int sh_flags)
{
    Section *sec;

    sec = xzmalloc(sizeof(Section) + strlen(name));
    strcpy(sec->name, name);
    sec->sh_type = sh_type;
    sec->sh_flags = sh_flags;
    switch(sh_type) {
    case SHT_HASH:
    case SHT_REL:
    case SHT_DYNSYM:
    case SHT_SYMTAB:
    case SHT_DYNAMIC:
        sec->sh_addralign = 4;
        break;
    case SHT_STRTAB:
        sec->sh_addralign = 1;
        break;
    default:
        sec->sh_addralign = 32; /* default conservative alignment */
        break;
    }

    /* only add section if not private */
    if (!(sh_flags & SHF_PRIVATE)) {
        sec->sh_num = s1->nb_sections;
        dynarray_add((void ***)&s1->sections, &s1->nb_sections, sec);
    }
    return sec;
}

static void free_section(Section *s)
{
    free(s->data);
    free(s);
}

/* realloc section and set its content to zero */
static void section_realloc(Section *sec, unsigned long new_size)
{
    unsigned long size;
    unsigned char *data;
    
    size = sec->data_allocated;
    if (size == 0)
        size = 1;
    while (size < new_size)
        size = size * 2;
    data = xrealloc(sec->data, size);
    memset(data + sec->data_allocated, 0, size - sec->data_allocated);
    sec->data = data;
    sec->data_allocated = size;
}

/* reserve at least 'size' bytes in section 'sec' from
   sec->data_offset. */
static void *section_ptr_add(Section *sec, unsigned long size)
{
    unsigned long offset, offset1;

    offset = sec->data_offset;
    offset1 = offset + size;
    if (offset1 > sec->data_allocated)
        section_realloc(sec, offset1);
    sec->data_offset = offset1;
    return sec->data + offset;
}

/* return a reference to a section, and create it if it does not
   exists */
Section *find_section(TCCState *s1, char *name)
{
    Section *sec;
    int i;
    for(i = 1; i < s1->nb_sections; i++) {
        sec = s1->sections[i];
        if (!strcmp(name, sec->name)) 
            return sec;
    }
    /* sections are created as PROGBITS */
    return new_section(s1, name, SHT_PROGBITS, SHF_ALLOC);
}

#define SECTION_ABS ((void *)1)

/* update sym->c so that it points to an external symbol in section
   'section' with value 'value' */
static void put_extern_sym2(Sym *sym, Section *section, 
                            unsigned long value, unsigned long size,
                            int can_add_underscore)
{
    int sym_type, sym_bind, sh_num, info;
    Elf32_Sym *esym;
    char *name;
    char buf1[256];

    if (!section) sh_num = SHN_UNDEF;
    else if (section == SECTION_ABS) sh_num = SHN_ABS;
    else sh_num = section->sh_num;

    if (!sym->c) {
        if ((sym->type.t & VT_BTYPE) == VT_FUNC) sym_type = STT_FUNC;
        else sym_type = STT_OBJECT;

        if (sym->type.t & VT_STATIC) sym_bind = STB_LOCAL;
        else sym_bind = STB_GLOBAL;
        
        name = get_tok_str(sym->token, NULL);
#ifdef CONFIG_TCC_BCHECK
        if (do_bounds_check) {
            char buf[32];

            /* XXX: avoid doing that for statics ? */
            /* if bound checking is activated, we change some function
               names by adding the "__bound" prefix */
            switch(sym->token) {
#if 0
            /* XXX: we rely only on malloc hooks */
            case TOK_malloc: 
            case TOK_free: 
            case TOK_realloc: 
            case TOK_memalign: 
            case TOK_calloc: 
#endif
            case TOK_memcpy: 
            case TOK_memmove:
            case TOK_memset:
            case TOK_strlen:
            case TOK_strcpy:
            case TOK_alloca:
                strcpy(buf, "__bound_");
                strcat(buf, name);
                name = buf;
                break;
            }
        }
#endif
        if (tccg_leading_underscore && can_add_underscore) {
            buf1[0] = '_';
            pstrcpy(buf1 + 1, sizeof(buf1) - 1, name);
            name = buf1;
        }
        info = ELF32_ST_INFO(sym_bind, sym_type);
        sym->c = add_elf_sym(symtab_section, value, size, info, 0, sh_num, name);		// the "other" arg (symbol visibility = 0) was added by patch 354 Grischka  OYOY
    } else {
        esym = &((Elf32_Sym *)symtab_section->data)[sym->c];
        esym->st_value = value;
        esym->st_size = size;
        esym->st_shndx = sh_num;
    }
}

static void put_extern_sym(Sym *sym, Section *section, 
                           unsigned long value, unsigned long size)
{
    put_extern_sym2(sym, section, value, size, 1);
}

/* add a new relocation entry to symbol 'sym' in section 's' */
static void greloc(Section *s, Sym *sym, unsigned long offset, int type)
{
    if (!sym->c) 
        put_extern_sym(sym, NULL, 0, 0);
    /* now we can add ELF relocation info */
    put_elf_reloc(symtab_section, s, offset, type, sym->c);
}

static inline int isnum(int c)
{
    return c >= '0' && c <= '9';
}

static inline int toup(int c)
{
    if (c >= 'a' && c <= 'z')
        return c - 'a' + 'A';
    else
        return c;
}

static void strcat_vprintf(char *buf, int buf_size, char *fmt, va_list ap)
{
    int len;
    len = strlen(buf);
    vsnprintf(buf + len, buf_size - len, fmt, ap);
}

static void strcat_printf(char *buf, int buf_size, char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    strcat_vprintf(buf, buf_size, fmt, ap);
    va_end(ap);
}

void error1(TCCState *s1, int is_warning, char *fmt, va_list ap)
{
    char buf[2048];
    BufferedFile **f;
    
    buf[0] = '\0';
    if (file) {
        for(f = s1->include_stack; f < s1->include_stack_ptr; f++)
            strcat_printf(buf, sizeof(buf), "In file included from %s:%d:\n", 
                          (*f)->filename, (*f)->line_num);
        if (file->line_num > 0) {
            strcat_printf(buf, sizeof(buf), 
                          "%s:%d: ", file->filename, file->line_num);
        } else {
            strcat_printf(buf, sizeof(buf),
                          "%s: ", file->filename);
        }
    } else {
        strcat_printf(buf, sizeof(buf),
                      "tcc: ");
    }
    if (is_warning)
        strcat_printf(buf, sizeof(buf), "warning: ");
    strcat_vprintf(buf, sizeof(buf), fmt, ap);

    if (!s1->error_func) {
        /* default case: stderr */
        fprintf(stderr, "%s\n", buf);
    } else {
        s1->error_func(s1->error_opaque, buf);
    }
    if (!is_warning || tccg_warn_error)
        s1->nb_errors++;
}

#ifdef LIBTCC
void tcc_set_error_func(TCCState *s, void *error_opaque,
                        void (*error_func)(void *opaque, char *msg))
{
    s->error_opaque = error_opaque;
    s->error_func = error_func;
}
#endif

/* error without aborting current compilation */
void error_noabort(char *fmt, ...)
{
    TCCState *s1 = tcc_state;
    va_list ap;

    va_start(ap, fmt);
    error1(s1, 0, fmt, ap);
    va_end(ap);
}

void error(char *fmt, ...)
{
    TCCState *s1 = tcc_state;
    va_list ap;

    va_start(ap, fmt);
    error1(s1, 0, fmt, ap);
    va_end(ap);
    /* better than nothing: in some cases, we accept to handle errors */
    if (s1->error_set_jmp_enabled) {
        longjmp(s1->error_jmp_buf, 1);
    } else {
        /* XXX: eliminate this someday */
        exit(1);
    }
}

void expect(char *msg)
{
    error("%s expected", msg);
}

void warning(char *fmt, ...)
{
    TCCState *s1 = tcc_state;
    va_list ap;

//    if (tccg_warn_none)		this variable and logic was added in patch 285 -- romain francoise  OYOY
//        return;				also added in patch 285

    va_start(ap, fmt);
    error1(s1, 1, fmt, ap);
    va_end(ap);
}

void skip(int c)
{
    if (tok != c)
        error("'%c' expected", c);
    next();
}

static void test_lvalue(void)
{
    if (!(vtop->r & VT_LVAL))
        expect("lvalue");
}

/* allocate a new token */
static TokenSym *tok_alloc_new(TokenSym **pts, char *str, int len)
{
    TokenSym *ts, **ptable;
    int i;

    if (tok_ident >= SYM_FIRST_ANOM) 
        error("memory full");

    /* expand token table if needed */
    i = tok_ident - TOK_IDENT;
    if ((i % TOK_ALLOC_INCR) == 0) {
        ptable = xrealloc(table_ident, (i + TOK_ALLOC_INCR) * sizeof(TokenSym *));
        table_ident = ptable;
    }

    ts = xmalloc(sizeof(TokenSym) + len);
    table_ident[i] = ts;
    ts->tok = tok_ident++;
    ts->sym_define = NULL;
    ts->sym_label = NULL;
    ts->sym_struct = NULL;
    ts->sym_identifier = NULL;
    ts->len = len;
    ts->hash_next = NULL;
    memcpy(ts->str, str, len);
    ts->str[len] = '\0';
    *pts = ts;
    return ts;
}

#define TOK_HASH_INIT 1
#define TOK_HASH_FUNC(h, c) ((h) * 263 + (c))

/* find a token and add it if not found */
static TokenSym *tok_alloc(char *str, int len)
{
    TokenSym *ts, **pts;
    int i;
    unsigned int h;
    
    h = TOK_HASH_INIT;
    for(i=0;i<len;i++)
        h = TOK_HASH_FUNC(h, ((unsigned char *)str)[i]);
    h &= (TOK_HASH_SIZE - 1);

    pts = &hash_ident[h];
    for(;;) {
        ts = *pts;
        if (!ts)
            break;
        if (ts->len == len && !memcmp(ts->str, str, len))
            return ts;
        pts = &(ts->hash_next);
    }
    return tok_alloc_new(pts, str, len);
}

/* CString handling */

static void cstr_realloc(CString *cstr, int new_size)
{
    int size;
    void *data;

    size = cstr->size_allocated;
    if (size == 0)
        size = 8; /* no need to allocate a too small first string */
    while (size < new_size)
        size = size * 2;
    data = xrealloc(cstr->data_allocated, size);
    cstr->data_allocated = data;
    cstr->size_allocated = size;
    cstr->data = data;
}

/* add a byte */
static inline void cstr_ccat(CString *cstr, int ch)
{
    int size;
    size = cstr->size + 1;
    if (size > cstr->size_allocated)
        cstr_realloc(cstr, size);
    ((unsigned char *)cstr->data)[size - 1] = ch;
    cstr->size = size;
}

static void cstr_cat(CString *cstr, char *str)
{
    int c;
    for(;;) {
        c = *str;
        if (c == '\0')
            break;
        cstr_ccat(cstr, c);
        str++;
    }
}

/* add a wide char */
static void cstr_wccat(CString *cstr, int ch)
{
    int size;
    size = cstr->size + sizeof(int);			// original code
//    size = cstr->size + sizeof(nwchar_t);		modded by patch 384 Navara  OYOY
    if (size > cstr->size_allocated)
        cstr_realloc(cstr, size);
    *(int *)(((unsigned char *)cstr->data) + size - sizeof(int)) = ch;				// original code
//    *(nwchar_t *)(((unsigned char *)cstr->data) + size - sizeof(nwchar_t)) = ch;		modded by patch 384 Navara  OYOY
    cstr->size = size;
}

static void cstr_new(CString *cstr)
{
    memset(cstr, 0, sizeof(CString));
}

/* free string and reset it to NULL */
static void cstr_free(CString *cstr)
{
    free(cstr->data_allocated);
    cstr_new(cstr);
}

#define cstr_reset(cstr) cstr_free(cstr)

/* XXX: unicode ? */
static void add_char(CString *cstr, int c)
{
    if (c == '\'' || c == '\"' || c == '\\') {
        /* XXX: could be more precise if char or string */
        cstr_ccat(cstr, '\\');
    }
    if (c >= 32 && c <= 126) {
        cstr_ccat(cstr, c);
    } else {
        cstr_ccat(cstr, '\\');
        if (c == '\n') {
            cstr_ccat(cstr, 'n');
        } else {
            cstr_ccat(cstr, '0' + ((c >> 6) & 7));
            cstr_ccat(cstr, '0' + ((c >> 3) & 7));
            cstr_ccat(cstr, '0' + (c & 7));
        }
    }
}

/* XXX: buffer overflow */
/* XXX: float tokens */
char *get_tok_str(int v, CValue *cv)
{
    static char buf[STRING_MAX_SIZE + 1];
    static CString cstr_buf;
    CString *cstr;
    unsigned char *q;
    char *p;
    int i, len;

    /* NOTE: to go faster, we give a fixed buffer for small strings */
    cstr_reset(&cstr_buf);
    cstr_buf.data = buf;
    cstr_buf.size_allocated = sizeof(buf);
    p = buf;

    switch(v) {
    case TOK_CINT:
    case TOK_CUINT:
        /* XXX: not quite exact, but only useful for testing */
        sprintf(p, "%u", cv->ui);
        break;
    case TOK_CLLONG:
    case TOK_CULLONG:
        /* XXX: not quite exact, but only useful for testing  */
        sprintf(p, "%Lu", cv->ull);
        break;
    case TOK_CCHAR:
    case TOK_LCHAR:
        cstr_ccat(&cstr_buf, '\'');
        add_char(&cstr_buf, cv->i);
        cstr_ccat(&cstr_buf, '\'');
        cstr_ccat(&cstr_buf, '\0');
        break;
    case TOK_PPNUM:
        cstr = cv->cstr;
        len = cstr->size - 1;
        for(i=0;i<len;i++)
            add_char(&cstr_buf, ((unsigned char *)cstr->data)[i]);
        cstr_ccat(&cstr_buf, '\0');
        break;
    case TOK_STR:
    case TOK_LSTR:
        cstr = cv->cstr;
        cstr_ccat(&cstr_buf, '\"');
        if (v == TOK_STR) {
            len = cstr->size - 1;
            for(i=0;i<len;i++)
                add_char(&cstr_buf, ((unsigned char *)cstr->data)[i]);
        } else {
            len = (cstr->size / sizeof(int)) - 1;				// original code
//            len = (cstr->size / sizeof(nwchar_t)) - 1;		modded by patch 384 Navara  OYOY
            for(i=0;i<len;i++)
                add_char(&cstr_buf, ((int *)cstr->data)[i]);	// original code
//                add_char(&cstr_buf, ((nwchar_t *)cstr->data)[i]);		modded by patch 384 Navara  OYOY
        }
        cstr_ccat(&cstr_buf, '\"');
        cstr_ccat(&cstr_buf, '\0');
        break;
    case TOK_LT:
        v = '<';
        goto addv;
    case TOK_GT:
        v = '>';
        goto addv;
    case TOK_DOTS:
        return strcpy(p, "...");
    case TOK_A_SHL:
        return strcpy(p, "<<=");
    case TOK_A_SAR:
        return strcpy(p, ">>=");
    default:
        if (v < TOK_IDENT) {
            /* search in two bytes table */
            q = tok_two_chars;
            while (*q) {
                if (q[2] == v) {
                    *p++ = q[0];
                    *p++ = q[1];
                    *p = '\0';
                    return buf;
                }
                q += 3;
            }
        addv:
            *p++ = v;
            *p = '\0';
        } else if (v < tok_ident) {
            return table_ident[v - TOK_IDENT]->str;
        } else if (v >= SYM_FIRST_ANOM) {
            /* special name for anonymous symbol */
            sprintf(p, "L.%u", v - SYM_FIRST_ANOM);
        } else {
            /* should never happen */
            return NULL;
        }
        break;
    }
    return cstr_buf.data;
}

/* push, without hashing */
static Sym *sym_push2(Sym **ps, int token, int t, long c)
{
    Sym *s;
    s = sym_malloc();
    s->token = token;
    s->type.t = t;
    s->c = c;
    s->next = NULL;
    /* add in stack */
    s->prev = *ps;
    *ps = s;
    return s;
}

/* find a symbol and return its associated structure. 's' is the top
   of the symbol stack */
static Sym *sym_find2(Sym *s, int token)
{
    while (s) {
        if (s->token == token)
            return s;
        s = s->prev;
    }
    return NULL;
}

/* structure lookup */
static inline Sym *struct_find(int v)
{
    v -= TOK_IDENT;
    if ((unsigned)v >= (unsigned)(tok_ident - TOK_IDENT))
        return NULL;
    return table_ident[v]->sym_struct;
}

/* find an identifier */
static inline Sym *sym_find(int v)
{
    v -= TOK_IDENT;
    if ((unsigned)v >= (unsigned)(tok_ident - TOK_IDENT))
        return NULL;
    return table_ident[v]->sym_identifier;
}

/* push a given symbol on the symbol stack */
static Sym *sym_push(int v, CType *type, int r, int c)
{
    Sym *s, **ps;
    TokenSym *ts;

    if (local_stack)
        ps = &local_stack;
    else
        ps = &global_stack;
    s = sym_push2(ps, v, type->t, c);
    s->type.ref = type->ref;
    s->r = r;
    /* don't record fields or anonymous symbols */
    /* XXX: simplify */
    if (!(v & SYM_FIELD) && (v & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
        /* record symbol in token array */
        ts = table_ident[(v & ~SYM_STRUCT) - TOK_IDENT];
        if (v & SYM_STRUCT)
            ps = &ts->sym_struct;
        else
            ps = &ts->sym_identifier;
        s->prev_tok = *ps;
        *ps = s;
    }
    return s;
}

/* push a global identifier */
static Sym *global_identifier_push(int v, int t, int c)
{
    Sym *s, **ps;
    s = sym_push2(&global_stack, v, t, c);
    /* don't record anonymous symbol */
    if (v < SYM_FIRST_ANOM) {
        ps = &table_ident[v - TOK_IDENT]->sym_identifier;
        /* modify the top most local identifier, so that
           sym_identifier will point to 's' when popped */
        while (*ps != NULL)
            ps = &(*ps)->prev_tok;
        s->prev_tok = NULL;
        *ps = s;
    }
    return s;
}

/* pop symbols until top reaches 'b' */
static void sym_pop(Sym **ptop, Sym *b)
{
    Sym *s, *ss, **ps;
    TokenSym *ts;
    int token;

    s = *ptop;
    while(s != b) {
        ss = s->prev;
        token = s->token;
        /* remove symbol in token array */
        /* XXX: simplify */
        if (!(token & SYM_FIELD) && (token & ~SYM_STRUCT) < SYM_FIRST_ANOM) {
            ts = table_ident[(token & ~SYM_STRUCT) - TOK_IDENT];
            if (token & SYM_STRUCT)
                ps = &ts->sym_struct;
            else
                ps = &ts->sym_identifier;
            *ps = s->prev_tok;
        }
        sym_free(s);
        s = ss;
    }
    *ptop = b;
}

/* I/O layer */

BufferedFile *tcc_open(TCCState *s1, char *filename)
{
    int fd;
    BufferedFile *bf;
//    int i, len;					added by patch 386 Navara  OYOY

    if (!filename) fd = 0;
    else fd = open(filename, O_RDONLY | O_BINARY);
    if (fd < 0)
        return NULL;
    bf = xmalloc(sizeof(BufferedFile));
    bf->fd = fd;
    bf->buf_ptr = bf->buffer;
    bf->buf_end = bf->buffer;
    bf->buffer[0] = CH_EOB; /* put eob symbol */
    pstrcpy(bf->filename, sizeof(bf->filename),
        filename ? filename : "*stdin*");
//    len = strlen(bf->filename);					4 lines added by patch 386 Navara  OYOY
//    for (i = 0; i < len; i++)
//        if (bf->filename[i] == '\\')
//            bf->filename[i] = '/';
    bf->line_num = 1;
    bf->ifndef_macro = 0;
    bf->ifdef_stack_ptr = s1->ifdef_stack_ptr;

    return bf;
}

void tcc_close(BufferedFile *bf)
{
    total_lines += bf->line_num;
    close(bf->fd);
    free(bf);
}

/* fill input buffer and peek next char */
static int tcc_peekc_slow(BufferedFile *bf)
{
    int len;
    /* only tries to read if really end of buffer */
    if (bf->buf_ptr >= bf->buf_end) {
        if (bf->fd != -1) {
#if defined(PARSE_DEBUG)
            len = 8;
#else
            len = IO_BUF_SIZE;
#endif
            len = read(bf->fd, bf->buffer, len);
            if (len < 0)
                len = 0;
        } else {
            len = 0;
        }
        total_bytes += len;
        bf->buf_ptr = bf->buffer;
        bf->buf_end = bf->buffer + len;
        *bf->buf_end = CH_EOB;
    }
    if (bf->buf_ptr < bf->buf_end) {
        return bf->buf_ptr[0];
    } else {
        bf->buf_ptr = bf->buf_end;
        return CH_EOF;
    }
}

/* return the current character, handling end of block if necessary
   (but not stray) */
static int handle_eob(void)
{
    return tcc_peekc_slow(file);
}

/* read next char from current input file and handle end of input buffer */
static inline void inp(void)
{
    fch = *(++(file->buf_ptr));
    /* end of buffer/file handling */
    if (fch == CH_EOB)
        fch = handle_eob();
}

/* space excluding newline */
int is_space(int ch)
{
    return strchr(" \t\v\f\r", ch) ? 1 : 0;
}

/* handle '\[\r]\n' */
static int handle_stray_noerror(void)
{
    while (fch == '\\') {
        inp();
        while (is_space(fch)) inp();
        if (fch == '\n') {
            file->line_num++;
            inp();
        } else return 1;
    }
    return 0;
}

static void handle_stray(void)
{
    if(handle_stray_noerror())
        error("stray '\\' in program");
}

/* skip the stray and handle the \\n case. Output an error if
   incorrect char after the stray */
static int handle_stray1(uint8_t *p)
{
    int c;

    if (p >= file->buf_end) {
        file->buf_ptr = p;
        c = handle_eob();
        p = file->buf_ptr;
        if (c == '\\')
            goto parse_stray;
    } else {
    parse_stray:
        file->buf_ptr = p;
        fch = *p;
        handle_stray();
        p = file->buf_ptr;
        c = *p;
    }
    return c;
}

/* handle just the EOB case, but not stray */
#define PEEKC_EOB(c, p)\
{\
    p++;\
    c = *p;\
    if (c == '\\') {\
        file->buf_ptr = p;\
        c = handle_eob();\
        p = file->buf_ptr;\
    }\
}

/* handle the complicated stray case */
#define PEEKC(c, p)\
{\
    p++;\
    c = *p;\
    if (c == '\\') {\
        c = handle_stray1(p);\
        p = file->buf_ptr;\
    }\
}

/* input with '\[\r]\n' handling. Note that this function cannot
   handle other characters after '\', so you cannot call it inside
   strings or comments */
static void minp(void)
{
    inp();
    if (fch == '\\') 
        handle_stray();
}


/* single line C++ comments */
static uint8_t *parse_line_comment(uint8_t *p)
{
    int c;

    p++;
    for(;;) {
        c = *p;
    redo:
        if (c == '\n' || c == CH_EOF) {
            break;
        } else if (c == '\\') {
            file->buf_ptr = p;
            c = handle_eob();
            p = file->buf_ptr;
            if (c == '\\') {
                PEEKC_EOB(c, p);
                if (c == '\n') {
                    file->line_num++;
                    PEEKC_EOB(c, p);
                } else if (c == '\r') {
                    PEEKC_EOB(c, p);
                    if (c == '\n') {
                        file->line_num++;
                        PEEKC_EOB(c, p);
                    }
                }
            } else {
                goto redo;
            }
        } else {
            p++;
        }
    }
    return p;
}

/* C comments */
static uint8_t *parse_comment(uint8_t *p)
{
    int c;
    
    p++;
    for(;;) {
        /* fast skip loop */
        for(;;) {
            c = *p;
            if (c == '\n' || c == '*' || c == '\\')
                break;
            p++;
            c = *p;
            if (c == '\n' || c == '*' || c == '\\')
                break;
            p++;
        }
        /* now we can handle all the cases */
        if (c == '\n') {
            file->line_num++;
            p++;
        } else if (c == '*') {
            p++;
            for(;;) {
                c = *p;
                if (c == '*') {
                    p++;
                } else if (c == '/') {
                    goto end_of_comment;
                } else if (c == '\\') {
                    file->buf_ptr = p;
                    c = handle_eob();
                    p = file->buf_ptr;
                    if (c == '\\') {
                        /* skip '\[\r]\n', otherwise just skip the stray */
                        while (c == '\\') {
                            PEEKC_EOB(c, p);
                            if (c == '\n') {
                                file->line_num++;
                                PEEKC_EOB(c, p);
                            } else if (c == '\r') {
                                PEEKC_EOB(c, p);
                                if (c == '\n') {
                                    file->line_num++;
                                    PEEKC_EOB(c, p);
                                }
                            } else {
                                goto after_star;
                            }
                        }
                    }
                } else {
                    break;
                }
            }
        after_star: ;
        } else {
            /* stray, eob or eof */
            file->buf_ptr = p;
            c = handle_eob();
            p = file->buf_ptr;
            if (c == CH_EOF) {
                error("unexpected end of file in comment");
            } else if (c == '\\') {
                p++;
            }
        }
    }
 end_of_comment:
    p++;
    return p;
}

#define cinp minp

static inline void skip_spaces(void)
{
    while (is_space(fch))
        cinp();
}

/* parse a string without interpreting escapes */
static uint8_t *parse_pp_string(uint8_t *p,
                                int sep, CString *str)
{
    int c;
    p++;
    for(;;) {
        c = *p;
        if (c == sep) {
            break;
        } else if (c == '\\') {
            file->buf_ptr = p;
            c = handle_eob();
            p = file->buf_ptr;
            if (c == CH_EOF) {
            unterminated_string:
                /* XXX: indicate line number of start of string */
                error("missing terminating %c character", sep);
            } else if (c == '\\') {
                /* escape : just skip \[\r]\n */
                PEEKC_EOB(c, p);
                if (c == '\n') {
                    file->line_num++;
                    p++;
                } else if (c == '\r') {
                    PEEKC_EOB(c, p);
                    if (c != '\n')
                        expect("'\n' after '\r'");
                    file->line_num++;
                    p++;
                } else if (c == CH_EOF) {
                    goto unterminated_string;
                } else {
                    if (str) {
                        cstr_ccat(str, '\\');
                        cstr_ccat(str, c);
                    }
                    p++;
                }
            }
        } else if (c == '\n') {
            file->line_num++;
            goto add_char;
        } else if (c == '\r') {
            PEEKC_EOB(c, p);
            if (c != '\n') {
                if (str)
                    cstr_ccat(str, '\r');
            } else {
                file->line_num++;
                goto add_char;
            }
        } else {
        add_char:
            if (str)
                cstr_ccat(str, c);
            p++;
        }
    }
    p++;
    return p;
}

/* skip block of text until #else, #elif or #endif. skip also pairs of
   #if/#endif */
void preprocess_skip(void)
{
    int a, start_of_line, c;
    uint8_t *p;

    p = file->buf_ptr;
    start_of_line = 1;
    a = 0;
    for(;;) {
    redo_no_start:
        c = *p;
        switch(c) {
        case ' ':
        case '\t':
        case '\f':
        case '\v':
        case '\r':
            p++;
            goto redo_no_start;
        case '\n':
            start_of_line = 1;
            file->line_num++;
            p++;
            goto redo_no_start;
        case '\\':
            file->buf_ptr = p;
            c = handle_eob();
            if (c == CH_EOF) {
                expect("#endif");
            } else if (c == '\\') {
                fch = file->buf_ptr[0];
                handle_stray_noerror();
            }
            p = file->buf_ptr;
            goto redo_no_start;
            /* skip strings */
        case '\"':
        case '\'':
            p = parse_pp_string(p, c, NULL);
            break;
            /* skip comments */
        case '/':
            file->buf_ptr = p;
            fch = *p;
            minp();
            p = file->buf_ptr;
            if (fch == '*') {
                p = parse_comment(p);
            } else if (fch == '/') {
                p = parse_line_comment(p);
            }
            break;

        case '#':
            p++;
            if (start_of_line) {
                file->buf_ptr = p;
                next_nomacro();
                p = file->buf_ptr;
                if (a == 0 && 
                    (tok == TOK_ELSE || tok == TOK_ELIF || tok == TOK_ENDIF))
                    goto the_end;
                if (tok == TOK_IF || tok == TOK_IFDEF || tok == TOK_IFNDEF)
                    a++;
                else if (tok == TOK_ENDIF)
                    a--;
            }
            break;
        default:
            p++;
            break;
        }
        start_of_line = 0;
    }
 the_end: ;
    file->buf_ptr = p;
}

/* ParseState handling */

/* XXX: currently, no include file info is stored. Thus, we cannot display
   accurate messages if the function or data definition spans multiple
   files */

/* save current parse state in 's' */
void save_parse_state(ParseState *s)
{
    s->line_num = file->line_num;
    s->macro_ptr = macro_ptr;
    s->tok = tok;
    s->tokc = tokc;
}

/* restore parse state from 's' */
void restore_parse_state(ParseState *s)
{
    file->line_num = s->line_num;
    macro_ptr = s->macro_ptr;
    tok = s->tok;
    tokc = s->tokc;
}

/* return the number of additional 'ints' necessary to store the
   token */
static inline int tok_ext_size(int t)
{
    switch(t) {
        /* 4 bytes */
    case TOK_CINT:
    case TOK_CUINT:
    case TOK_CCHAR:
    case TOK_LCHAR:
    case TOK_CFLOAT:
    case TOK_LINENUM:
        return 1;
    case TOK_STR:
    case TOK_LSTR:
    case TOK_PPNUM:
        error("unsupported token");
        return 1;
    case TOK_CDOUBLE:
    case TOK_CLLONG:
    case TOK_CULLONG:
        return 2;
    case TOK_CLDOUBLE:
        return LDOUBLE_SIZE / 4;
    default:
        return 0;
    }
}

/* token string handling */

static inline void tok_str_new(TokenString *s)
{
    s->str = NULL;
    s->len = 0;
    s->allocated_len = 0;
    s->last_line_num = -1;
}

static void tok_str_free(int *str)
{
    free(str);
}

static int *tok_str_realloc(TokenString *s)
{
    int *str, len;

    if (s->allocated_len == 0) {
        len = 8;
    } else {
        len = s->allocated_len * 2;
    }
    str = xrealloc(s->str, len * sizeof(int));
    s->allocated_len = len;
    s->str = str;
    return str;
}

static void tok_str_add(TokenString *s, int t, int tf)
{
    int len, *str;

    len = s->len;
    str = s->str;
    if (len+1 >= s->allocated_len)
        str = tok_str_realloc(s);
    str[len++] = t;
    str[len++] = tf;
    s->len = len;
}

static void tok_str_add2(TokenString *s, int t, int tf, CValue *cv)
{
    int len, *str;

    len = s->len;
    str = s->str;

    /* allocate space for worst case */
    if (len + TOK_MAX_SIZE > s->allocated_len)
        str = tok_str_realloc(s);
    str[len++] = t;
    str[len++] = tf;
    switch(t) {
    case TOK_CINT:
    case TOK_CUINT:
    case TOK_CCHAR:
    case TOK_LCHAR:
    case TOK_CFLOAT:
    case TOK_LINENUM:
        str[len++] = cv->tab[0];
        break;
    case TOK_PPNUM:
    case TOK_STR:
    case TOK_LSTR:
        {
            int nb_words;
            CString *cstr;

            nb_words = (sizeof(CString) + cv->cstr->size + 3) >> 2;
            while ((len + nb_words) > s->allocated_len)
                str = tok_str_realloc(s);
            cstr = (CString *)(str + len);
            cstr->data = NULL;
            cstr->size = cv->cstr->size;
            cstr->data_allocated = NULL;
            cstr->size_allocated = cstr->size;
            memcpy((char *)cstr + sizeof(CString), 
                   cv->cstr->data, cstr->size);
            len += nb_words;
        }
        break;
    case TOK_CDOUBLE:
    case TOK_CLLONG:
    case TOK_CULLONG:
//#if LDOUBLE_SIZE == 8					3 lines added by patch 295 Glockner  OYOY
//    case TOK_CLDOUBLE:
//#endif
        str[len++] = cv->tab[0];
        str[len++] = cv->tab[1];
        break;
// #if LDOUBLE_SIZE == 12				moves the line 2 below in patch 295 Glockner  OYOY
    case TOK_CLDOUBLE:
#if LDOUBLE_SIZE == 12					// original code
        str[len++] = cv->tab[0];
        str[len++] = cv->tab[1];
        str[len++] = cv->tab[2];
#else									// original code
//#elif LDOUBLE_SIZE != 8				replaces the above line in patch 295 Glockner  OYOY
#error add long double size support
#endif
        break;
    default:
        break;
    }
    s->len = len;
}

/* add the current parse token in token string 's' */
static void tok_str_add_tok(TokenString *s)
{
    CValue cval;

    /* save line number info */
    if (file->line_num != s->last_line_num) {
        s->last_line_num = file->line_num;
        cval.i = s->last_line_num;
        tok_str_add2(s, TOK_LINENUM, 0, &cval);
    }
    tok_str_add2(s, tok, tok_flags, &tokc);
}

#if LDOUBLE_SIZE == 12
#define LDOUBLE_GET(p, cv)                      \
        cv.tab[0] = p[0];                       \
        cv.tab[1] = p[1];                       \
        cv.tab[2] = p[2];
//#elif LDOUBLE_SIZE == 8					4 lines added in patch 295 Glockner  OYOY
//#define LDOUBLE_GET(p, cv)                      \
//        cv.tab[0] = p[0];                       \
//        cv.tab[1] = p[1];
#else
#error add long double size support
#endif


/* get a token from an integer array and increment pointer
   accordingly. we code it as a macro to avoid pointer aliasing. */
#define TOK_GET(t, tf, p, cv)                   \
{                                               \
    t = *p++;                                   \
    tf = *p++;                                  \
    switch(t) {                                 \
    case TOK_CINT:                              \
    case TOK_CUINT:                             \
    case TOK_CCHAR:                             \
    case TOK_LCHAR:                             \
    case TOK_CFLOAT:                            \
    case TOK_LINENUM:                           \
        cv.tab[0] = *p++;                       \
        break;                                  \
    case TOK_STR:                               \
    case TOK_LSTR:                              \
    case TOK_PPNUM:                             \
        cv.cstr = (CString *)p;                 \
        cv.cstr->data = (char *)p + sizeof(CString);\
        p += (sizeof(CString) + cv.cstr->size + 3) >> 2;\
        break;                                  \
    case TOK_CDOUBLE:                           \
    case TOK_CLLONG:                            \
    case TOK_CULLONG:                           \
        cv.tab[0] = p[0];                       \
        cv.tab[1] = p[1];                       \
        p += 2;                                 \
        break;                                  \
    case TOK_CLDOUBLE:                          \
        LDOUBLE_GET(p, cv);                     \
        p += LDOUBLE_SIZE / 4;                  \
        break;                                  \
    default:                                    \
        break;                                  \
    }                                           \
}

/* defines handling */
static inline void define_push(int token, int macro_type, int *str, Sym *first_arg)
{
    Sym *s;

    s = sym_push2(&define_stack, token, macro_type, (long)str);
    s->next = first_arg;
    table_ident[token - TOK_IDENT]->sym_define = s;
}

/* undefined a define symbol. Its name is just set to zero */
static void define_undef(Sym *s)
{
    int token;
    token = s->token;
    if (token >= TOK_IDENT && token < tok_ident)
        table_ident[token - TOK_IDENT]->sym_define = NULL;
    s->token = 0;
}

static inline Sym *define_find(int token)
{
    token -= TOK_IDENT;
    if ((unsigned)token >= (unsigned)(tok_ident - TOK_IDENT))
        return NULL;
    return table_ident[token]->sym_define;
}

/* free define stack until top reaches 'b' */
static void free_defines(Sym *b)
{
    Sym *top, *top1;
    int token;

    top = define_stack;
    while (top != b) {
        top1 = top->prev;
        /* do not free args or predefined defines */
        if (top->c)
            tok_str_free((int *)top->c);
        token = top->token;
        if (token >= TOK_IDENT && token < tok_ident)
            table_ident[token - TOK_IDENT]->sym_define = NULL;
        sym_free(top);
        top = top1;
    }
    define_stack = b;
}

/* label lookup */
static Sym *label_find(int token)
{
    token -= TOK_IDENT;
    if ((unsigned)token >= (unsigned)(tok_ident - TOK_IDENT))
        return NULL;
    return table_ident[token]->sym_label;
}

static Sym *label_push(Sym **ptop, int token, int flags)
{
    Sym *s, **ps;
    s = sym_push2(ptop, token, 0, 0);
    s->r = flags;
    ps = &table_ident[token - TOK_IDENT]->sym_label;
    if (ptop == &global_label_stack) {
        /* modify the top most local identifier, so that
           sym_identifier will point to 's' when popped */
        while (*ps != NULL)
            ps = &(*ps)->prev_tok;
    }
    s->prev_tok = *ps;
    *ps = s;
    return s;
}

/* pop labels until element last is reached. Look if any labels are
   undefined. Define symbols if '&&label' was used. */
static void label_pop(Sym **ptop, Sym *slast)
{
    Sym *s, *s1;
    for(s = *ptop; s != slast; s = s1) {
        s1 = s->prev;
        if (s->r == LABEL_DECLARED) {
            warning("label '%s' declared but not used",
                get_tok_str(s->token, NULL));
        } else if (s->r == LABEL_FORWARD) {
                error("label '%s' used but not defined",
                      get_tok_str(s->token, NULL));
        } else {
            if (s->c) {
                /* define corresponding symbol. A size of 1 is put. */
                put_extern_sym(s, cur_text_section, (long)s->next, 1);
            }
        }
        /* remove label */
        table_ident[s->token - TOK_IDENT]->sym_label = s->prev_tok;
        sym_free(s);
    }
    *ptop = slast;
}

/* eval an expression for #if/#elif */
static int expr_preprocess(void)
{
    int c, t;
    TokenString str;
    
    tok_str_new(&str);
    while (tok != TOK_LINEFEED && tok != TOK_EOF) {
        next(); /* do macro subst */
        if (tok == TOK_DEFINED) {
            next_nomacro();
            t = tok;
            if (t == '(') 
                next_nomacro();
            c = define_find(tok) != 0;
            if (t == '(')
                next_nomacro();
            tok = TOK_CINT;
            tokc.i = c;
        } else if (tok >= TOK_IDENT) {
            /* if undefined macro */
            tok = TOK_CINT;
            tokc.i = 0;
        }
        tok_str_add_tok(&str);
    }
    tok_str_add(&str, -1, 0); /* simulate end of file */
    tok_str_add(&str, 0, 0);
    /* now evaluate C constant expression */
    macro_ptr = str.str;
    next();
    c = expr_const();
    macro_ptr = NULL;
    tok_str_free(str.str);
    return c != 0;
}

#if defined(PARSE_DEBUG) || defined(PP_DEBUG)
static void tok_print(int *str)
{
    int t, tf;
    CValue cval;

    while (1) {
        TOK_GET(t, tf, str, cval);
        if (!t)
            break;
        printf(" %s", get_tok_str(t, &cval));
    }
    printf("\n");
}
#endif

/* parse after #define */
static void parse_define(void)
{
    Sym *s, *first, **ps;
    int v, t, varg, is_vaargs, c;
    TokenString str;
    
    v = tok;
    if (v < TOK_IDENT)
        error("invalid macro name '%s'", get_tok_str(tok, &tokc));
    /* XXX: should check if same macro (ANSI) */
    first = NULL;
    t = MACRO_OBJ;
    /* '(' must be just after macro definition for MACRO_FUNC */
    c = file->buf_ptr[0];
    if (c == '\\')
        c = handle_stray1(file->buf_ptr);
    if (c == '(') {
        next_nomacro();
        next_nomacro();
        ps = &first;
        while (tok != ')') {
            varg = tok;
            next_nomacro();
            is_vaargs = 0;
            if (varg == TOK_DOTS) {
                varg = TOK___VA_ARGS__;
                is_vaargs = 1;
            } else if (tok == TOK_DOTS && gnu_ext) {
                is_vaargs = 1;
                next_nomacro();
            }
            if (varg < TOK_IDENT)
                error("badly punctuated parameter list");
            s = sym_push2(&define_stack, varg | SYM_FIELD, is_vaargs, 0);
            *ps = s;
            ps = &s->next;
            if (tok != ',')
                break;
            next_nomacro();
        }
        t = MACRO_FUNC;
    }
    tok_str_new(&str);
    next_nomacro();
    /* EOF testing necessary for '-D' handling */
    while (tok != TOK_LINEFEED && tok != TOK_EOF) {
        tok_str_add2(&str, tok, tok_flags, &tokc);
        next_nomacro();
    }
    tok_str_add(&str, 0, 0);
#ifdef PP_DEBUG
    printf("define %s %d: ", get_tok_str(v, NULL), t);
    tok_print(str.str);
#endif
    define_push(v, t, str.str, first);
}

static inline int hash_cached_include(int type, char *filename)
{
    uint8_t *s;
    unsigned int h;

    h = TOK_HASH_INIT;
    h = TOK_HASH_FUNC(h, type);
    s = (uint8_t *)filename;
    while (*s) {
        h = TOK_HASH_FUNC(h, *s);
        s++;
    }
    h &= (CACHED_INCLUDES_HASH_SIZE - 1);
    return h;
}

/* XXX: use a token or a hash table to accelerate matching ? */
static CachedInclude *search_cached_include(TCCState *s1,
                                            int type, char *filename)
{
    CachedInclude *e;
    int i, h;
    h = hash_cached_include(type, filename);
    i = s1->cached_includes_hash[h];
    for(;;) {
        if (i == 0)
            break;
        e = s1->cached_includes[i - 1];
        if (e->type == type && !strcmp(e->filename, filename))
            return e;
        i = e->hash_next;
    }
    return NULL;
}

static inline void add_cached_include(TCCState *s1, int type, 
                                      char *filename, int ifndef_macro)
{
    CachedInclude *e;
    int h;

    if (search_cached_include(s1, type, filename))
        return;
#ifdef INC_DEBUG
    printf("adding cached '%s' %s\n", filename, get_tok_str(ifndef_macro, NULL));
#endif
    e = xmalloc(sizeof(CachedInclude) + strlen(filename));
    e->type = type;
    strcpy(e->filename, filename);
    e->ifndef_macro = ifndef_macro;
    dynarray_add((void ***)&s1->cached_includes, &s1->nb_cached_includes, e);
    /* add in hash table */
    h = hash_cached_include(type, filename);
    e->hash_next = s1->cached_includes_hash[h];
    s1->cached_includes_hash[h] = s1->nb_cached_includes;
}

/*
static void pragma_parse(TCCState *s1)				this entire function was added by patch 354 Grischka  OYOY
{
    int val;

    next();
    if (tok == TOK_pack) {

//          This may be:
//          #pragma pack(1) // set
//          #pragma pack() // reset to default
//          #pragma pack(push,1) // push & set
//          #pragma pack(pop) // restore previous

	    next();
        skip('(');
        if (tok == TOK_ASM_pop) {
            next();
            if (s1->pack_stack_ptr <= s1->pack_stack) {
            stk_error:
                error("out of pack stack");
            }
            s1->pack_stack_ptr--;
        } else {
            val = 0;
            if (tok != ')') {
                if (tok == TOK_ASM_push) {
                    next();
                    if (s1->pack_stack_ptr >= s1->pack_stack + PACK_STACK_SIZE - 1)
                        goto stk_error;
                    s1->pack_stack_ptr++;
                    skip(',');
                }
                if (tok != TOK_CINT) {
                pack_error:
                    error("invalid pack pragma");
                }
                val = tokc.i;
                if (val < 1 || val > 16 || (val & (val - 1)) != 0)
                    goto pack_error;
                next();
            }
            *s1->pack_stack_ptr = val;
            skip(')');
        }
    }
}
*/

/* is_bof is true if first non space token at beginning of file */
static void preprocess(int is_bof)
{
    TCCState *s1 = tcc_state;
    int size, i, c, n, saved_parse_flags;
    char buf[1024], *q, *p;
    char buf1[1024];
    BufferedFile *f;
    Sym *s;
    CachedInclude *e;
    
    saved_parse_flags = parse_flags;
    parse_flags = PARSE_FLAG_PREPROCESS | PARSE_FLAG_TOK_NUM | 
        PARSE_FLAG_LINEFEED;
    next_nomacro();
 redo:
    switch(tok) {
    case TOK_DEFINE:
        next_nomacro();
        parse_define();
        break;
    case TOK_UNDEF:
        next_nomacro();
        s = define_find(tok);
        /* undefine symbol by putting an invalid name */
        if (s)
            define_undef(s);
        break;
    case TOK_INCLUDE:
//    case TOK_INCLUDE_NEXT:			added in patch 352 (Bernhard Fischer)  OYOY
        fch = file->buf_ptr[0];
        /* XXX: incorrect if comments : use next_nomacro with a special mode */
        skip_spaces();
        if (fch == '<') {
            c = '>';
            goto read_name;
        } else if (fch == '\"') {
            c = fch;
        read_name:
            /* XXX: better stray handling */
            minp();
            q = buf;
            while (fch != c && fch != '\n' && fch != CH_EOF) {
                if ((q - buf) < sizeof(buf) - 1)
                    *q++ = fch;
                minp();
            }
            *q = '\0';
            minp();
#if 0
            /* eat all spaces and comments after include */
            /* XXX: slightly incorrect */
            while (ch1 != '\n' && ch1 != CH_EOF)
                inp();
#endif
        } else {
            /* computed #include : either we have only strings or
               we have anything enclosed in '<>' */
            next();
            buf[0] = '\0';
            if (tok == TOK_STR) {
                while (tok != TOK_LINEFEED) {
                    if (tok != TOK_STR) {
                    include_syntax:
                        error("'#include' expects \"FILENAME\" or <FILENAME>");
                    }
                    pstrcat(buf, sizeof(buf), (char *)tokc.cstr->data);
                    next();
                }
                c = '\"';
            } else {
                int len;
                while (tok != TOK_LINEFEED) {
                    pstrcat(buf, sizeof(buf), get_tok_str(tok, &tokc));
                    next();
                }
                len = strlen(buf);
                /* check syntax and remove '<>' */
                if (len < 2 || buf[0] != '<' || buf[len - 1] != '>')
                    goto include_syntax;
                memmove(buf, buf + 1, len - 2);
                buf[len - 2] = '\0';
                c = '>';
            }
        }

        e = search_cached_include(s1, c, buf);
        if (e && define_find(e->ifndef_macro)) {
            /* no need to parse the include because the 'ifndef macro'
               is defined */
#ifdef INC_DEBUG
            printf("%s: skipping %s\n", file->filename, buf);
#endif
        } else {
            if (c == '\"') {
                /* first search in current dir if "header.h" */
                size = 0;
                p = strrchr(file->filename, '/');
                if (p) 
                    size = p + 1 - file->filename;
                if (size > sizeof(buf1) - 1)
                    size = sizeof(buf1) - 1;
                memcpy(buf1, file->filename, size);
                buf1[size] = '\0';
                pstrcat(buf1, sizeof(buf1), buf);
                f = tcc_open(s1, buf1);
                if (f) {
//                    if (tok == TOK_INCLUDE_NEXT)			3 lines added in patch 352 (Bernhard Fischer)  OYOY
//                        tok = TOK_INCLUDE;
//                    else
                        goto found;
                }
            }
            if (s1->include_stack_ptr >= s1->include_stack + INCLUDE_STACK_SIZE)
                error("#include recursion too deep");
            /* now search in all the include paths */
            n = tccg_include_paths.len + s1->sysinclude_paths.len;
            for(i = 0; i < n; i++) {
                char *path;
                int verbose = tccg_verbose;

                verbose -= (s1->include_stack_ptr != s1->include_stack);
                if (i < tccg_include_paths.len)
                    path = tccg_include_paths.data[i];
                else
                    path = s1->sysinclude_paths.data[i - tccg_include_paths.len];
                pstrcpy(buf1, sizeof(buf1), path);
                pstrcat(buf1, sizeof(buf1), "/");
                pstrcat(buf1, sizeof(buf1), buf);
                f = tcc_open(s1, buf1);
                if (verbose > !f)
                    printf("#%s '%s'\n", f ? "include" : "checked", buf1);
                if (f) {
//                    if (tok == TOK_INCLUDE_NEXT)			3 lines added in patch 352 (Bernhard Fischer)  OYOY
//                        tok = TOK_INCLUDE;
//                    else
                        goto found;
                }
            }
            error("include file '%s' not found", buf);
            f = NULL;
        found:
#ifdef INC_DEBUG
            printf("%s: including %s\n", file->filename, buf1);
#endif
            f->inc_type = c;
            pstrcpy(f->inc_filename, sizeof(f->inc_filename), buf);
            /* push current file in stack */
            /* XXX: fix current line init */
            *s1->include_stack_ptr++ = file;
            file = f;
            /* add include file debug info */
            if (do_debug) {
                put_stabs(file->filename, N_BINCL, 0, 0, 0);
            }
            next_tok_flags |= TOK_FLAG_BOW | TOK_FLAG_BOF | TOK_FLAG_BOL;
            fch = file->buf_ptr[0];
            goto the_end;
        }
        break;
    case TOK_IFNDEF:
        c = 1;
        goto do_ifdef;
    case TOK_IF:
        c = expr_preprocess();
        goto do_if;
    case TOK_IFDEF:
        c = 0;
    do_ifdef:
        next_nomacro();
        if (tok < TOK_IDENT)
            error("invalid argument for '#if%sdef'", c ? "n" : "");
        if (is_bof) {
            if (c) {
#ifdef INC_DEBUG
                printf("#ifndef %s\n", get_tok_str(tok, NULL));
#endif
                file->ifndef_macro = tok;
            }
        }
        c = (define_find(tok) != 0) ^ c;
    do_if:
        if (s1->ifdef_stack_ptr >= s1->ifdef_stack + IFDEF_STACK_SIZE)
            error("memory full");
        *s1->ifdef_stack_ptr++ = c;
        goto test_skip;
    case TOK_ELSE:
        if (s1->ifdef_stack_ptr == s1->ifdef_stack)
            error("#else without matching #if");
        if (s1->ifdef_stack_ptr[-1] & 2)
            error("#else after #else");
        c = (s1->ifdef_stack_ptr[-1] ^= 3);
        goto test_skip;
    case TOK_ELIF:
        if (s1->ifdef_stack_ptr == s1->ifdef_stack)
            error("#elif without matching #if");
        c = s1->ifdef_stack_ptr[-1];
        if (c > 1)
            error("#elif after #else");
        /* last #if/#elif expression was true: we skip */
        if (c == 1)
            goto skip;
        c = expr_preprocess();
        s1->ifdef_stack_ptr[-1] = c;
    test_skip:
        if (!(c & 1)) {
        skip:
            preprocess_skip();
            is_bof = 0;
            goto redo;
        }
        break;
    case TOK_ENDIF:
        if (s1->ifdef_stack_ptr <= file->ifdef_stack_ptr)
            error("#endif without matching #if");
        s1->ifdef_stack_ptr--;
        /* '#ifndef macro' was at the start of file. Now we check if
           an '#endif' is exactly at the end of file */
        if (file->ifndef_macro &&
            s1->ifdef_stack_ptr == file->ifdef_stack_ptr) {
            file->ifndef_macro_saved = file->ifndef_macro;
            /* need to set to zero to avoid false matches if another
               #ifndef at middle of file */
            file->ifndef_macro = 0;
            while (tok != TOK_LINEFEED)
                next_nomacro();
            next_tok_flags |= TOK_FLAG_ENDIF;
            goto the_end;
        }
        break;
    case TOK_LINE:
        next();
        if (tok != TOK_CINT)
            error("#line");
        file->line_num = tokc.i - 1; /* the line number will be incremented after */
        next();
        if (tok != TOK_LINEFEED) {
            if (tok != TOK_STR)
                error("#line");
            pstrcpy(file->filename, sizeof(file->filename), 
                    (char *)tokc.cstr->data);
        }
        break;
    case TOK_ERROR:
    case TOK_WARNING:
        c = tok;
        fch = file->buf_ptr[0];
        skip_spaces();
        q = buf;
        while (fch != '\n' && fch != CH_EOF) {
            if ((q - buf) < sizeof(buf) - 1)
                *q++ = fch;
            minp();
        }
        *q = '\0';
        if (c == TOK_ERROR)
            error("#error %s", buf);
        else
            warning("#warning %s", buf);
        break;
    case TOK_PRAGMA:
		// this case ignored in the original code
//        pragma_parse(s1);				// added by patch 354 Grischka  OYOY
        break;
    default:
        if (tok == TOK_LINEFEED || tok == '!' || tok == TOK_CINT) {
            /* '!' is ignored to allow C scripts. numbers are ignored
               to emulate cpp behaviour */
        } else {
            if (!(saved_parse_flags & PARSE_FLAG_ASM_COMMENTS))
                warning("Ignoring unknown preprocessing directive #%s",
                        get_tok_str(tok, &tokc));
        }
        break;
    }
    /* ignore other preprocess commands or #! for C scripts */
    while (tok != TOK_LINEFEED)
        next_nomacro();
 the_end:
    parse_flags = saved_parse_flags;
}

/* evaluate escape codes in a string. */
static void parse_escape_string(CString *outstr, uint8_t *buf, int is_long)
{
    int c, n, i;
    uint8_t *p;

    p = buf;
    for(;;) {
        c = *p;
        if (c == '\0')
            break;
        if (c == '\\') {
            /* at most three octal digits */
            p++;
            for (i = c = 0; i<3; i++) {
                n = *p;
                if (n<'0' || n>'7') break;
                p++;
                c = c*8+n-'0';
            }
            if (i) goto add_char_nonext;
            switch(c = *p) {
            case 'x':
            case 'u':
            case 'U':
                p++;
                n = 0;
                for(;;) {
                    c = *p;
                    if (c >= 'a' && c <= 'f')
                        c = c - 'a' + 10;
                    else if (c >= 'A' && c <= 'F')
                        c = c - 'A' + 10;
                    else if (isnum(c))
                        c = c - '0';
                    else
                        break;
                    n = n * 16 + c;
                    p++;
                }
                c = n;
                goto add_char_nonext;
            case 'a':
                c = '\a';
                break;
            case 'b':
                c = '\b';
                break;
            case 'f':
                c = '\f';
                break;
            case 'n':
                c = '\n';
                break;
            case 'r':
                c = '\r';
                break;
            case 't':
                c = '\t';
                break;
            case 'v':
                c = '\v';
                break;
            case 'e':
                if (!gnu_ext)
                    goto invalid_escape;
                c = 27;
                break;
            case '\'':
            case '\"':
            case '\\': 
            case '?':
                break;
            default:
            invalid_escape:
                if (c >= '!' && c <= '~')
                    warning("unknown escape sequence: \'\\%c\'", c);
                else
                    warning("unknown escape sequence: \'\\x%x\'", c);
                break;
            }
        }
        p++;
    add_char_nonext:
        if (!is_long)
            cstr_ccat(outstr, c);
        else
            cstr_wccat(outstr, c);
    }
    /* add a trailing '\0' */
    if (!is_long)
        cstr_ccat(outstr, '\0');
    else
        cstr_wccat(outstr, '\0');
}

/* we use 64 bit numbers */
#define BN_SIZE 2

/* bn = (bn << shift) | or_val */
void bn_lshift(unsigned int *bn, int shift, int or_val)
{
    int i;
    unsigned int v;
    for(i=0;i<BN_SIZE;i++) {
        v = bn[i];
        bn[i] = (v << shift) | or_val;
        or_val = v >> (32 - shift);
    }
}

void bn_zero(unsigned int *bn)
{
    int i;
    for(i=0;i<BN_SIZE;i++) {
        bn[i] = 0;
    }
}

/* parse number in null terminated string 'p' and return it in the
   current token */
void parse_number(char *p)
{
    int b, t, shift, frac_bits, s, exp_val, ch;
    char *q;
    unsigned int bn[BN_SIZE];
    double d;

    /* number */
    q = token_buf;
    ch = *p++;
    t = ch;
    ch = *p++;
    *q++ = t;
    b = 10;
    if (t == '.') {
        goto float_frac_parse;
    } else if (t == '0') {
        if (ch == 'x' || ch == 'X') {
            q--;
            ch = *p++;
            b = 16;
        } else if (tcc_ext && (ch == 'b' || ch == 'B')) {
            q--;
            ch = *p++;
            b = 2;
        }
    }
    /* parse all digits. cannot check octal numbers at this stage
       because of floating point constants */
    for (;;) {
        if (ch >= 'a' && ch <= 'f')
            t = ch - 'a' + 10;
        else if (ch >= 'A' && ch <= 'F')
            t = ch - 'A' + 10;
        else if (isnum(ch))
            t = ch - '0';
        else
            break;
        if (t >= b)
            break;
        if (q >= token_buf + STRING_MAX_SIZE) {
        num_too_long:
            error("number too long");
        }
        *q++ = ch;
        ch = *p++;
    }
    if (ch == '.' ||
        ((ch == 'e' || ch == 'E') && b == 10) ||
        ((ch == 'p' || ch == 'P') && (b == 16 || b == 2))) {
        if (b != 10) {
            /* NOTE: strtox should support that for hexa numbers, but
               non ISOC99 libcs do not support it, so we prefer to do
               it by hand */
            /* hexadecimal or binary floats */
            /* XXX: handle overflows */
            *q = '\0';
            if (b == 16)
                shift = 4;
            else 
                shift = 2;
            bn_zero(bn);
            q = token_buf;
            while (1) {
                t = *q++;
                if (t == '\0') {
                    break;
                } else if (t >= 'a') {
                    t = t - 'a' + 10;
                } else if (t >= 'A') {
                    t = t - 'A' + 10;
                } else {
                    t = t - '0';
                }
                bn_lshift(bn, shift, t);
            }
            frac_bits = 0;
            if (ch == '.') {
                ch = *p++;
                while (1) {
                    t = ch;
                    if (t >= 'a' && t <= 'f') {
                        t = t - 'a' + 10;
                    } else if (t >= 'A' && t <= 'F') {
                        t = t - 'A' + 10;
                    } else if (isnum(t)) {
                        t = t - '0';
                    } else {
                        break;
                    }
                    if (t >= b)
                        error("invalid digit");
                    bn_lshift(bn, shift, t);
                    frac_bits += shift;
                    ch = *p++;
                }
            }
            if (ch != 'p' && ch != 'P')
                expect("exponent");
            ch = *p++;
            s = 1;
            exp_val = 0;
            if (ch == '+') {
                ch = *p++;
            } else if (ch == '-') {
                s = -1;
                ch = *p++;
            }
            if (!isnum(ch))
                expect("exponent digits");
            while (isnum(ch)) {
                exp_val = exp_val * 10 + ch - '0';
                ch = *p++;
            }
            exp_val = exp_val * s;
            
            /* now we can generate the number */
            /* XXX: should patch directly float number */
            d = (double)bn[1] * 4294967296.0 + (double)bn[0];
            d = ldexp(d, exp_val - frac_bits);
            t = toup(ch);
            if (t == 'F') {
                ch = *p++;
                tok = TOK_CFLOAT;
                /* float : should handle overflow */
                tokc.f = (float)d;
            } else if (t == 'L') {
                ch = *p++;
                tok = TOK_CLDOUBLE;
                /* XXX: not large enough */
                tokc.ld = (long double)d;
            } else {
                tok = TOK_CDOUBLE;
                tokc.d = d;
            }
        } else {
            /* decimal floats */
            if (ch == '.') {
                if (q >= token_buf + STRING_MAX_SIZE)
                    goto num_too_long;
                *q++ = ch;
                ch = *p++;
            float_frac_parse:
                while (ch >= '0' && ch <= '9') {
                    if (q >= token_buf + STRING_MAX_SIZE)
                        goto num_too_long;
                    *q++ = ch;
                    ch = *p++;
                }
            }
            if (ch == 'e' || ch == 'E') {
                if (q >= token_buf + STRING_MAX_SIZE)
                    goto num_too_long;
                *q++ = ch;
                ch = *p++;
                if (ch == '-' || ch == '+') {
                    if (q >= token_buf + STRING_MAX_SIZE)
                        goto num_too_long;
                    *q++ = ch;
                    ch = *p++;
                }
                if (ch < '0' || ch > '9')
                    expect("exponent digits");
                while (ch >= '0' && ch <= '9') {
                    if (q >= token_buf + STRING_MAX_SIZE)
                        goto num_too_long;
                    *q++ = ch;
                    ch = *p++;
                }
            }
            *q = '\0';
            t = toup(ch);
            errno = 0;
            if (t == 'F') {
                ch = *p++;
                tok = TOK_CFLOAT;
                tokc.f = strtof(token_buf, NULL);
            } else if (t == 'L') {
                ch = *p++;
                tok = TOK_CLDOUBLE;
                tokc.ld = strtold(token_buf, NULL);
            } else {
                tok = TOK_CDOUBLE;
                tokc.d = strtod(token_buf, NULL);
            }
        }
    } else {
        unsigned long long n, n1;
        int lcount, ucount;

        /* integer number */
        *q = '\0';
        q = token_buf;
        if (b == 10 && *q == '0') {
            b = 8;
            q++;
        }
        n = 0;
        while(1) {
            t = *q++;
            /* no need for checks except for base 10 / 8 errors */
            if (t == '\0') {
                break;
            } else if (t >= 'a') {
                t = t - 'a' + 10;
            } else if (t >= 'A') {
                t = t - 'A' + 10;
            } else {
                t = t - '0';
                if (t >= b)
                    error("invalid digit");
            }
            n1 = n;
            n = n * b + t;
            /* detect overflow */
            /* XXX: this test is not reliable */
            if (n < n1)
                error("integer constant overflow");
        }
        
        /* XXX: not exactly ANSI compliant */
        if ((n & 0xffffffff00000000LL) != 0) {
            if ((n >> 63) != 0)
                tok = TOK_CULLONG;
            else
                tok = TOK_CLLONG;
        } else if (n > 0x7fffffff) {
            tok = TOK_CUINT;
        } else {
            tok = TOK_CINT;
        }
        lcount = 0;
        ucount = 0;
        for(;;) {
            t = toup(ch);
            if (t == 'L') {
                if (lcount >= 2)
                    error("three 'l's in integer constant");
                lcount++;
                if (lcount == 2) {
                    if (tok == TOK_CINT)
                        tok = TOK_CLLONG;
                    else if (tok == TOK_CUINT)
                        tok = TOK_CULLONG;
                }
                ch = *p++;
            } else if (t == 'U') {
                if (ucount >= 1)
                    error("two 'u's in integer constant");
                ucount++;
                if (tok == TOK_CINT)
                    tok = TOK_CUINT;
                else if (tok == TOK_CLLONG)
                    tok = TOK_CULLONG;
                ch = *p++;
            } else {
                break;
            }
        }
        if (tok == TOK_CINT || tok == TOK_CUINT)
            tokc.ui = n;
        else
            tokc.ull = n;
    }
}


#define PARSE2(c1, tok1, c2, tok2)              \
    case c1:                                    \
        PEEKC(c, p);                            \
        if (c == c2) {                          \
            p++;                                \
            tok = tok2;                         \
        } else {                                \
            tok = tok1;                         \
        }                                       \
        break;

/* return next token without macro substitution */
static inline void next_nomacro1(void)
{
    int t, c, is_long;
    TokenSym *ts;
    uint8_t *p, *p1;
    unsigned int h;

    p = file->buf_ptr;
 redo_no_start:
    c = *p;
    switch(c) {
    case ' ':
    case '\t':
    case '\f':
    case '\v':
    case '\r':
        p++;
        next_tok_flags |= TOK_FLAG_BOW;
        goto redo_no_start;
        
    case '\\':
        /* first look if it is in fact an end of buffer */
        if (p >= file->buf_end) {
            file->buf_ptr = p;
            handle_eob();
            p = file->buf_ptr;
            if (p >= file->buf_end)
                goto parse_eof;
            else
                goto redo_no_start;
        } else {
            file->buf_ptr = p;
            fch = *p;
            handle_stray();
            p = file->buf_ptr;
            goto redo_no_start;
        }
    parse_eof:
        {
            TCCState *s1 = tcc_state;
            if (parse_flags & PARSE_FLAG_LINEFEED) {
                tok = TOK_LINEFEED;
            } else if (s1->include_stack_ptr == s1->include_stack ||
                       !(parse_flags & PARSE_FLAG_PREPROCESS)) {
                /* no include left : end of file. */
                tok = TOK_EOF;
            } else {
                /* pop include file */
                
                /* test if previous '#endif' was after a #ifdef at
                   start of file */
                if (next_tok_flags & TOK_FLAG_ENDIF) {
#ifdef INC_DEBUG
                    printf("#endif %s\n", get_tok_str(file->ifndef_macro_saved, NULL));
#endif
                    add_cached_include(s1, file->inc_type, file->inc_filename,
                                       file->ifndef_macro_saved);
                }

                /* add end of include file debug info */
                if (do_debug) {
                    put_stabd(N_EINCL, 0, 0);
                }
                /* pop include stack */
                tcc_close(file);
                s1->include_stack_ptr--;
                file = *s1->include_stack_ptr;
                p = file->buf_ptr;
                goto redo_no_start;
            }
        }
        break;

    case '\n':
        if (parse_flags & PARSE_FLAG_LINEFEED) {
            tok = TOK_LINEFEED;
        } else {
            file->line_num++;
            next_tok_flags |= TOK_FLAG_BOW | TOK_FLAG_BOL;
            p++;
            goto redo_no_start;
        }
        break;

    case '#':
        /* XXX: simplify */
        PEEKC(c, p);
        if ((next_tok_flags & TOK_FLAG_BOL) && 
            (parse_flags & PARSE_FLAG_PREPROCESS)) {
            file->buf_ptr = p;
            preprocess(next_tok_flags & TOK_FLAG_BOF);
            p = file->buf_ptr;
            goto redo_no_start;
        } else {
            if (c == '#') {
                p++;
                tok = TOK_TWOSHARPS;
            } else {
                if (parse_flags & PARSE_FLAG_ASM_COMMENTS) {
                    p = parse_line_comment(p - 1);
                    goto redo_no_start;
                } else {
                    tok = '#';
                }
            }
        }
        break;

    case 'a': case 'b': case 'c': case 'd':
    case 'e': case 'f': case 'g': case 'h':
    case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p':
    case 'q': case 'r': case 's': case 't':
    case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z': 
    case 'A': case 'B': case 'C': case 'D':
    case 'E': case 'F': case 'G': case 'H':
    case 'I': case 'J': case 'K': 
    case 'M': case 'N': case 'O': case 'P':
    case 'Q': case 'R': case 'S': case 'T':
    case 'U': case 'V': case 'W': case 'X':
    case 'Y': case 'Z': 
    case '_':
    parse_ident_fast:
        p1 = p;
        h = TOK_HASH_INIT;
        h = TOK_HASH_FUNC(h, c);
        p++;
        for(;;) {
            c = *p;
            if (!isidnum_table[c])
                break;
            h = TOK_HASH_FUNC(h, c);
            p++;
        }
        if (c != '\\') {
            TokenSym **pts;
            int len;

            /* fast case : no stray found, so we have the full token
               and we have already hashed it */
            len = p - p1;
            h &= (TOK_HASH_SIZE - 1);
            pts = &hash_ident[h];
            for(;;) {
                ts = *pts;
                if (!ts)
                    break;
                if (ts->len == len && !memcmp(ts->str, p1, len))
                    goto token_found;
                pts = &(ts->hash_next);
            }
            ts = tok_alloc_new(pts, p1, len);
        token_found: ;
        } else {
            /* slower case */
            cstr_reset(&tokcstr);

            while (p1 < p) {
                cstr_ccat(&tokcstr, *p1);
                p1++;
            }
            p--;
            PEEKC(c, p);
        parse_ident_slow:
            while (isidnum_table[c]) {
                cstr_ccat(&tokcstr, c);
                PEEKC(c, p);
            }
            ts = tok_alloc(tokcstr.data, tokcstr.size);
        }
        tok = ts->tok;
        break;
    case 'L':
        t = p[1];
        if (t != '\\' && t != '\'' && t != '\"') {
            /* fast case */
            goto parse_ident_fast;
        } else {
            PEEKC(c, p);
            if (c == '\'' || c == '\"') {
                is_long = 1;
                goto str_const;
            } else {
                cstr_reset(&tokcstr);
                cstr_ccat(&tokcstr, 'L');
                goto parse_ident_slow;
            }
        }
        break;
    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
    case '8': case '9':

        cstr_reset(&tokcstr);
        /* after the first digit, accept digits, alpha, '.' or sign if
           prefixed by 'eEpP' */
    parse_num:
        for(;;) {
            t = c;
            cstr_ccat(&tokcstr, c);
            PEEKC(c, p);
            if (!(isidnum_table[c] || c == '.' ||
                  ((c == '+' || c == '-') && 
                   (t == 'e' || t == 'E' || t == 'p' || t == 'P'))))
                break;
        }
        /* We add a trailing '\0' to ease parsing */
        cstr_ccat(&tokcstr, '\0');
        tokc.cstr = &tokcstr;
        tok = TOK_PPNUM;
        break;
    case '.':
        /* special dot handling because it can also start a number */
        PEEKC(c, p);
        if (isnum(c)) {
            cstr_reset(&tokcstr);
            cstr_ccat(&tokcstr, '.');
            goto parse_num;
        } else if (c == '.') {
            PEEKC(c, p);
            if (c != '.')
                expect("'.'");
            PEEKC(c, p);
            tok = TOK_DOTS;
        } else {
            tok = '.';
        }
        break;
    case '\'':
    case '\"':
        is_long = 0;
    str_const:
        {
            CString str;
            int sep;

            sep = c;

            /* parse the string */
            cstr_new(&str);
            p = parse_pp_string(p, sep, &str);
            cstr_ccat(&str, '\0');
            
            /* eval the escape (should be done as TOK_PPNUM) */
            cstr_reset(&tokcstr);
            parse_escape_string(&tokcstr, str.data, is_long);
            cstr_free(&str);

            if (sep == '\'') {
                int char_size;
                /* XXX: make it portable */
                if (!is_long)
                    char_size = 1;
                else
                    char_size = sizeof(int);			// original code
//                    char_size = sizeof(nwchar_t);		modded by patch 384 Navara  OYOY
                if (tokcstr.size <= char_size)
                    error("empty character constant");
                if (tokcstr.size > 2 * char_size)
                    warning("multi-character character constant");
                if (!is_long) {
                    tokc.i = *(int8_t *)tokcstr.data;
                    tok = TOK_CCHAR;
                } else {
                    tokc.i = *(int *)tokcstr.data;				// original code
//                    tokc.i = *(nwchar_t *)tokcstr.data;		modded by patch 384 Navara  OYOY
                    tok = TOK_LCHAR;
                }
            } else {
                tokc.cstr = &tokcstr;
                if (!is_long)
                    tok = TOK_STR;
                else
                    tok = TOK_LSTR;
            }
        }
        break;

    case '<':
        PEEKC(c, p);
        if (c == '=') {
            p++;
            tok = TOK_LE;
        } else if (c == '<') {
            PEEKC(c, p);
            if (c == '=') {
                p++;
                tok = TOK_A_SHL;
            } else {
                tok = TOK_SHL;
            }
        } else {
            tok = TOK_LT;
        }
        break;
        
    case '>':
        PEEKC(c, p);
        if (c == '=') {
            p++;
            tok = TOK_GE;
        } else if (c == '>') {
            PEEKC(c, p);
            if (c == '=') {
                p++;
                tok = TOK_A_SAR;
            } else {
                tok = TOK_SAR;
            }
        } else {
            tok = TOK_GT;
        }
        break;
        
    case '&':
        PEEKC(c, p);
        if (c == '&') {
            p++;
            tok = TOK_LAND;
        } else if (c == '=') {
            p++;
            tok = TOK_A_AND;
        } else {
            tok = '&';
        }
        break;
        
    case '|':
        PEEKC(c, p);
        if (c == '|') {
            p++;
            tok = TOK_LOR;
        } else if (c == '=') {
            p++;
            tok = TOK_A_OR;
        } else {
            tok = '|';
        }
        break;

    case '+':
        PEEKC(c, p);
        if (c == '+') {
            p++;
            tok = TOK_INC;
        } else if (c == '=') {
            p++;
            tok = TOK_A_ADD;
        } else {
            tok = '+';
        }
        break;
        
    case '-':
        PEEKC(c, p);
        if (c == '-') {
            p++;
            tok = TOK_DEC;
        } else if (c == '=') {
            p++;
            tok = TOK_A_SUB;
        } else if (c == '>') {
            p++;
            tok = TOK_ARROW;
        } else {
            tok = '-';
        }
        break;

    PARSE2('!', '!', '=', TOK_NE)
    PARSE2('=', '=', '=', TOK_EQ)
    PARSE2('*', '*', '=', TOK_A_MUL)
    PARSE2('%', '%', '=', TOK_A_MOD)
    PARSE2('^', '^', '=', TOK_A_XOR)
        
        /* comments or operator */
    case '/':
        PEEKC(c, p);
        if (c == '*') {
            p = parse_comment(p);
            next_tok_flags |= TOK_FLAG_BOW;
            goto redo_no_start;
        } else if (c == '/') {
            p = parse_line_comment(p);
            next_tok_flags |= TOK_FLAG_BOW;
            goto redo_no_start;
        } else if (c == '=') {
            p++;
            tok = TOK_A_DIV;
        } else {
            tok = '/';
        }
        break;
        
        /* simple tokens */
    case '(':
    case ')':
    case '[':
    case ']':
    case '{':
    case '}':
    case ',':
    case ';':
    case ':':
    case '?':
    case '~':
    case '$': /* only used in assembler */
    case '@': /* dito */
        tok = c;
        p++;
        break;
    default:
        error("unrecognized character \\x%02x", c);
        break;
    }
    file->buf_ptr = p;
    tok_flags = next_tok_flags;
    next_tok_flags = 0;
#if defined(PARSE_DEBUG)
    printf("token = %s\n", get_tok_str(tok, &tokc));
#endif
}

/* return next token without macro substitution. Can read input from
   macro_ptr buffer */
static void next_nomacro(void)
{
    if (!macro_ptr) next_nomacro1();
    else for (;;) {
        tok = *macro_ptr;
        if (tok) {
            TOK_GET(tok, tok_flags, macro_ptr, tokc);
            if (tok == TOK_LINENUM) {
                file->line_num = tokc.i;
                continue;
            }
        }
        return;
    }
}

/* substitute args in macro_str and return allocated string */
static int *macro_arg_subst(Sym **nested_list, int *macro_str, Sym *args)
{
    int *st, last_tok, t, tf, save_tf, notfirst;
    Sym *s;
    CValue cval;
    TokenString str;
    CString cstr;

    tok_str_new(&str);
    last_tok = 0;
    while(1) {
        TOK_GET(t, tf, macro_str, cval);
        save_tf = tf;
        if (!t)
            break;
        if (t == '#') {
            /* stringize */
            TOK_GET(t, tf, macro_str, cval);
            if (!t)
                break;
            s = sym_find2(args, t);
            if (s) {
                cstr_new(&cstr);
                st = (int *)s->c;
                notfirst = 0;
                while (*st) {
                    TOK_GET(t, tf, st, cval);
                    if (notfirst && tf & TOK_FLAG_BOW)
                        cstr_ccat(&cstr, ' ');
                    cstr_cat(&cstr, get_tok_str(t, &cval));
                    notfirst = 1;
                }
                cstr_ccat(&cstr, '\0');
#ifdef PP_DEBUG
                printf("stringize: %s\n", (char *)cstr.data);
#endif
                /* add string */
                cval.cstr = &cstr;
                tok_str_add2(&str, TOK_STR, save_tf, &cval);
                cstr_free(&cstr);
            } else {
                tok_str_add2(&str, t, save_tf, &cval);
            }
        } else if (t >= TOK_IDENT) {
            s = sym_find2(args, t);
            if (s) {
                st = (int *)s->c;
                st[1] = save_tf;
                /* if '##' is present before or after, no arg substitution */
                if (*macro_str == TOK_TWOSHARPS || last_tok == TOK_TWOSHARPS) {
                    /* special case for var arg macros : ## eats the
                       ',' if empty VA_ARGS variable. */
                    /* XXX: test of the ',' is not 100%
                       reliable. should fix it to avoid security
                       problems */
                    if (gnu_ext && s->type.t &&
                        last_tok == TOK_TWOSHARPS && 
                        str.len >= 4 && str.str[str.len - 4] == ',') {
                        if (*st == 0) {
                            /* suppress ',' '##' */
                            str.len -= 4;
                            next_tok_flags = tok_flags;
                        } else {
                            /* suppress '##' and add variable */
                            str.len -= 2;
                            goto add_var;
                        }
                    } else {
                        int t1;
                    add_var:
                        for(;;) {
                            TOK_GET(t1, tf, st, cval);
                            if (!t1)
                                break;
                            tok_str_add2(&str, t1, save_tf, &cval);
                        }
                    }
                } else {
                    /* NOTE: the stream cannot be read when macro
                       substituing an argument */
                    macro_subst(&str, nested_list, st, 0);			// original code
//                    macro_subst(&str, nested_list, st, NULL);		modded by patch 377 Grischka  OYOY
                }
            } else {
                tok_str_add(&str, t, tf);
            }
        } else {
            tok_str_add2(&str, t, tf, &cval);
        }
        last_tok = t;
    }
    tok_str_add(&str, 0, 0);
    return str.str;
}

static char ab_month_name[12][4] =
{
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

/* do macro substitution of current token with macro 's' and add
   result to (tok_str,tok_len). 'nested_list' is the list of all
   macros we got inside to avoid recursing. Return non zero if no
   substitution needs to be done */
static int macro_subst_tok(TokenString *tok_str,
                           Sym **nested_list, Sym *s, int can_read_stream)					// original code
//                           Sym **nested_list, Sym *s, struct macro_level **can_read_stream)	modded by patch 377 Grischka  OYOY
{
    Sym *args, *sa, *sa1;
    int mstr_allocated, parlevel, *mstr, t, t1;
    TokenString str;
    char *cstrval;
    CValue cval;
    CString cstr;
    char buf[32];
    
    /* if symbol is a macro, prepare substitution */
    /* special macros */
    if (tok == TOK___LINE__) {
        snprintf(buf, sizeof(buf), "%d", file->line_num);
        cstrval = buf;
        t1 = TOK_PPNUM;
        goto add_cstr1;
    } else if (tok == TOK___FILE__) {
        cstrval = file->filename;
        goto add_cstr;
    } else if (tok == TOK___DATE__ || tok == TOK___TIME__) {
        time_t ti;
        struct tm *tm;

        time(&ti);
        tm = localtime(&ti);
        if (tok == TOK___DATE__) {
            snprintf(buf, sizeof(buf), "%s %2d %d", 
                     ab_month_name[tm->tm_mon], tm->tm_mday, tm->tm_year + 1900);
        } else {
            snprintf(buf, sizeof(buf), "%02d:%02d:%02d", 
                     tm->tm_hour, tm->tm_min, tm->tm_sec);
        }
        cstrval = buf;
    add_cstr:
        t1 = TOK_STR;
    add_cstr1:
        cstr_new(&cstr);
        cstr_cat(&cstr, cstrval);
        cstr_ccat(&cstr, '\0');
        cval.cstr = &cstr;
        tok_str_add2(tok_str, t1, tok_flags, &cval);
        cstr_free(&cstr);
    } else {
        mstr = (int *)s->c;
        mstr[1] = tok_flags;
        mstr_allocated = 0;
        if (s->type.t == MACRO_FUNC) {
            /* NOTE: we do not use next_nomacro to avoid eating the
               next token. XXX: find better solution */
//        redo:						added by patch 377 Grischka  OYOY
            if (macro_ptr) {
                t = *macro_ptr;
                if (t == 0 && can_read_stream) {
                    /* end of macro stream: we must look at the token
                       after in the file */
//                    struct macro_level *ml = *can_read_stream;				added by patch 377 Grischka  OYOY
                    macro_ptr = NULL;
//                    if (ml)				7 lines added by patch 377 Grischka  OYOY
//                    {
//                        macro_ptr = ml->p;
//                        ml->p = NULL;
//                        *can_read_stream = ml -> prev;
//                    }
//                    goto redo;
					goto parse_stream;		// original code, removed by above patch
                }
            } else {
parse_stream:		// original code, removed by patch 377 Grischka  OYOY
                /* XXX: incorrect with comments */
                fch = file->buf_ptr[0];
                while (is_space(fch) || fch == '\n')
                    cinp();
                t = fch;
            }
            if (t != '(') /* no macro subst */
                return -1;
                    
            /* argument macro */
            next_nomacro();
            next_nomacro();
            args = NULL;
            sa = s->next;
            /* NOTE: empty args are allowed, except if no args */
            for(;;) {
                /* handle '()' case */
                if (!args && !sa && tok == ')')
                    break;
                if (!sa)
                    error("macro '%s' used with too many args",
                          get_tok_str(s->token, 0));
                tok_str_new(&str);
                parlevel = 0;
                /* NOTE: non zero sa->t indicates VA_ARGS */
                while ((parlevel > 0 || 
                        (tok != ')' && 
                         (tok != ',' || sa->type.t))) && 
                       tok != -1) {
                    if (tok == '(')
                        parlevel++;
                    else if (tok == ')')
                        parlevel--;
                    tok_str_add2(&str, tok, tok_flags, &tokc);
                    next_nomacro();
                }
                tok_str_add(&str, 0, 0);
                sym_push2(&args, sa->token & ~SYM_FIELD, sa->type.t,
                    (long)str.str);
                sa = sa->next;
                if (tok == ')') {
                    /* special case for gcc var args: add an empty
                       var arg argument if it is omitted */
                    if (sa && sa->type.t && gnu_ext)
                        continue;
                    else
                        break;
                }
                if (tok != ',')
                    expect(",");
                next_nomacro();
            }
            if (sa) {
                error("macro '%s' used with too few args",
                      get_tok_str(s->token, 0));
            }

            /* now subst each arg */
            mstr = macro_arg_subst(nested_list, mstr, args);
            /* free memory */
            sa = args;
            while (sa) {
                sa1 = sa->prev;
                tok_str_free((int *)sa->c);
                sym_free(sa);
                sa = sa1;
            }
            mstr_allocated = 1;
        }
        sym_push2(nested_list, s->token, 0, 0);
        macro_subst(tok_str, nested_list, mstr, can_read_stream);
        /* pop nested defined symbol */
        sa1 = *nested_list;
        *nested_list = sa1->prev;
        sym_free(sa1);
        if (mstr_allocated)
            tok_str_free(mstr);
    }
    return 0;
}

/* handle the '##' operator. Return NULL if no '##' seen. Otherwise
   return the resulting string (which must be freed). */
static inline int *macro_twosharps(int *macro_str)
{
    TokenSym *ts;
    int *macro_ptr1, *start_macro_ptr, *ptr, *saved_macro_ptr;
    int t, tf;
    char *p1, *p2;
    CValue cval;
    TokenString macro_str1;
    CString cstr;

    start_macro_ptr = macro_str;
    /* we search the first '##' */
    for(;;) {
        macro_ptr1 = macro_str;
        TOK_GET(t, tf, macro_str, cval);
        /* nothing more to do if end of string */
        if (t == 0)
            return NULL;
        if (*macro_str == TOK_TWOSHARPS)
            break;
    }

    /* we saw '##', so we need more processing to handle it */
    cstr_new(&cstr);
    tok_str_new(&macro_str1);
    tok = t;
    tok_flags = tf;
    tokc = cval;

    /* add all tokens seen so far */
    for(ptr = start_macro_ptr; ptr < macro_ptr1;) {
        TOK_GET(t, tf, ptr, cval);
        tok_str_add2(&macro_str1, t, tf, &cval);
    }
    saved_macro_ptr = macro_ptr;
    /* XXX: get rid of the use of macro_ptr here */
    macro_ptr = (int *)macro_str;
    for(;;) {
        while (*macro_ptr == TOK_TWOSHARPS) {
            macro_ptr+=2;
            macro_ptr1 = macro_ptr;
            t = *macro_ptr;
            if (t) {
                TOK_GET(t, tf, macro_ptr, cval);
                /* We concatenate the two tokens if we have an
                   identifier or a preprocessing number */
                cstr_reset(&cstr);
                p1 = get_tok_str(tok, &tokc);
                cstr_cat(&cstr, p1);
                p2 = get_tok_str(t, &cval);
                cstr_cat(&cstr, p2);
                cstr_ccat(&cstr, '\0');
                
                if ((tok >= TOK_IDENT || tok == TOK_PPNUM) && 
                    (t >= TOK_IDENT || t == TOK_PPNUM)) {
                    if (tok == TOK_PPNUM) {
                        /* if number, then create a number token */
                        /* NOTE: we cannot use cstr directly for this
                           because if there are multiple token pastings
                           in a sequence the concatenation will reset
                           cstr before the final token is ready. */
                        cstr_reset(&tokcstr);
                        cstr_cat(&tokcstr, cstr.data);
                        cstr_ccat(&tokcstr, '\0');
                        tokc.cstr = &tokcstr;
                    } else {
                        /* if identifier, we must do a test to
                           validate we have a correct identifier */
                        if (t == TOK_PPNUM) {
                            char *p;
                            int c;

                            p = p2;
                            for(;;) {
                                c = *p;
                                if (c == '\0')
                                    break;
                                p++;
                                if (!isidnum_table[c])
                                    goto error_pasting;
                            }
                        }
                        ts = tok_alloc(cstr.data, strlen(cstr.data));
                        tok = ts->tok; /* modify current token */
                    }
                } else {
                    char *str = cstr.data;
                    unsigned char *q;

                    /* we look for a valid token */
                    /* XXX: do more extensive checks */
                    if (!strcmp(str, ">>=")) {
                        tok = TOK_A_SAR;
                    } else if (!strcmp(str, "<<=")) {
                        tok = TOK_A_SHL;
                    } else if (strlen(str) == 2) {
                        /* search in two bytes table */
                        q = tok_two_chars;
                        for(;;) {
                            if (!*q)
                                goto error_pasting;
                            if (q[0] == str[0] && q[1] == str[1])
                                break;
                            q += 3;
                        }
                        tok = q[2];
                    } else {
                    error_pasting:
                        /* NOTE: because get_tok_str use a static buffer,
                           we must save it */
                        cstr_reset(&cstr);
                        p1 = get_tok_str(tok, &tokc);
                        cstr_cat(&cstr, p1);
                        cstr_ccat(&cstr, '\0');
                        p2 = get_tok_str(t, &cval);
                        warning("pasting \"%s\" and \"%s\" does not give a valid preprocessing token", cstr.data, p2);
                        /* cannot merge tokens: just add them separately */
                        tok_str_add2(&macro_str1, tok, tok_flags, &tokc);
                        /* XXX: free associated memory ? */
                        tok = t;
                        tokc = cval;
                    }
                }
            }
        }
        tok_str_add2(&macro_str1, tok, tok_flags, &tokc);
        next_nomacro();
        if (tok == 0)
            break;
    }
    macro_ptr = (int *)saved_macro_ptr;
    cstr_free(&cstr);
    tok_str_add(&macro_str1, 0, 0);
    return macro_str1.str;
}


/* do macro substitution of macro_str and add result to
   (tok_str,tok_len). 'nested_list' is the list of all macros we got
   inside to avoid recursing. */
static void macro_subst(TokenString *tok_str, Sym **nested_list, 
                        int *macro_str, int can_read_stream)					// original code
//                        int *macro_str, struct macro_level ** can_read_stream)		modded by patch 377 Grischka  OYOY
{
    Sym *s;
    int *macro_str1;
	int *saved_macro_ptr;		// variable removed by patch 377 Grischka  OYOY
    int *ptr;
    int t, tf, ret;
    CValue cval;
//    struct macro_level ml;			added by patch 377 Grischka  OYOY
    
    /* first scan for '##' operator handling */
    ptr = macro_str;
    macro_str1 = macro_twosharps(ptr);
    if (macro_str1) 
        ptr = macro_str1;
    while (1) {
        /* NOTE: ptr == NULL can only happen if tokens are read from
           file stream due to a macro function call */
        if (ptr == NULL)
            break;
        TOK_GET(t, tf, ptr, cval);
        if (t == 0)
            break;
        s = define_find(t);
        if (s != NULL) {
            /* if nested substitution, do nothing */
            if (sym_find2(*nested_list, t))
                goto no_subst;
            saved_macro_ptr = macro_ptr;			// original code
//            ml.p = macro_ptr;					3 lines added by patch 377 Grischka (to replace above line)  OYOY
//            if (can_read_stream)
//                ml.prev = *can_read_stream, *can_read_stream = &ml;
            macro_ptr = (int *)ptr;
            tok = t;
            tok_flags = tf;
            ret = macro_subst_tok(tok_str, nested_list, s, can_read_stream);
            ptr = (int *)macro_ptr;
            macro_ptr = ml.p;
            if (can_read_stream && *can_read_stream == &ml)
                    *can_read_stream = ml.prev;
            if (ret != 0)
                goto no_subst;
        } else {
        no_subst:
            tok_str_add2(tok_str, t, tf, &cval);
        }
    }
    if (macro_str1)
        tok_str_free(macro_str1);
}


/* return next token with macro substitution */
static void next(void)
{
    Sym *nested_list, *s;
    TokenString str;
//    struct macro_level *ml;			added by patch 377 Grischka  OYOY

    for (;;) {
        next_nomacro();
        if (!macro_ptr) {
            /* if not reading from macro substituted string, then try
               to substitute macros */
            if (tok >= TOK_IDENT &&
                (parse_flags & PARSE_FLAG_PREPROCESS)) {
                s = define_find(tok);
                if (s) {
                    /* we have a macro: we try to substitute */
                    tok_str_new(&str);
                    nested_list = NULL;
//                    ml = NULL;							2 lines added by patch 377 Grischka to replace the following line  OYOY
//                    if (macro_subst_tok(&str, &nested_list, s, &ml) == 0) {
                    if (macro_subst_tok(&str, &nested_list, s, 1) == 0) {			// original code
                        /* substitution done, NOTE: maybe empty */
                        tok_str_add(&str, 0, 0);
                        macro_ptr = str.str;
                        macro_ptr_allocated = str.str;
                        continue;
                    }
                }
            }
        } else {
            if (tok == 0) {
                /* end of macro or end of unget buffer */
                if (unget_buffer_enabled) {
                    macro_ptr = unget_saved_macro_ptr;
                    unget_buffer_enabled = 0;
                } else {
                    /* end of macro string: free it */
                    tok_str_free(macro_ptr_allocated);
                    macro_ptr = NULL;
                }
                continue;
            }
        }
        break;
    }
    
    /* convert preprocessor tokens into C tokens */
    if (tok == TOK_PPNUM &&
        (parse_flags & PARSE_FLAG_TOK_NUM)) {
        parse_number((char *)tokc.cstr->data);
    }
    
}

/* push back current token and set current token to 'last_tok'. Only
   identifier case handled for labels. */
static inline void unget_tok(int last_tok)
{
    int i, n;
    int *q;
    unget_saved_macro_ptr = macro_ptr;
    unget_buffer_enabled = 1;
    q = unget_saved_buffer;
    macro_ptr = q;
    *q++ = tok;
    n = tok_ext_size(tok) - 1;
    for(i=0;i<n;i++)
        *q++ = tokc.tab[i];
    *q = 0; /* end of token string */
    tok = last_tok;
}


void swap(int *p, int *q)
{
    int t;
    t = *p;
    *p = *q;
    *q = t;
}

void vsetc(CType *type, int r, CValue *vc)
{
    int v;

    if (vtop >= vstack + (VSTACK_SIZE - 1))
        error("memory full");
    /* cannot let cpu flags if other instruction are generated. Also
       avoid leaving VT_JMP anywhere except on the top of the stack
       because it would complicate the code generator. */
    if (vtop >= vstack) {
        v = vtop->r & VT_VALMASK;
        if (v == VT_CMP || (v & ~1) == VT_JMP)
            gv(RC_INT);
    }
    vtop++;
    vtop->type = *type;
    vtop->r = r;
    vtop->r2 = VT_CONST;
    vtop->c = *vc;
}

/* push integer constant */
void vpushi(int v)
{
    CValue cval;
    cval.i = v;
    vsetc(&int_type, VT_CONST, &cval);
}

/* Return a static symbol pointing to a section */
static Sym *get_sym_ref(CType *type, Section *sec, 
                        unsigned long offset, unsigned long size)
{
    int v;
    Sym *sym;

    v = anon_sym++;
    sym = global_identifier_push(v, type->t | VT_STATIC, 0);
    sym->type.ref = type->ref;
    sym->r = VT_CONST | VT_SYM;
    put_extern_sym(sym, sec, offset, size);
    return sym;
}

/* push a reference to a section offset by adding a dummy symbol */
static void vpush_ref(CType *type, Section *sec, unsigned long offset, unsigned long size)
{
    CValue cval;

    cval.ul = 0;
    vsetc(type, VT_CONST | VT_SYM, &cval);
    vtop->sym = get_sym_ref(type, sec, offset, size);
}

/* define a new external reference to a symbol 'v' of type 'u' */
static Sym *external_global_sym(int v, CType *type, int r)
{
    Sym *s;

    s = sym_find(v);
    if (!s) {
        /* push forward reference */
        s = global_identifier_push(v, type->t | VT_EXTERN, 0);
        s->type.ref = type->ref;
        s->r = r | VT_CONST | VT_SYM;
    }
    return s;
}

/* define a new external reference to a symbol 'v' of type 'u' */
static Sym *external_sym(int v, CType *type, int r)
{
    Sym *s;

    s = sym_find(v);
    if (!s) {
        /* push forward reference */
        s = sym_push(v, type, r | VT_CONST | VT_SYM, 0);
        s->type.t |= VT_EXTERN;
    } else {
        if (!is_compatible_types(&s->type, type))
            error("incompatible types for redefinition of '%s'", 
                  get_tok_str(v, NULL));
    }
    return s;
}

/* push a reference to global symbol v */
static void vpush_global_sym(CType *type, int v)
{
    Sym *sym;
    CValue cval;

    sym = external_global_sym(v, type, 0);
    cval.ul = 0;
    vsetc(type, VT_CONST | VT_SYM, &cval);
    vtop->sym = sym;
}

void vset(CType *type, int r, int v)
{
    CValue cval;

    cval.i = v;
    vsetc(type, r, &cval);
}

void vseti(int r, int v)
{
    CType type;
    type.t = VT_INT;
    vset(&type, r, v);
}

void vswap(void)
{
    SValue tmp;

    tmp = vtop[0];
    vtop[0] = vtop[-1];
    vtop[-1] = tmp;
}

void vpushv(SValue *v)
{
    if (vtop >= vstack + (VSTACK_SIZE - 1))
        error("memory full");
    vtop++;
    *vtop = *v;
}

void vdup(void)
{
    vpushv(vtop);
}

/* save r to the memory stack, and mark it as being free */
void save_reg(int r)
{
    int l, saved, size, align;
    SValue *p, sv;
    CType *type;

    /* modify all stack values */
    saved = 0;
    l = 0;
    for(p=vstack;p<=vtop;p++) {
        if ((p->r & VT_VALMASK) == r ||
            ((p->type.t & VT_BTYPE) == VT_LLONG && (p->r2 & VT_VALMASK) == r))
        {
            /* must save value on stack if not already done */
            if (!saved) {
                /* NOTE: must reload 'r' because r might be equal to r2 */
                r = p->r & VT_VALMASK;
                /* store register in the stack */
                type = &p->type;
                if ((p->r & VT_LVAL) || 
                    (!is_float(type->t) && (type->t & VT_BTYPE) != VT_LLONG))
                    type = &int_type;
                size = type_size(type, &align);
                loc = (loc - size) & -align;
                sv.type.t = type->t;
                sv.r = VT_LOCAL | VT_LVAL;
                sv.c.ul = loc;
                store(r, &sv);
#ifdef TINYCC_TARGET_I386
                /* x86 specific: need to pop fp register ST0 if saved */
                if (r == TREG_ST0) {
                    gen_multibyte(0xd9dd); /* fstp %st(1) */
                }
#endif
                /* special long long case */
                if ((type->t & VT_BTYPE) == VT_LLONG) {
                    sv.c.ul += 4;
                    store(p->r2, &sv);
                }
                l = loc;
                saved = 1;
            }
            /* mark that stack entry as being saved on the stack */
            if (p->r & VT_LVAL) {
                /* also clear the bounded flag because the
                   relocation address of the function was stored in
                   p->c.ul */
                p->r = (p->r & ~(VT_VALMASK | VT_BOUNDED)) | VT_LLOCAL;
            } else {
                p->r = lvalue_type(p->type.t) | VT_LOCAL;
            }
            p->r2 = VT_CONST;
            p->c.ul = l;
        }
    }
}

/*
// find a register of class 'rc2' with at most one reference on stack.		this function was created by patch 295 Glockner  OYOY
// If none, call get_reg(rc)
int get_reg_ex(int rc, int rc2) 
{
    int r;
    SValue *p;
    
    for(r=0;r<NB_REGS;r++) {
        if (reg_classes[r] & rc2) {
            int n;
            n=0;
            for(p = vstack; p <= vtop; p++) {
                if ((p->r & VT_VALMASK) == r ||
                    (p->r2 & VT_VALMASK) == r)
                    n++;
            }
            if (n <= 1)
                return r;
        }
    }
    return get_reg(rc);
}
*/

/* find a free register of class 'rc'. If none, save one register */
int get_reg(int rc)
{
    int r;
    SValue *p;

    /* find a free register */
    for(r=0;r<NB_REGS;r++) {
        if (reg_classes[r] & rc) {
            for(p=vstack;p<=vtop;p++) {
                if ((p->r & VT_VALMASK) == r ||
                    (p->r2 & VT_VALMASK) == r)
                    goto notfound;
            }
            return r;
        }
    notfound: ;
    }
    
    /* no register left : free the first one on the stack (VERY
       IMPORTANT to start from the bottom to ensure that we don't
       spill registers used in gen_opi()) */
    for(p=vstack;p<=vtop;p++) {
        r = p->r & VT_VALMASK;
        if (r < VT_CONST && (reg_classes[r] & rc))
            goto save_found;
        /* also look at second register (if long long) */
        r = p->r2 & VT_VALMASK;
        if (r < VT_CONST && (reg_classes[r] & rc)) {
        save_found:
            save_reg(r);
            return r;
        }
    }
    /* Should never comes here */
    return -1;
}

/* save registers up to (vtop - n) stack entry */
void save_regs(int n)
{
    int r;
    SValue *p, *p1;
    p1 = vtop - n;
    for(p = vstack;p <= p1; p++) {
        r = p->r & VT_VALMASK;
        if (r < VT_CONST) {
            save_reg(r);
        }
    }
}

/* move register 's' to 'r', and flush previous value of r to memory
   if needed */
void move_reg(int r, int s)
{
    SValue sv;

    if (r != s) {
        save_reg(r);
        sv.type.t = VT_INT;
        sv.r = s;
        sv.c.ul = 0;
        load(r, &sv);
    }
}

/* get address of vtop (vtop MUST BE an lvalue) */
void gaddrof(void)
{
    vtop->r &= ~VT_LVAL;
    /* tricky: if saved lvalue, then we can go back to lvalue */
    if ((vtop->r & VT_VALMASK) == VT_LLOCAL)
        vtop->r = (vtop->r & ~(VT_VALMASK | VT_LVAL_TYPE)) | VT_LOCAL | VT_LVAL;
}

#ifdef CONFIG_TCC_BCHECK
/* generate lvalue bound code */
void gbound(void)
{
    int lval_type;
    CType type1;

    vtop->r &= ~VT_MUSTBOUND;
    /* if lvalue, then use checking code before dereferencing */
    if (vtop->r & VT_LVAL) {
        /* if not VT_BOUNDED value, then make one */
        if (!(vtop->r & VT_BOUNDED)) {
            lval_type = vtop->r & (VT_LVAL_TYPE | VT_LVAL);
            /* must save type because we must set it to int to get pointer */
            type1 = vtop->type;
            vtop->type.t = VT_INT;
            gaddrof();
            vpushi(0);
            gen_bounded_ptr_add();
            vtop->r |= lval_type;
            vtop->type = type1;
        }
        /* then check for dereferencing */
        gen_bounded_ptr_deref();
    }
}
#endif

/* store vtop a register belonging to class 'rc'. lvalues are
   converted to values. Cannot be used if cannot be converted to
   register value (such as structures). */
int gv(int rc)
{
    int r, r2, rc2, bit_pos, bit_size, size, align, i;
    unsigned long long ll;

    /* NOTE: get_reg can modify vstack[] */
    if (vtop->type.t & VT_BITFIELD) {
        bit_pos = (vtop->type.t >> VT_STRUCT_SHIFT) & 0x3f;
        bit_size = (vtop->type.t >> (VT_STRUCT_SHIFT + 6)) & 0x3f;
        /* remove bit field info to avoid loops */
        vtop->type.t &= ~(VT_BITFIELD | (-1 << VT_STRUCT_SHIFT));
        /* generate shifts */
        vpushi(32 - (bit_pos + bit_size));
        gen_op(TOK_SHL);
        vpushi(32 - bit_size);
        /* NOTE: transformed to SHR if unsigned */
        gen_op(TOK_SAR);
        r = gv(rc);
    } else {
        if (is_float(vtop->type.t) && 
            (vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
            Sym *sym;
            int *ptr;
            unsigned long offset;
#if defined(TINYCC_TARGET_ARM) && !defined(TCC_ARM_VFP)
            CValue check;
#endif
            
            /* XXX: unify with initializers handling ? */
            /* CPUs usually cannot use float constants, so we store them
               generically in data segment */
            size = type_size(&vtop->type, &align);
            offset = (data_section->data_offset + align - 1) & -align;
            data_section->data_offset = offset;
            /* XXX: not portable yet */
#ifdef __i386__
            /* long doubles are defined to be 96 bit wide by
               the i386 ABI but the x87 only uses the first 80. The rest
               is filled with garbage by now, so clear it before writing */
            if(size == 12)
                vtop->c.tab[2] &= 0xffff;
#endif
            ptr = section_ptr_add(data_section, size);
            size = size >> 2;
#if defined(TINYCC_TARGET_ARM) && !defined(TCC_ARM_VFP)
            check.d = 1;
            if(check.tab[0])
                for(i=0;i<size;i++)
                    ptr[i] = vtop->c.tab[size-1-i];
            else
#endif
            for(i=0;i<size;i++)
                ptr[i] = vtop->c.tab[i];
            sym = get_sym_ref(&vtop->type, data_section, offset, size << 2);
            vtop->r |= VT_LVAL | VT_SYM;
            vtop->sym = sym;
            vtop->c.ul = 0;
        }
#ifdef CONFIG_TCC_BCHECK
        if (vtop->r & VT_MUSTBOUND) 
            gbound();
#endif

        r = vtop->r & VT_VALMASK;
        /* need to reload if:
           - constant
           - lvalue (need to dereference pointer)
           - already a register, but not in the right class */
        if (r >= VT_CONST || 
            (vtop->r & VT_LVAL) ||
            !(reg_classes[r] & rc) ||
            ((vtop->type.t & VT_BTYPE) == VT_LLONG && 
             !(reg_classes[vtop->r2] & rc))) {
            r = get_reg(rc);
            if ((vtop->type.t & VT_BTYPE) == VT_LLONG) {
                /* two register type load : expand to two words
                   temporarily */
                if ((vtop->r & (VT_VALMASK | VT_LVAL)) == VT_CONST) {
                    /* load constant */
                    ll = vtop->c.ull;
                    vtop->c.ui = ll; /* first word */
                    load(r, vtop);
                    vtop->r = r; /* save register value */
                    vpushi(ll >> 32); /* second word */
                } else if (r >= VT_CONST || /* XXX: test to VT_CONST incorrect ? */
                           (vtop->r & VT_LVAL)) {
                    /* We do not want to modifier the long long
                       pointer here, so the safest (and less
                       efficient) is to save all the other registers
                       in the stack. XXX: totally inefficient. */
                    save_regs(1);
                    /* load from memory */
                    load(r, vtop);
                    vdup();
                    vtop[-1].r = r; /* save register value */
                    /* increment pointer to get second word */
                    vtop->type.t = VT_INT;
                    gaddrof();
                    vpushi(4);
                    gen_op('+');
                    vtop->r |= VT_LVAL;
                } else {
                    /* move registers */
                    load(r, vtop);
                    vdup();
                    vtop[-1].r = r; /* save register value */
                    vtop->r = vtop[-1].r2;
                }
                /* allocate second register */
                rc2 = RC_INT;
                if (rc == RC_IRET)
                    rc2 = RC_LRET;
                r2 = get_reg(rc2);
                load(r2, vtop);
                vpop();
                /* write second register */
                vtop->r2 = r2;
            } else if ((vtop->r & VT_LVAL) && !is_float(vtop->type.t)) {
                int t1, t;
                /* lvalue of scalar type : need to use lvalue type
                   because of possible cast */
                t = vtop->type.t;
                t1 = t;
                /* compute memory access type */
                if (vtop->r & VT_LVAL_BYTE)
                    t = VT_BYTE;
                else if (vtop->r & VT_LVAL_SHORT)
                    t = VT_SHORT;
                if (vtop->r & VT_LVAL_UNSIGNED)
                    t |= VT_UNSIGNED;
                vtop->type.t = t;
                load(r, vtop);
                /* restore wanted type */
                vtop->type.t = t1;
            } else {
                /* one register type load */
                load(r, vtop);
            }
        }
        vtop->r = r;
//#ifdef TINYCC_TARGET_C67						5 lines added by patch 305 TK  OYOY
        /* uses register pairs for doubles */
//        if ((vtop->type.t & VT_BTYPE) == VT_DOUBLE) 
//            vtop->r2 = r+1;
//#endif
    }
    return r;
}

/* generate vtop[-1] and vtop[0] in resp. classes rc1 and rc2 */
void gv2(int rc1, int rc2)
{
    int v;

    /* generate more generic register first. But VT_JMP or VT_CMP
       values must be generated first in all cases to avoid possible
       reload errors */
    v = vtop[0].r & VT_VALMASK;
    if (v != VT_CMP && (v & ~1) != VT_JMP && rc1 <= rc2) {
        vswap();
        gv(rc1);
        vswap();
        gv(rc2);
        /* test if reload is needed for first register */
        if ((vtop[-1].r & VT_VALMASK) >= VT_CONST) {
            vswap();
            gv(rc1);
            vswap();
        }
    } else {
        gv(rc2);
        vswap();
        gv(rc1);
        vswap();
        /* test if reload is needed for first register */
        if ((vtop[0].r & VT_VALMASK) >= VT_CONST) {
            gv(rc2);
        }
    }
}

/* expand long long on stack in two int registers */
void lexpand(void)
{
    int u;

    u = vtop->type.t & VT_UNSIGNED;
    gv(RC_INT);
    vdup();
    vtop[0].r = vtop[-1].r2;
    vtop[0].r2 = VT_CONST;
    vtop[-1].r2 = VT_CONST;
    vtop[0].type.t = VT_INT | u;
    vtop[-1].type.t = VT_INT | u;
}

/*
#ifdef TINYCC_TARGET_ARM			function added by patch 307 Glockner  OYOY
// expand long long on stack
void lexpand_nr(void)
{
    int u,v;

    u = vtop->type.t & VT_UNSIGNED;
    vdup();
    vtop->r2 = VT_CONST;
    vtop->type.t = VT_INT | u;
    v=vtop[-1].r & (VT_VALMASK | VT_LVAL);
    if (v == VT_CONST) {
      vtop[-1].c.ui = vtop->c.ull;
      vtop->c.ui = vtop->c.ull >> 32;
      vtop->r = VT_CONST;
    } else if (v == (VT_LVAL|VT_CONST) || v == (VT_LVAL|VT_LOCAL)) {
      vtop->c.ui += 4;
      vtop->r = vtop[-1].r;
    } else if (v > VT_CONST) {
      vtop--;
      lexpand();
    } else
      vtop->r = vtop[-1].r2;
    vtop[-1].r2 = VT_CONST;
    vtop[-1].type.t = VT_INT | u;
}
#endif
*/

/* build a long long from two ints */
void lbuild(int t)
{
    gv2(RC_INT, RC_INT);
    vtop[-1].r2 = vtop[0].r;
    vtop[-1].type.t = t;
    vpop();
}

/* rotate n first stack elements to the bottom 
   I1 ... In -> I2 ... In I1 [top is right]
*/
void vrotb(int n)
{
    int i;
    SValue tmp;

    tmp = vtop[-n + 1];
    for(i=-n+1;i!=0;i++)
        vtop[i] = vtop[i+1];
    vtop[0] = tmp;
}

/* rotate n first stack elements to the top 
   I1 ... In -> In I1 ... I(n-1)  [top is right]
 */
void vrott(int n)
{
    int i;
    SValue tmp;

    tmp = vtop[0];
    for(i = 0;i < n - 1; i++)
        vtop[-i] = vtop[-i - 1];
    vtop[-n + 1] = tmp;
}

/*
#ifdef TINYCC_TARGET_ARM			function added by patch 307 Glockner  OYOY
// like vrott but in other direction
//   In ... I1 -> I(n-1) ... I1 In  [top is right]
void vnrott(int n)
{
    int i;
    SValue tmp;

    tmp = vtop[-n + 1];
    for(i = n - 1; i > 0; i--)
        vtop[-i] = vtop[-i + 1];
    vtop[0] = tmp;
}
#endif
*/

/* pop stack value */
void vpop(void)
{
    int v;
    v = vtop->r & VT_VALMASK;
#ifdef TINYCC_TARGET_I386
    /* for x86, we need to pop the FP stack */
    if (v == TREG_ST0) {
        gen_multibyte(0xd8dd); /* fstp %st(0) */
    } else
#endif
    if (v == VT_JMP || v == VT_JMPI) {
        /* need to put correct jump if && or || without test */
        gen_resolve_sym(vtop->c.ul);
    }
    vtop--;
}

/* convert stack entry to register and duplicate its value in another
   register */
void gv_dup(void)
{
    int rc, t, r, r1;
    SValue sv;

    t = vtop->type.t;
    if ((t & VT_BTYPE) == VT_LLONG) {
        lexpand();
        gv_dup();
        vswap();
        vrotb(3);
        gv_dup();
        vrotb(4);
        /* stack: H L L1 H1 */
        lbuild(t);
        vrotb(3);
        vrotb(3);
        vswap();
        lbuild(t);
        vswap();
    } else {
        /* duplicate value */
        rc = RC_INT;
        sv.type.t = VT_INT;
        if (is_float(t)) {
            rc = RC_FLOAT;
            sv.type.t = t;
        }
        r = gv(rc);
        r1 = get_reg(rc);
        sv.r = r;
        sv.c.ul = 0;
        load(r1, &sv); /* move r to r1 */
        vdup();
        /* duplicates value */
        vtop->r = r1;
    }
}

/* generate CPU independent (unsigned) long long operations */
void gen_opl(int op)
{
    int t, a, b, op1, c, i;
    int func;
    SValue tmp;

    switch(op) {
    case '/':
    case TOK_PDIV:
        func = TOK___divdi3;
        goto gen_func;
    case TOK_UDIV:
        func = TOK___udivdi3;
        goto gen_func;
    case '%':
        func = TOK___moddi3;
        goto gen_func;
    case TOK_UMOD:
        func = TOK___umoddi3;
    gen_func:
        /* call generic long long function */
        vpush_global_sym(&func_old_type, func);
        vrott(3);
        gfunc_call(2);
        vpushi(0);
        vtop->r = REG_IRET;
        vtop->r2 = REG_LRET;
        break;
    case '^':
    case '&':
    case '|':
    case '*':
    case '+':
    case '-':
        t = vtop->type.t;
        vswap();
        lexpand();
        vrotb(3);
        lexpand();
        /* stack: L1 H1 L2 H2 */
        tmp = vtop[0];
        vtop[0] = vtop[-3];
        vtop[-3] = tmp;
        tmp = vtop[-2];
        vtop[-2] = vtop[-3];
        vtop[-3] = tmp;
        vswap();
        /* stack: H1 H2 L1 L2 */
        if (op == '*') {
            vpushv(vtop - 1);
            vpushv(vtop - 1);
            gen_op(TOK_UMULL);
            lexpand();
            /* stack: H1 H2 L1 L2 ML MH */
            for(i=0;i<4;i++)
                vrotb(6);
            /* stack: ML MH H1 H2 L1 L2 */
            tmp = vtop[0];
            vtop[0] = vtop[-2];
            vtop[-2] = tmp;
            /* stack: ML MH H1 L2 H2 L1 */
            gen_op('*');
            vrotb(3);
            vrotb(3);
            gen_op('*');
            /* stack: ML MH M1 M2 */
            gen_op('+');
            gen_op('+');
        } else if (op == '+' || op == '-') {
            /* XXX: add non carry method too (for MIPS or alpha) */
            if (op == '+')
                op1 = TOK_ADDC1;
            else
                op1 = TOK_SUBC1;
            gen_op(op1);
            /* stack: H1 H2 (L1 op L2) */
            vrotb(3);
            vrotb(3);
            gen_op(op1 + 1); /* TOK_xxxC2 */
        } else {
            gen_op(op);
            /* stack: H1 H2 (L1 op L2) */
            vrotb(3);
            vrotb(3);
            /* stack: (L1 op L2) H1 H2 */
            gen_op(op);
            /* stack: (L1 op L2) (H1 op H2) */
        }
        /* stack: L H */
        lbuild(t);
        break;
    case TOK_SAR:
    case TOK_SHR:
    case TOK_SHL:
        if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST) {
            t = vtop[-1].type.t;
            vswap();
            lexpand();
            vrotb(3);
            /* stack: L H shift */
            c = (int)vtop->c.i;
            /* constant: simpler */
            /* NOTE: all comments are for SHL. the other cases are
               done by swaping words */
            vpop();
            if (op != TOK_SHL)
                vswap();
            if (c >= 32) {
                /* stack: L H */
                vpop();
                if (c > 32) {
                    vpushi(c - 32);
                    gen_op(op);
                }
                if (op != TOK_SAR) {
                    vpushi(0);
                } else {
                    gv_dup();
                    vpushi(31);
                    gen_op(TOK_SAR);
                }
                vswap();
            } else {
                vswap();
                gv_dup();
                /* stack: H L L */
                vpushi(c);
                gen_op(op);
                vswap();
                vpushi(32 - c);
                if (op == TOK_SHL)
                    gen_op(TOK_SHR);
                else
                    gen_op(TOK_SHL);
                vrotb(3);
                /* stack: L L H */
                vpushi(c);
                if (op == TOK_SHL)
                    gen_op(TOK_SHL);
                else
                    gen_op(TOK_SHR);
                gen_op('|');
            }
            if (op != TOK_SHL)
                vswap();
            lbuild(t);
        } else {
            /* XXX: should provide a faster fallback on x86 ? */
            switch(op) {
            case TOK_SAR:
                func = TOK___sardi3;
                goto gen_func;
            case TOK_SHR:
                func = TOK___shrdi3;
                goto gen_func;
            case TOK_SHL:
                func = TOK___shldi3;
                goto gen_func;
            }
        }
        break;
    default:
        /* compare operations */
        t = vtop->type.t;
        vswap();
        lexpand();
        vrotb(3);
        lexpand();
        /* stack: L1 H1 L2 H2 */
        tmp = vtop[-1];
        vtop[-1] = vtop[-2];
        vtop[-2] = tmp;
        /* stack: L1 L2 H1 H2 */
        /* compare high */
        op1 = op;
        /* when values are equal, we need to compare low words. since
           the jump is inverted, we invert the test too. */
        if (op1 == TOK_LT)
            op1 = TOK_LE;
        else if (op1 == TOK_GT)
            op1 = TOK_GE;
        else if (op1 == TOK_ULT)
            op1 = TOK_ULE;
        else if (op1 == TOK_UGT)
            op1 = TOK_UGE;
        a = 0;
        b = 0;
        gen_op(op1);
        if (op1 != TOK_NE) {
            a = gtst(1, 0);
        }
        if (op != TOK_EQ) {
            /* generate non equal test */
            /* XXX: NOT PORTABLE yet */
            if (a == 0) {
                b = gtst(0, 0);
            } else {
#ifdef TINYCC_TARGET_I386				// original code
//#if defined(TINYCC_TARGET_I386)		replaced by patch 295 Glockner  OYOY
                b = psym(0x850f, 0);
//#elif defined(TINYCC_TARGET_ARM)		3 lines added by patch 295 Glockner  OYOY
//                b = gen_ind;
//                gen_multibyte(0x1A000000 | encbranch(gen_ind, 0, 1));
//#elif defined(TINYCC_TARGET_C67)		2 lines added by patch 305 TK  OYOY
//                error("not implemented");
#else
				error("not implemented");				// original code
//#error not supported						replaced by patch 295 Glockner  OYOY
#endif
            }
        }
        /* compare low. Always unsigned */
        op1 = op;
        if (op1 == TOK_LT)
            op1 = TOK_ULT;
        else if (op1 == TOK_LE)
            op1 = TOK_ULE;
        else if (op1 == TOK_GT)
            op1 = TOK_UGT;
        else if (op1 == TOK_GE)
            op1 = TOK_UGE;
        gen_op(op1);
        a = gtst(1, a);
        gen_resolve_sym(b);
        vseti(VT_JMPI, a);
        break;
    }
}

/* handle long long constant and various machine independent optimizations */
void gen_opic(int op)
{
    int c1, c2, t1, t2, squash;
    SValue *v1, *v2;
    long long l1, l2;

    /* Grab the two symbols to operate on, their types, and their values */
    v1 = vtop - 1;
    v2 = vtop;
    t1 = v1->type.t & VT_BTYPE;
    t2 = v2->type.t & VT_BTYPE;
    l1 = (t1 == VT_LLONG) ? v1->c.ll : v1->c.i;
    l2 = (t2 == VT_LLONG) ? v2->c.ll : v2->c.i;

    /* For forward symbols we can only constify &&, || or == NULL.  If we
       do so, we need to remove VT_SYM from the resulting type. */
    squash = VT_SYM;
    if (op == TOK_LAND || op == TOK_LOR ||
        (op == TOK_EQ && (v1->c.ll == 0 || v2->c.ll == 0)))
    {
        squash = 0;
        if (v1->r & VT_SYM) l1=1;
        if (v2->r & VT_SYM) l2=1;
    } else c2 = VT_SYM;

    /* Are both arguments constant? */

    c1 = (v1->r & (VT_VALMASK | VT_LVAL | squash)) == VT_CONST;
    c2 = (v2->r & (VT_VALMASK | VT_LVAL | squash)) == VT_CONST;

    if (c1 && c2) {
        switch(op) {
        case '+': l1 += l2; break;
        case '-': l1 -= l2; break;
        case '&': l1 &= l2; break;
        case '^': l1 ^= l2; break;
        case '|': l1 |= l2; break;
        case '*': l1 *= l2; break;

        case TOK_PDIV:
        case '/':
        case '%':
        case TOK_UDIV:
        case TOK_UMOD:
            /* if division by zero, generate explicit division */
            if (l2 == 0) {
                if (const_wanted)
                    error("division by zero in constant");
                goto general_case;
            }
            switch(op) {
            default: l1 /= l2; break;
            case '%': l1 %= l2; break;
            case TOK_UDIV: l1 = (unsigned long long)l1 / l2; break;
            case TOK_UMOD: l1 = (unsigned long long)l1 % l2; break;
            }
            break;
        case TOK_SHL: l1 <<= l2; break;
        case TOK_SHR: l1 = (unsigned long long)l1 >> l2; break;
        case TOK_SAR: l1 >>= l2; break;
            /* tests */
        case TOK_ULT: l1 = (unsigned long long)l1 < (unsigned long long)l2; break;
        case TOK_UGE: l1 = (unsigned long long)l1 >= (unsigned long long)l2; break;
        case TOK_EQ: l1 = l1 == l2; break;
        case TOK_NE: l1 = l1 != l2; break;
        case TOK_ULE: l1 = (unsigned long long)l1 <= (unsigned long long)l2; break;
        case TOK_UGT: l1 = (unsigned long long)l1 > (unsigned long long)l2; break;
        case TOK_LT: l1 = l1 < l2; break;
        case TOK_GE: l1 = l1 >= l2; break;
        case TOK_LE: l1 = l1 <= l2; break;
        case TOK_GT: l1 = l1 > l2; break;
            /* logical */
        case TOK_LAND: l1 = l1 && l2; break;
        case TOK_LOR: l1 = l1 || l2; break;
        default:
            goto general_case;
        }
        v1->c.ll = l1;
        // If v1 was a symbol, the resulting type is an int.
        if (!squash) v1->r &= ~VT_SYM;
        vtop--;
    } else {
        /* if commutative ops, put c2 as constant */
        if (c1 && (op == '+' || op == '&' || op == '^' || 
                   op == '|' || op == '*')) {
            vswap();
            swap(&c1, &c2);
            l2 = l1;
        }
        /* Filter out NOP operations like x*1, x-0, x&-1... */
        if (c2 && (((op == '*' || op == '/' || op == TOK_UDIV || 
                     op == TOK_PDIV) && 
                    l2 == 1) ||
                   ((op == '+' || op == '-' || op == '|' || op == '^' || 
                     op == TOK_SHL || op == TOK_SHR || op == TOK_SAR) && 
                    l2 == 0) ||
                   (op == '&' && 
                    l2 == -1))) {
            /* nothing to do */
            vtop--;
        } else if (c2 && (op == '*' || op == TOK_PDIV || op == TOK_UDIV)) {
            /* try to use shifts instead of muls or divs */
            if (l2 > 0 && (l2 & (l2 - 1)) == 0) {
                int n = -1;
                while (l2) {
                    l2 >>= 1;
                    n++;
                }
                vtop->c.ll = n;
                if (op == '*')
                    op = TOK_SHL;
                else if (op == TOK_PDIV)
                    op = TOK_SAR;
                else
                    op = TOK_SHR;
            }
            goto general_case;
        } else if (c2 && (op == '+' || op == '-') &&
                   (vtop[-1].r & (VT_VALMASK | VT_LVAL | VT_SYM)) == 
                   (VT_CONST | VT_SYM)) {
            /* symbol + constant case */
            if (op == '-')
                l2 = -l2;
            vtop--;
            vtop->c.ll += l2;
        } else {
general_case:
            /* call low level op generator */
            if (cur_text_section) {
                if (t1 == VT_LLONG|| t2 == VT_LLONG) gen_opl(op);
                else gen_opi(op);
            } else vtop--;
        }
    }
}

/* generate a floating point operation with constant propagation */
void gen_opif(int op)
{
    int c1, c2;
    SValue *v1, *v2;
    long double f1, f2;

    v1 = vtop - 1;
    v2 = vtop;
    /* currently, we cannot do computations with forward symbols */
    c1 = (v1->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    c2 = (v2->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
    if (c1 && c2) {
        if (v1->type.t == VT_FLOAT) {
            f1 = v1->c.f;
            f2 = v2->c.f;
        } else if (v1->type.t == VT_DOUBLE) {
            f1 = v1->c.d;
            f2 = v2->c.d;
        } else {
            f1 = v1->c.ld;
            f2 = v2->c.ld;
        }

        /* NOTE: we only do constant propagation if finite number (not
           NaN or infinity) (ANSI spec) */
        if (!ieee_finite(f1) || !ieee_finite(f2))
            goto general_case;

        switch(op) {
        case '+': f1 += f2; break;
        case '-': f1 -= f2; break;
        case '*': f1 *= f2; break;
        case '/': 
            if (f2 == 0.0) {
                if (const_wanted)
                    error("division by zero in constant");
                goto general_case;
            }
            f1 /= f2; 
            break;
            /* XXX: also handles tests ? */
        default:
            goto general_case;
        }
        /* XXX: overflow test ? */
        if (v1->type.t == VT_FLOAT) {
            v1->c.f = f1;
        } else if (v1->type.t == VT_DOUBLE) {
            v1->c.d = f1;
        } else {
            v1->c.ld = f1;
        }
        vtop--;
    } else {
    general_case:
        if (cur_text_section) gen_opf(op);
        else vtop--;
    }
}

static int pointed_size(CType *type)
{
    int align;
    return type_size(pointed_type(type), &align);
}

static inline int is_null_pointer(SValue *p)
{
    if ((p->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
        return 0;
    return ((p->type.t & VT_BTYPE) == VT_INT && p->c.i == 0) ||
        ((p->type.t & VT_BTYPE) == VT_LLONG && p->c.ll == 0);
}

static inline int is_integer_btype(int bt)
{
    return (bt == VT_BYTE || bt == VT_SHORT || 
            bt == VT_INT || bt == VT_LLONG);
}

/* check types for comparison or substraction of pointers */
static void check_comparison_pointer_types(SValue *p1, SValue *p2, int op)
{
    CType *type1, *type2, tmp_type1, tmp_type2;
    int bt1, bt2;
    
    /* null pointers are accepted for all comparisons as gcc */
    if (is_null_pointer(p1) || is_null_pointer(p2))
        return;
    type1 = &p1->type;
    type2 = &p2->type;
    bt1 = type1->t & VT_BTYPE;
    bt2 = type2->t & VT_BTYPE;
    /* accept comparison between pointer and integer with a warning */
    if ((is_integer_btype(bt1) || is_integer_btype(bt2)) && op != '-') {
        if (op != TOK_LOR && op != TOK_LAND )
            warning("comparison between pointer and integer");
        return;
    }

    /* both must be pointers or implicit function pointers */
    if (bt1 == VT_PTR) {
        type1 = pointed_type(type1);
    } else if (bt1 != VT_FUNC) 
        goto invalid_operands;

    if (bt2 == VT_PTR) {
        type2 = pointed_type(type2);
    } else if (bt2 != VT_FUNC) { 
    invalid_operands:
        error("invalid operands to binary %s", get_tok_str(op, NULL));
    }
    if ((type1->t & VT_BTYPE) == VT_VOID || 
        (type2->t & VT_BTYPE) == VT_VOID)
        return;
    tmp_type1 = *type1;
    tmp_type2 = *type2;
    tmp_type1.t &= ~(VT_UNSIGNED | VT_CONSTANT | VT_VOLATILE);
    tmp_type2.t &= ~(VT_UNSIGNED | VT_CONSTANT | VT_VOLATILE);
    if (!is_compatible_types(&tmp_type1, &tmp_type2)) {
        /* gcc-like error if '-' is used */
        if (op == '-')
            goto invalid_operands;
        else
            warning("comparison of distinct pointer types lacks a cast");
    }
}

/* generic gen_op: handles types problems */
void gen_op(int op)
{
    int u, t1, t2, bt1, bt2, t;
    CType type1;

    t1 = vtop[-1].type.t;
    t2 = vtop[0].type.t;
    bt1 = t1 & VT_BTYPE;
    bt2 = t2 & VT_BTYPE;

    if (bt1 == VT_PTR || bt2 == VT_PTR) {
        /* at least one operand is a pointer */
        /* relationnal op: must be both pointers */
        if (op >= TOK_ULT && op <= TOK_LOR) {
            check_comparison_pointer_types(vtop - 1, vtop, op);
            /* pointers are handled are unsigned */
            t = VT_INT | VT_UNSIGNED;
            goto std_op;
        }
        /* if both pointers, then it must be the '-' op */
        if (bt1 == VT_PTR && bt2 == VT_PTR) {
            if (op != '-')
                error("cannot use pointers here");
            check_comparison_pointer_types(vtop - 1, vtop, op);
            /* XXX: check that types are compatible */
            u = pointed_size(&vtop[-1].type);
            gen_opic(op);
            /* set to integer type */
            vtop->type.t = VT_INT; 
            vpushi(u);
            gen_op(TOK_PDIV);
        } else {
            /* exactly one pointer : must be '+' or '-'. */
            if (op != '-' && op != '+')
                error("cannot use pointers here");
            /* Put pointer as first operand */
            if (bt2 == VT_PTR) {
                vswap();
                swap(&t1, &t2);
            }
            type1 = vtop[-1].type;
            /* XXX: cast to int ? (long long case) */
            vpushi(pointed_size(&vtop[-1].type));
            gen_op('*');
#ifdef CONFIG_TCC_BCHECK
            /* if evaluating constant expression, no code should be
               generated, so no bound check */
            if (do_bounds_check && !const_wanted) {
                /* if bounded pointers, we generate a special code to
                   test bounds */
                if (op == '-') {
                    vpushi(0);
                    vswap();
                    gen_op('-');
                }
                gen_bounded_ptr_add();
            } else
#endif
            {
                gen_opic(op);
            }
            /* put again type if gen_opic() swaped operands */
            vtop->type = type1;
        }
    } else if (is_float(bt1) || is_float(bt2)) {
        /* compute bigger type and do implicit casts */
        if (bt1 == VT_LDOUBLE || bt2 == VT_LDOUBLE) {
            t = VT_LDOUBLE;
        } else if (bt1 == VT_DOUBLE || bt2 == VT_DOUBLE) {
            t = VT_DOUBLE;
        } else {
            t = VT_FLOAT;
        }
        /* floats can only be used for a few operations */
        if (op != '+' && op != '-' && op != '*' && op != '/' &&
            (op < TOK_ULT || op > TOK_GT))
            error("invalid operands for binary operation");
        goto std_op;
    } else if (bt1 == VT_LLONG || bt2 == VT_LLONG) {
        /* cast to biggest op */
        t = VT_LLONG;
        /* convert to unsigned if it does not fit in a long long */
        if ((t1 & (VT_BTYPE | VT_UNSIGNED)) == (VT_LLONG | VT_UNSIGNED) ||
            (t2 & (VT_BTYPE | VT_UNSIGNED)) == (VT_LLONG | VT_UNSIGNED))
            t |= VT_UNSIGNED;
        goto std_op;
    } else {
        /* integer operations */
        t = VT_INT;
        /* convert to unsigned if it does not fit in an integer */
        if ((t1 & (VT_BTYPE | VT_UNSIGNED)) == (VT_INT | VT_UNSIGNED) ||
            (t2 & (VT_BTYPE | VT_UNSIGNED)) == (VT_INT | VT_UNSIGNED))
            t |= VT_UNSIGNED;
    std_op:
        /* XXX: currently, some unsigned operations are explicit, so
           we modify them here */
        if (t & VT_UNSIGNED) {
            if (op == TOK_SAR)
                op = TOK_SHR;
            else if (op == '/')
                op = TOK_UDIV;
            else if (op == '%')
                op = TOK_UMOD;
            else if (op == TOK_LT)
                op = TOK_ULT;
            else if (op == TOK_GT)
                op = TOK_UGT;
            else if (op == TOK_LE)
                op = TOK_ULE;
            else if (op == TOK_GE)
                op = TOK_UGE;
        }
        vswap();
        type1.t = t;
        gen_cast(&type1);
        vswap();
        /* special case for shifts and long long: we keep the shift as
           an integer */
        if (op == TOK_SHR || op == TOK_SAR || op == TOK_SHL)
            type1.t = VT_INT;
        gen_cast(&type1);
        if (is_float(t))
            gen_opif(op);
        else
            gen_opic(op);
        if (op >= TOK_ULT && op <= TOK_GT) {
            /* relationnal op: the result is an int */
            vtop->type.t = VT_INT;
        } else {
            vtop->type.t = t;
        }
    }
}

/* generic itof for unsigned long long case */
void gen_cvt_itof1(int t)
{
    if ((vtop->type.t & (VT_BTYPE | VT_UNSIGNED)) == 
        (VT_LLONG | VT_UNSIGNED)) {

        if (t == VT_FLOAT)
            vpush_global_sym(&func_old_type, TOK___ulltof);
        else if (t == VT_DOUBLE)
            vpush_global_sym(&func_old_type, TOK___ulltod);
        else
            vpush_global_sym(&func_old_type, TOK___ulltold);
        vrott(2);
        gfunc_call(1);
        vpushi(0);
        vtop->r = REG_FRET;
    } else {
        gen_cvt_itof(t);
    }
}

/* generic ftoi for unsigned long long case */
void gen_cvt_ftoi1(int t)
{
    int st;

    if (t == (VT_LLONG | VT_UNSIGNED)) {
        /* not handled natively */
        st = vtop->type.t & VT_BTYPE;
        if (st == VT_FLOAT)
            vpush_global_sym(&func_old_type, TOK___fixunssfdi);
        else if (st == VT_DOUBLE)
            vpush_global_sym(&func_old_type, TOK___fixunsdfdi);
        else
            vpush_global_sym(&func_old_type, TOK___fixunsxfdi);
        vrott(2);
        gfunc_call(1);
        vpushi(0);
        vtop->r = REG_IRET;
        vtop->r2 = REG_LRET;
    } else {
        gen_cvt_ftoi(t);
    }
}

/* force char or short cast */
void force_charshort_cast(int t)
{
    int bits, dbt;
    dbt = t & VT_BTYPE;
    /* XXX: add optimization if lvalue : just change type and offset */
    if (dbt == VT_BYTE)
        bits = 8;
    else
        bits = 16;
    if (t & VT_UNSIGNED) {
        vpushi((1 << bits) - 1);
        gen_op('&');
    } else {
        bits = 32 - bits;
        vpushi(bits);
        gen_op(TOK_SHL);
        /* result must be signed or the SAR is converted to an SHL
           This was not the case when "t" was a signed short
           and the last value on the stack was an unsigned int
        */
        vtop->type.t &= (~VT_UNSIGNED);
        vpushi(bits);
        gen_op(TOK_SAR);
    }
}

/* cast 'vtop' to 'type'. Casting to bitfields is forbidden. */
static void gen_cast(CType *type)
{
    int sbt, dbt, sf, df, c;

    /* special delayed cast for char/short */
    /* XXX: in some cases (multiple cascaded casts), it may still
       be incorrect */
    if (vtop->r & VT_MUSTCAST) {
        vtop->r &= ~VT_MUSTCAST;
        force_charshort_cast(vtop->type.t);
    }

    /* bitfields first get cast to ints */
    if (vtop->type.t & VT_BITFIELD) {
        gv(RC_INT);
    }

    dbt = type->t & (VT_BTYPE | VT_UNSIGNED);
    sbt = vtop->type.t & (VT_BTYPE | VT_UNSIGNED);

    if (sbt != dbt && cur_text_section) {
        sf = is_float(sbt);
        df = is_float(dbt);
        c = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
        if (sf && df) {
            /* convert from fp to fp */
            if (c) {
                /* constant case: we can do it now */
                /* XXX: in ISOC, cannot do it if error in convert */
                if (dbt == VT_FLOAT && sbt == VT_DOUBLE) 
                    vtop->c.f = (float)vtop->c.d;
                else if (dbt == VT_FLOAT && sbt == VT_LDOUBLE) 
                    vtop->c.f = (float)vtop->c.ld;
                else if (dbt == VT_DOUBLE && sbt == VT_FLOAT) 
                    vtop->c.d = (double)vtop->c.f;
                else if (dbt == VT_DOUBLE && sbt == VT_LDOUBLE) 
                    vtop->c.d = (double)vtop->c.ld;
                else if (dbt == VT_LDOUBLE && sbt == VT_FLOAT) 
                    vtop->c.ld = (long double)vtop->c.f;
                else if (dbt == VT_LDOUBLE && sbt == VT_DOUBLE) 
                    vtop->c.ld = (long double)vtop->c.d;
            } else {
                /* non constant case: generate code */
                gen_cvt_ftof(dbt);
            }
        } else if (df) {
            /* convert int to fp */
            if (c) {
                switch(sbt) {
                case VT_LLONG | VT_UNSIGNED:
                case VT_LLONG:
                    /* XXX: add const cases for long long */
                    goto do_itof;
                case VT_INT | VT_UNSIGNED:
                    switch(dbt) {
                    case VT_FLOAT: vtop->c.f = (float)vtop->c.ui; break;
                    case VT_DOUBLE: vtop->c.d = (double)vtop->c.ui; break;
                    case VT_LDOUBLE: vtop->c.ld = (long double)vtop->c.ui; break;
                    }
                    break;
                default:
                    switch(dbt) {
                    case VT_FLOAT: vtop->c.f = (float)vtop->c.i; break;
                    case VT_DOUBLE: vtop->c.d = (double)vtop->c.i; break;
                    case VT_LDOUBLE: vtop->c.ld = (long double)vtop->c.i; break;
                    }
                    break;
                }
            } else {
            do_itof:
//#if !defined(TINYCC_TARGET_ARM)				added by patch 295 Glockner, and removed, and readded in patch 309 TK  OYOY
                gen_cvt_itof1(dbt);
//#else											3 lines added by patch 295 Glockner (ditto)   OYOY
//                gen_cvt_itof(dbt);
//#endif
            }
        } else if (sf) {
            /* convert fp to int */
            if (dbt == VT_BOOL) {
                 vpushi(0);
                 gen_op(TOK_NE);
            } else {
                /* we handle char/short/etc... with generic code */
                if (dbt != (VT_INT | VT_UNSIGNED) &&
                    dbt != (VT_LLONG | VT_UNSIGNED) &&
                    dbt != VT_LLONG)
                    dbt = VT_INT;
                if (c) {
                    switch(dbt) {
                    case VT_LLONG | VT_UNSIGNED:
                    case VT_LLONG:
                        /* XXX: add const cases for long long */
                        goto do_ftoi;
                    case VT_INT | VT_UNSIGNED:
                        switch(sbt) {
                        case VT_FLOAT: vtop->c.ui = (unsigned int)vtop->c.d; break;
                        case VT_DOUBLE: vtop->c.ui = (unsigned int)vtop->c.d; break;
                        case VT_LDOUBLE: vtop->c.ui = (unsigned int)vtop->c.d; break;
                        }
                        break;
                    default:
                        /* int case */
                        switch(sbt) {
                        case VT_FLOAT: vtop->c.i = (int)vtop->c.d; break;
                        case VT_DOUBLE: vtop->c.i = (int)vtop->c.d; break;
                        case VT_LDOUBLE: vtop->c.i = (int)vtop->c.d; break;
                        }
                        break;
                    }
                } else {
                do_ftoi:
                    gen_cvt_ftoi1(dbt);
                }
                if (dbt == VT_INT && (type->t & (VT_BTYPE | VT_UNSIGNED)) != dbt) {
                    /* additional cast for char/short... */
                    vtop->type.t = dbt;
                    gen_cast(type);
                }
            }
        } else if ((dbt & VT_BTYPE) == VT_LLONG) {
            if ((sbt & VT_BTYPE) != VT_LLONG) {
                /* scalar to long long */
                if (c) {
                    if (sbt == (VT_INT | VT_UNSIGNED))
                        vtop->c.ll = vtop->c.ui;
                    else
                        vtop->c.ll = vtop->c.i;
                } else {
                    /* machine independent conversion */
                    gv(RC_INT);
                    /* generate high word */
                    if (sbt == (VT_INT | VT_UNSIGNED)) {
                        vpushi(0);
                        gv(RC_INT);
                    } else {
                        gv_dup();
                        vpushi(31);
                        gen_op(TOK_SAR);
                    }
                    /* patch second register */
                    vtop[-1].r2 = vtop->r;
                    vpop();
                }
            }
        } else if (dbt == VT_BOOL) {
            /* scalar to bool */
            vpushi(0);
            gen_op(TOK_NE);
        } else if ((dbt & VT_BTYPE) == VT_BYTE || 
                   (dbt & VT_BTYPE) == VT_SHORT) {
            if (sbt == VT_PTR) {
                vtop->type.t = VT_INT;
                warning("nonportable conversion from pointer to char/short");
            }
            force_charshort_cast(dbt);
        } else if ((dbt & VT_BTYPE) == VT_INT) {
            /* scalar to int */
            if (sbt == VT_LLONG) {
                /* from long long: just take low order word */
                lexpand();
                vpop();
            } 
            /* if lvalue and single word type, nothing to do because
               the lvalue already contains the real type size (see
               VT_LVAL_xxx constants) */
        }
    } else if ((dbt & VT_BTYPE) == VT_PTR && !(vtop->r & VT_LVAL)) {
        /* if we are casting between pointer types,
           we must update the VT_LVAL_xxx size */
        vtop->r = (vtop->r & ~VT_LVAL_TYPE)
                  | (lvalue_type(type->ref->type.t) & VT_LVAL_TYPE);
    }
    vtop->type = *type;
}

/* return type size. Put alignment at 'a' */
static int type_size(CType *type, int *a)
{
    Sym *s;
    int bt;

    bt = type->t & VT_BTYPE;
    if (bt == VT_STRUCT) {
        /* struct/union */
        s = type->ref;
        *a = s->r;
        return s->c;
    } else if (bt == VT_PTR) {
        if (type->t & VT_ARRAY) {
            s = type->ref;
            return type_size(&s->type, a) * s->c;
        } else {
            *a = PTR_SIZE;
            return PTR_SIZE;
        }
    } else if (bt == VT_LDOUBLE) {
        *a = LDOUBLE_ALIGN;
        return LDOUBLE_SIZE;
    } else if (bt == VT_DOUBLE || bt == VT_LLONG) {
#ifdef TINYCC_TARGET_I386
        *a = 4;
#elif defined(TINYCC_TARGET_ARM)
#ifdef TCC_ARM_EABI
        *a = 8;
#else
        *a = 4;
#endif
//#else					2 lines added by patch 305 TK  OYOY -- this is the C67 case
//        *a = 8;
#endif
        return 8;
    } else if (bt == VT_INT || bt == VT_ENUM || bt == VT_FLOAT) {
        *a = 4;
        return 4;
    } else if (bt == VT_SHORT) {
        *a = 2;
        return 2;
    } else {
        /* char, void, function, _Bool */
        *a = 1;
        return 1;
    }
}

/* return the pointed type of t */
static inline CType *pointed_type(CType *type)
{
    return &type->ref->type;
}

/* modify type so that its it is a pointer to type. */
static void mk_pointer(CType *type)
{
    Sym *s;
    s = sym_push(SYM_FIELD, type, 0, -1);
    type->t = VT_PTR | (type->t & ~VT_TYPE);
    type->ref = s;
}

/* compare function types. OLD functions match any new functions */
static int is_compatible_func(CType *type1, CType *type2)
{
    Sym *s1, *s2;

    s1 = type1->ref;
    s2 = type2->ref;
    if (!is_compatible_types(&s1->type, &s2->type))
        return 0;
    /* check func_call */
    if (s1->r != s2->r)
        return 0;
    /* XXX: not complete */
    if (s1->c == FUNC_OLD || s2->c == FUNC_OLD)
        return 1;
    if (s1->c != s2->c)
        return 0;
    while (s1 != NULL) {
        if (s2 == NULL)
            return 0;
        if (!is_compatible_types(&s1->type, &s2->type))
            return 0;
        s1 = s1->next;
        s2 = s2->next;
    }
    if (s2)
        return 0;
    return 1;
}

/* return true if type1 and type2 are exactly the same (including
   qualifiers). 

   - enums are not checked as gcc __builtin_types_compatible_p () 
 */
static int is_compatible_types(CType *type1, CType *type2)
{
    int bt1, t1, t2;

    t1 = type1->t & VT_TYPE;
    t2 = type2->t & VT_TYPE;
    /* XXX: bitfields ? */
    if (t1 != t2)
        return 0;
    /* test more complicated cases */
    bt1 = t1 & VT_BTYPE;
    if (bt1 == VT_PTR) {
        type1 = pointed_type(type1);
        type2 = pointed_type(type2);
        return is_compatible_types(type1, type2);
    } else if (bt1 == VT_STRUCT) {
        return (type1->ref == type2->ref);
    } else if (bt1 == VT_FUNC) {
        return is_compatible_func(type1, type2);
    } else {
        return 1;
    }
}

/* print a type. If 'varstr' is not NULL, then the variable is also
   printed in the type */
/* XXX: union */
/* XXX: add array and function pointers */
void type_to_str(char *buf, int buf_size, 
                 CType *type, char *varstr)
{
    int bt, token, t;
    Sym *s, *sa;
    char buf1[256];
    char *tstr;

    t = type->t & VT_TYPE;
    bt = t & VT_BTYPE;
    buf[0] = '\0';
    if (t & VT_CONSTANT)
        pstrcat(buf, buf_size, "const ");
    if (t & VT_VOLATILE)
        pstrcat(buf, buf_size, "volatile ");
    if (t & VT_UNSIGNED)
        pstrcat(buf, buf_size, "unsigned ");
    switch(bt) {
    case VT_VOID:
        tstr = "void";
        goto add_tstr;
    case VT_BOOL:
        tstr = "_Bool";
        goto add_tstr;
    case VT_BYTE:
        tstr = "char";
        goto add_tstr;
    case VT_SHORT:
        tstr = "short";
        goto add_tstr;
    case VT_INT:
        tstr = "int";
        goto add_tstr;
    case VT_LONG:
        tstr = "long";
        goto add_tstr;
    case VT_LLONG:
        tstr = "long long";
        goto add_tstr;
    case VT_FLOAT:
        tstr = "float";
        goto add_tstr;
    case VT_DOUBLE:
        tstr = "double";
        goto add_tstr;
    case VT_LDOUBLE:
        tstr = "long double";
    add_tstr:
        pstrcat(buf, buf_size, tstr);
        break;
    case VT_ENUM:
    case VT_STRUCT:
        if (bt == VT_STRUCT)
            tstr = "struct ";
        else
            tstr = "enum ";
        pstrcat(buf, buf_size, tstr);
        token = type->ref->token & ~SYM_STRUCT;
        pstrcat(buf, buf_size,
            token >= SYM_FIRST_ANOM ? "<anonymous>" : get_tok_str(token, 0));
        break;
    case VT_FUNC:
        s = type->ref;
        type_to_str(buf, buf_size, &s->type, varstr);
        pstrcat(buf, buf_size, "(");
        sa = s->next;
        while (sa != NULL) {
            type_to_str(buf1, sizeof(buf1), &sa->type, NULL);
            pstrcat(buf, buf_size, buf1);
            sa = sa->next;
            if (sa)
                pstrcat(buf, buf_size, ", ");
        }
        pstrcat(buf, buf_size, ")");
        goto no_var;
    case VT_PTR:
        s = type->ref;
        pstrcpy(buf1, sizeof(buf1), "*");
        if (varstr)
            pstrcat(buf1, sizeof(buf1), varstr);
        type_to_str(buf, buf_size, &s->type, buf1);
        goto no_var;
    }
    if (varstr) {
        pstrcat(buf, buf_size, " ");
        pstrcat(buf, buf_size, varstr);
    }
 no_var: ;
}

/* verify type compatibility to store vtop in 'dt' type, and generate
   casts if needed. */
static void gen_assign_cast(CType *dt)
{
    CType *st, *type1, *type2, tmp_type1, tmp_type2;
    char buf1[256], buf2[256];
    int dbt, sbt;

    st = &vtop->type; /* source type */
    dbt = dt->t & VT_BTYPE;
    sbt = st->t & VT_BTYPE;
    if (dt->t & VT_CONSTANT)
        warning("assignment of read-only location");
    switch(dbt) {
    case VT_PTR:
        /* special cases for pointers */
        /* '0' can also be a pointer */
        if (is_null_pointer(vtop))
            goto type_ok;
        /* accept implicit pointer to integer cast with warning */
        if (is_integer_btype(sbt)) {
            warning("assignment makes pointer from integer without a cast");
            goto type_ok;
        }
        type1 = pointed_type(dt);
        /* a function is implicitely a function pointer */
        if (sbt == VT_FUNC) {
            if ((type1->t & VT_BTYPE) != VT_VOID &&
                !is_compatible_types(pointed_type(dt), st))
                goto error;
            else
                goto type_ok;
        }
        if (sbt != VT_PTR)
            goto error;
        type2 = pointed_type(st);
        if ((type1->t & VT_BTYPE) == VT_VOID || 
            (type2->t & VT_BTYPE) == VT_VOID) {
            /* void * can match anything */
        } else {
            /* exact type match, except for unsigned */
            tmp_type1 = *type1;
            tmp_type2 = *type2;
            tmp_type1.t &= ~(VT_UNSIGNED | VT_CONSTANT | VT_VOLATILE);
            tmp_type2.t &= ~(VT_UNSIGNED | VT_CONSTANT | VT_VOLATILE);
            if (!is_compatible_types(&tmp_type1, &tmp_type2))
                warning("assignment from incompatible pointer type");
        }
        /* check const and volatile */
        if ((!(type1->t & VT_CONSTANT) && (type2->t & VT_CONSTANT)) ||
            (!(type1->t & VT_VOLATILE) && (type2->t & VT_VOLATILE)))
            warning("assignment discards qualifiers from pointer target type");
        break;
    case VT_BYTE:
    case VT_SHORT:
    case VT_INT:
    case VT_LLONG:
        if (sbt == VT_PTR || sbt == VT_FUNC) {
            warning("assignment makes integer from pointer without a cast");
        }
        /* XXX: more tests */
        break;
    case VT_STRUCT:
        tmp_type1 = *dt;
        tmp_type2 = *st;
        tmp_type1.t &= ~(VT_CONSTANT | VT_VOLATILE);
        tmp_type2.t &= ~(VT_CONSTANT | VT_VOLATILE);
        if (!is_compatible_types(&tmp_type1, &tmp_type2)) {
        error:
            type_to_str(buf1, sizeof(buf1), st, NULL);
            type_to_str(buf2, sizeof(buf2), dt, NULL);
            error("cannot cast '%s' to '%s'", buf1, buf2);
        }
        break;
    }
 type_ok:
    gen_cast(dt);
}

/* store vtop in lvalue pushed on stack */
void vstore(void)
{
    int sbt, dbt, ft, r, t, size, align, bit_size, bit_pos, rc, delayed_cast;

    ft = vtop[-1].type.t;
    sbt = vtop->type.t & VT_BTYPE;
    dbt = ft & VT_BTYPE;
    if (((sbt == VT_INT || sbt == VT_SHORT) && dbt == VT_BYTE) ||
        (sbt == VT_INT && dbt == VT_SHORT)) {
        /* optimize char/short casts */
        delayed_cast = VT_MUSTCAST;
        vtop->type.t = ft & VT_TYPE;
        /* XXX: factorize */
        if (ft & VT_CONSTANT)
            warning("assignment of read-only location");
    } else {
        delayed_cast = 0;
        if (!(ft & VT_BITFIELD))
            gen_assign_cast(&vtop[-1].type);
    }

    if (sbt == VT_STRUCT) {
        /* if structure, only generate pointer */
        /* structure assignment : generate memcpy */
        /* XXX: optimize if small size */
        if (cur_text_section) {
            size = type_size(&vtop->type, &align);

#ifdef TCC_ARM_EABI
            if(!(align & 7))
                vpush_global_sym(&func_old_type, TOK_memcpy8);
            else if(!(align & 3))
                vpush_global_sym(&func_old_type, TOK_memcpy4);
            else
#endif
            vpush_global_sym(&func_old_type, TOK_memcpy);

            /* destination */
            vpushv(vtop - 2);
            vtop->type.t = VT_INT;
            gaddrof();
            /* source */
            vpushv(vtop - 2);
            vtop->type.t = VT_INT;
            gaddrof();
            /* type size */
            vpushi(size);
            gfunc_call(3);
            
            vswap();
            vpop();
        } else {
            vswap();
            vpop();
        }
        /* leave source on stack */
    } else if (ft & VT_BITFIELD) {
        /* bitfield store handling */
        bit_pos = (ft >> VT_STRUCT_SHIFT) & 0x3f;
        bit_size = (ft >> (VT_STRUCT_SHIFT + 6)) & 0x3f;
        /* remove bit field info to avoid loops */
        vtop[-1].type.t = ft & ~(VT_BITFIELD | (-1 << VT_STRUCT_SHIFT));

        /* duplicate source into other register */
        gv_dup();
        vswap();
        vrott(3);

        /* duplicate destination */
        vdup();
        vtop[-1] = vtop[-2];

        /* mask and shift source */
        vpushi((1 << bit_size) - 1);
        gen_op('&');
        vpushi(bit_pos);
        gen_op(TOK_SHL);
        /* load destination, mask and or with source */
        vswap();
        vpushi(~(((1 << bit_size) - 1) << bit_pos));
        gen_op('&');
        gen_op('|');
        /* store result */
        vstore();

        /* pop off shifted source from "duplicate source..." above */
        vpop();

    } else {
#ifdef CONFIG_TCC_BCHECK
        /* bound check case */
        if (vtop[-1].r & VT_MUSTBOUND) {
            vswap();
            gbound();
            vswap();
        }
#endif
        if (cur_text_section) {
            rc = RC_INT;
            if (is_float(ft))
                rc = RC_FLOAT;
            r = gv(rc);  /* generate value */
            /* if lvalue was saved on stack, must read it */
            if ((vtop[-1].r & VT_VALMASK) == VT_LLOCAL) {
                SValue sv;
                t = get_reg(RC_INT);
                sv.type.t = VT_INT;
                sv.r = VT_LOCAL | VT_LVAL;
                sv.c.ul = vtop[-1].c.ul;
                load(t, &sv);
                vtop[-1].r = t | VT_LVAL;
            }
            store(r, vtop - 1);
            /* two word case handling : store second register at word + 4 */
            if ((ft & VT_BTYPE) == VT_LLONG) {
                vswap();
                /* convert to int to increment easily */
                vtop->type.t = VT_INT;
                gaddrof();
                vpushi(4);
                gen_op('+');
                vtop->r |= VT_LVAL;
                vswap();
                /* XXX: it works because r2 is spilled last ! */
                store(vtop->r2, vtop - 1);
            }
        }
        vswap();
        vtop--; /* NOT vpop() because on x86 it would flush the fp stack */
        vtop->r |= delayed_cast;
    }
}

/* post defines POST/PRE add. c is the token ++ or -- */
void inc(int post, int c)
{
    test_lvalue();
    vdup(); /* save lvalue */
    if (post) {
        gv_dup(); /* duplicate value */
        vrotb(3);
        vrotb(3);
    }
    /* add constant */
    vpushi(c - TOK_MID); 
    gen_op('+');
    vstore(); /* store value */
    if (post)
        vpop(); /* if post op, return saved value */
}

/* Parse GNUC __attribute__ extension. Currently, the following
   extensions are recognized:
   - aligned(n) : set data/function alignment.
   - packed : force data alignment to 1
   - section(x) : generate data/code in this section.
   - unused : currently ignored, but may be used someday.
   - regparm(n) : pass function parameters in registers (i386 only)
 */
static void parse_attribute(AttributeDef *ad)
{
    int t, n;
    
    while (tok == TOK_ATTRIBUTE1 || tok == TOK_ATTRIBUTE2) {
    next();
    skip('(');
    skip('(');
    while (tok != ')') {
        if (tok < TOK_IDENT)
            expect("attribute name");
        t = tok;
        next();
        switch(t) {
        case TOK_SECTION1:
        case TOK_SECTION2:
            skip('(');
            if (tok != TOK_STR)
                expect("section name");
            ad->section = find_section(tcc_state, (char *)tokc.cstr->data);
            next();
            skip(')');
            break;
        case TOK_ALIGNED1:
        case TOK_ALIGNED2:
            if (tok == '(') {
                next();
                n = expr_const();
                if (n <= 0 || (n & (n - 1)) != 0) 
                    error("alignment must be a positive power of two");
                skip(')');
            } else {
                n = MAX_ALIGN;
            }
            ad->aligned = n;
            break;
        case TOK_PACKED1:
        case TOK_PACKED2:
            ad->packed = 1;
            break;
        case TOK_UNUSED1:
        case TOK_UNUSED2:
            /* currently, no need to handle it because tcc does not
               track unused objects */
            break;
        case TOK_NORETURN1:
        case TOK_NORETURN2:
            /* currently, no need to handle it because tcc does not
               track unused objects */
            break;
        case TOK_CDECL1:
        case TOK_CDECL2:
        case TOK_CDECL3:
            ad->func_call = FUNC_CDECL;
            break;
        case TOK_STDCALL1:
        case TOK_STDCALL2:
        case TOK_STDCALL3:
            ad->func_call = FUNC_STDCALL;
            break;
#ifdef TINYCC_TARGET_I386
        case TOK_REGPARM1:
        case TOK_REGPARM2:
            skip('(');
            n = expr_const();
            if (n > 3) 
                n = 3;
            else if (n < 0)
                n = 0;
            if (n > 0)
                ad->func_call = FUNC_FASTCALL1 + n - 1;
            skip(')');
            break;
//        case TOK_FASTCALL1:			5 lines added by patch 387 Navara  OYOY
//        case TOK_FASTCALL2:
//        case TOK_FASTCALL3:
//            ad->func_call = FUNC_FASTCALLW;
//            break;            
#endif
        case TOK_DLLEXPORT:
            ad->dllexport = 1;
            break;
        default:
            if (tccg_warn_unsupported)
                warning("'%s' attribute ignored", get_tok_str(t, NULL));
            /* skip parameters */
            if (tok == '(') {
                int parenthesis = 0;
                do {
                    if (tok == '(') parenthesis++;
                    else if (tok == ')') parenthesis--;
                    next();
                } while (parenthesis && tok != -1);
            }
            break;
        }
        if (tok != ',')
            break;
        next();
    }
    skip(')');
    skip(')');
    }
}

/* enum/struct/union declaration. u is either VT_ENUM or VT_STRUCT */
static void struct_decl(CType *type, int u)
{
    int a, v, size, align, maxalign, c, offset;
    int bit_size, bit_pos, bsize, bt, lbit_pos;
    Sym *s, *ss, **ps;				// , *ass -- variable added by patch 389 Navara  OYOY
    AttributeDef ad;
    CType type1, btype;

    a = tok; /* save decl type */
    next();
    if (tok != '{') {
        v = tok;
        next();
        /* struct already defined ? return it */
        if (v < TOK_IDENT)
            expect("struct/union/enum name");
        s = struct_find(v);
        if (s) {
            if (s->type.t != a)
                error("invalid type");
            goto do_decl;
        }
    } else {
        v = anon_sym++;
    }
    type1.t = a;
    /* we put an undefined size for struct/union */
    s = sym_push(v | SYM_STRUCT, &type1, 0, -1);
    s->r = 0; /* default alignment is zero as gcc */
    /* put struct/union/enum name in type */
 do_decl:
    type->t = u;
    type->ref = s;
    
    if (tok == '{') {
        next();
        if (s->c != -1)
            error("struct/union/enum already defined");
        /* cannot be empty */
        c = 0;
        /* non empty enums are not allowed */
        if (a == TOK_ENUM) {
            for(;;) {
                v = tok;
                if (v < TOK_UIDENT)
                    expect("identifier");
                next();
                if (tok == '=') {
                    next();
                    c = expr_const();
                }
                /* enum symbols have static storage */
                ss = sym_push(v, &int_type, VT_CONST, c);
                ss->type.t |= VT_STATIC;
                if (tok != ',')
                    break;
                next();
                c++;
                /* NOTE: we accept a trailing comma */
                if (tok == '}')
                    break;
            }
            skip('}');
        } else {
            maxalign = 1;
            ps = &s->next;
            bit_pos = 0;
            offset = 0;
            while (tok != '}') {
                parse_btype(&btype, &ad);
                while (1) {
                    bit_size = -1;
                    v = 0;
                    type1 = btype;
                    if (tok != ':') {
                        type_decl(&type1, &ad, &v, TYPE_DIRECT);					// original code
//                        type_decl(&type1, &ad, &v, TYPE_DIRECT | TYPE_ABSTRACT);		3 lines to replace 1 line, patch 389 Navara  OYOY
//                        if (v == 0 && (type1.t & VT_BTYPE) != VT_STRUCT)
//                            expect("identifier");
                        if ((type1.t & VT_BTYPE) == VT_FUNC ||
                            (type1.t & (VT_TYPEDEF | VT_STATIC | VT_EXTERN | VT_INLINE)))
                            error("invalid type for '%s'", 
                                  get_tok_str(v, NULL));
                    }
                    if (tok == ':') {
                        next();
                        bit_size = expr_const();
                        /* XXX: handle v = 0 case for messages */
                        if (bit_size < 0)
                            error("negative width in bit-field '%s'", 
                                  get_tok_str(v, NULL));
                        if (v && bit_size == 0)
                            error("zero width for bit-field '%s'", 
                                  get_tok_str(v, NULL));
                    }
                    size = type_size(&type1, &align);
                    if (ad.aligned) {
                        if (align < ad.aligned)
                            align = ad.aligned;
                    } else if (ad.packed) {
                        align = 1;
//                    } else if (*tcc_state->pack_stack_ptr) {				added by patch 354 Grischka  OYOY
//                        if (align > *tcc_state->pack_stack_ptr)
//                            align = *tcc_state->pack_stack_ptr;
                    }
                    lbit_pos = 0;
                    if (bit_size >= 0) {
                        bt = type1.t & VT_BTYPE;
                        if (bt != VT_INT && 
                            bt != VT_BYTE && 
                            bt != VT_SHORT &&
                            bt != VT_BOOL &&
                            bt != VT_ENUM)
                            error("bitfields must have scalar type");
                        bsize = size * 8;
                        if (bit_size > bsize) {
                            error("width of '%s' exceeds its type",
                                  get_tok_str(v, NULL));
                        } else if (bit_size == bsize) {
                            /* no need for bit fields */
                            bit_pos = 0;
                        } else if (bit_size == 0) {
                            /* XXX: what to do if only padding in a
                               structure ? */
                            /* zero size: means to pad */
                            if (bit_pos > 0)
                                bit_pos = bsize;
                        } else {
                            /* we do not have enough room ? */
                            if ((bit_pos + bit_size) > bsize)
                                bit_pos = 0;
                            lbit_pos = bit_pos;
                            /* XXX: handle LSB first */
                            type1.t |= VT_BITFIELD | 
                                (bit_pos << VT_STRUCT_SHIFT) |
                                (bit_size << (VT_STRUCT_SHIFT + 6));
                            bit_pos += bit_size;
                        }
                    } else {
                        bit_pos = 0;
                    }
                    if (v) {		// original code
//                    if (v != 0 || (type1.t & VT_BTYPE) == VT_STRUCT) {		modded by patch 389 Navara  OYOY
                        /* add new memory data only if starting
                           bit field */
                        if (lbit_pos == 0) {
                            if (a == TOK_STRUCT) {
                                c = (c + align - 1) & -align;
                                offset = c;
                                if (size > 0)
                                    c += size;
                            } else {
                                offset = 0;
                                if (size > c)
                                    c = size;
                            }
                            if (align > maxalign)
                                maxalign = align;
                        }
#if 0
                        printf("add field %s offset=%d", 
                               get_tok_str(v, NULL), offset);
                        if (type1.t & VT_BITFIELD) {
                            printf(" pos=%d size=%d", 
                                   (type1.t >> VT_STRUCT_SHIFT) & 0x3f,
                                   (type1.t >> (VT_STRUCT_SHIFT + 6)) & 0x3f);
                        }
                        printf("\n");
#endif
//                    }						8 lines added by patch 389 Navara  OYOY
//                    if (v == 0 && (type1.t & VT_BTYPE) == VT_STRUCT) {
//                        ass = type1.ref;
//                        while ((ass = ass->next) != NULL) {
//                           ss = sym_push(ass->token, &ass->type, 0, offset + ass->c);
//                           *ps = ss;
//                           ps = &ss->next;
//                        }
//                    } else if (v) {
                        ss = sym_push(v | SYM_FIELD, &type1, 0, offset);
                        *ps = ss;
                        ps = &ss->next;
                    }
                    if (tok == ';' || tok == TOK_EOF)
                        break;
                    skip(',');
                }
                skip(';');
            }
            skip('}');
            /* store size and alignment */
            s->c = (c + maxalign - 1) & -maxalign; 
            s->r = maxalign;
        }
    }
}

/* return 0 if no type declaration. otherwise, return the basic type
   and skip it. 
 */
static int parse_btype(CType *type, AttributeDef *ad)
{
    int t, u, type_found, typedef_found;		// , typespec_found -- this variable was added by patch 282 mauro persano OYOY
    Sym *s;
    CType type1;

    memset(ad, 0, sizeof(AttributeDef));
    type_found = 0;
//    typespec_found = 0;		added by patch 282 mauro persano OYOY
    typedef_found = 0;
    t = 0;
    while(1) {
        switch(tok) {
        case TOK_EXTENSION:
            /* currently, we really ignore extension */
            next();
            continue;

            /* basic types */
        case TOK_CHAR:
            u = VT_BYTE;
        basic_type:
            next();
        basic_type1:
            if ((t & VT_BTYPE) != 0)
                error("too many basic types");
            t |= u;
//            typespec_found = 1;		added by patch 282 mauro persano OYOY
            break;
        case TOK_VOID:
            u = VT_VOID;
            goto basic_type;
        case TOK_SHORT:
            u = VT_SHORT;
            goto basic_type;
        case TOK_INT:
            next();
//            typespec_found = 1;		added by patch 282 mauro persano OYOY
            break;
        case TOK_LONG:
            next();
            if ((t & VT_BTYPE) == VT_DOUBLE) {
                t = (t & ~VT_BTYPE) | VT_LDOUBLE;
            } else if ((t & VT_BTYPE) == VT_LONG) {
                t = (t & ~VT_BTYPE) | VT_LLONG;
            } else {
                u = VT_LONG;
                goto basic_type1;
            }
            break;
        case TOK_BOOL:
            u = VT_BOOL;
            goto basic_type;
        case TOK_FLOAT:
            u = VT_FLOAT;
            goto basic_type;
        case TOK_DOUBLE:
            next();
            if ((t & VT_BTYPE) == VT_LONG) {
                t = (t & ~VT_BTYPE) | VT_LDOUBLE;
            } else {
                u = VT_DOUBLE;
                goto basic_type1;
            }
            break;
        case TOK_ENUM:
            struct_decl(&type1, VT_ENUM);
        basic_type2:
            u = type1.t;
            type->ref = type1.ref;
            goto basic_type1;
        case TOK_STRUCT:
        case TOK_UNION:
            struct_decl(&type1, VT_STRUCT);
            goto basic_type2;

            /* type modifiers */
        case TOK_CONST1:
        case TOK_CONST2:
        case TOK_CONST3:
            t |= VT_CONSTANT;
            next();
            break;
        case TOK_VOLATILE1:
        case TOK_VOLATILE2:
        case TOK_VOLATILE3:
            t |= VT_VOLATILE;
            next();
            break;
        case TOK_REGISTER:		// deleted by patch 282 mauro persano OYOY
        case TOK_SIGNED1:
        case TOK_SIGNED2:
        case TOK_SIGNED3:
//            typespec_found = 1;		added by patch 282 mauro persano OYOY
//            t |= VT_SIGNED;			3 lines added by patch 295 Glockner
//            next();
//            break;
//        case TOK_REGISTER:		moved by patch 282 mauro persano OYOY
        case TOK_AUTO:
        case TOK_RESTRICT1:
        case TOK_RESTRICT2:
        case TOK_RESTRICT3:
            next();
            break;
        case TOK_UNSIGNED:
            t |= VT_UNSIGNED;
            next();
//            typespec_found = 1;		added by patch 282 mauro persano OYOY
            break;

            /* storage */
        case TOK_EXTERN:
            t |= VT_EXTERN;
            next();
            break;
        case TOK_STATIC:
            t |= VT_STATIC;
            next();
            break;
        case TOK_TYPEDEF:
            t |= VT_TYPEDEF;
            next();
            break;
        case TOK_INLINE1:
        case TOK_INLINE2:
        case TOK_INLINE3:
            t |= VT_INLINE;
            next();
            break;

            /* GNUC attribute */
        case TOK_ATTRIBUTE1:
        case TOK_ATTRIBUTE2:
            parse_attribute(ad);
            break;
            /* GNUC typeof */
        case TOK_TYPEOF1:
        case TOK_TYPEOF2:
        case TOK_TYPEOF3:
            next();
            parse_expr_type(&type1);
            goto basic_type2;
        default:
//            if (typespec_found || typedef_found)		typespec_found added by patch 282 mauro persano OYOY
//                goto the_end;							added by patch 282 mauro persano OYOY
            s = sym_find(tok);
            if (!s || !(s->type.t & VT_TYPEDEF) || typedef_found)		// OYOY I modified this line to preserve the logic for typedef_found when removing patch 282
//            if (!s || !(s->type.t & VT_TYPEDEF))		-- previous code
                goto the_end;
            typedef_found = 1;
            t |= (s->type.t & ~VT_TYPEDEF);
            type->ref = s->type.ref;
            next();
            typespec_found = 1;
            break;
        }
        type_found = 1;
    }
the_end:
/*    if ((t & (VT_SIGNED|VT_UNSIGNED)) == (VT_SIGNED|VT_UNSIGNED))			these 7 lines added by patch 295 (Glockner)  OYOY
      error("signed and unsigned modifier");
    if (tccg_char_is_unsigned) {
        if ((t & (VT_SIGNED|VT_UNSIGNED|VT_BTYPE)) == VT_BYTE)
            t |= VT_UNSIGNED;
    }
    t &= ~VT_SIGNED;		*/

    /* long is never used as type */
    if ((t & VT_BTYPE) == VT_LONG)
        t = (t & ~VT_BTYPE) | VT_INT;
    type->t = t;
    return type_found;
}

/* convert a function parameter type (array to pointer and function to
   function pointer) */
static inline void convert_parameter_type(CType *pt)
{
    /* remove const and volatile qualifiers (XXX: const could be used
       to indicate a const function parameter */
    pt->t &= ~(VT_CONSTANT | VT_VOLATILE);
    /* array must be transformed to pointer according to ANSI C */
    pt->t &= ~VT_ARRAY;
    if ((pt->t & VT_BTYPE) == VT_FUNC) {
        mk_pointer(pt);
    }
}

static void parse_function_parameters(CType *type, AttributeDef *ad)
{
    int n, l, t1;
    Sym **plast, *s, *first;
    AttributeDef ad1;
    CType pt;

    /* Starting with '(' parse attributes for function declaration */
    next();
    l = 0;
    first = NULL;
    plast = &first;
    if (tok != ')') {
        for(;;) {
            /* read param name and compute offset */
            if (l != FUNC_OLD) {
                if (!parse_btype(&pt, &ad1)) {
                    if (l) {
                        error("invalid type");
                    } else {
                        l = FUNC_OLD;
                        goto old_proto;
                    }
                }
                l = FUNC_NEW;
                if ((pt.t & VT_BTYPE) == VT_VOID && tok == ')')
                    break;
                type_decl(&pt, &ad1, &n, TYPE_DIRECT | TYPE_ABSTRACT);
                if ((pt.t & VT_BTYPE) == VT_VOID)
                    error("parameter declared as void");
            } else {
            old_proto:
                n = tok;
                if (n < TOK_UIDENT)
                    expect("identifier");
                pt.t = VT_INT;
                next();
            }
            convert_parameter_type(&pt);
            s = sym_push(n | SYM_FIELD, &pt, 0, 0);
            *plast = s;
            plast = &s->next;
            if (tok == ')')
                break;
            skip(',');
            if (l == FUNC_NEW && tok == TOK_DOTS) {
                l = FUNC_ELLIPSIS;
                next();
                break;
            }
        }
    }
    /* if no parameters, then old type prototype */
    if (!l) l = FUNC_OLD;
    skip(')');
    t1 = type->t & VT_STORAGE;
    /* NOTE: const is ignored in returned type as it has a special
       meaning in gcc / C++ */
    type->t &= ~(VT_STORAGE | VT_CONSTANT); 
    /* we push a anonymous symbol which will contain the function prototype */
    s = sym_push(SYM_FIELD, type, ad->func_call, l);
    s->next = first;
    type->t = t1 | VT_FUNC;
    type->ref = s;
}

static void parse_array_dimensions(CType *type)
{
    int n = -1, t1;
    Sym *s;

    /* Starting with '[' parse array dimensions */
    next();
    if (tok == TOK_RESTRICT1) next(); 
    if (tok != ']') {
        char *message = "invalid array size";
        expr_const1();
        /* Conventional constant array? */
        if ((vtop->r & (VT_VALMASK |VT_LVAL | VT_SYM)) == VT_CONST) {
            n = vtop->c.i;
            vpop();

            if (n < 0) error(message);
        } else if (!local_stack) error(message);
        else {
            gen_assign_cast(&int_type);
            n = -2;
            error("dynamic arrays not implemented yet");
        }
    }
    skip(']');
    /* parse next post type */
    t1 = type->t & VT_STORAGE;
    type->t &= ~VT_STORAGE;
    if (tok == '[') parse_array_dimensions(type);
    
    /* we push a anonymous symbol which will contain the array
       element type */
    s = sym_push(SYM_FIELD, type, 0, n);
    type->t = t1 | VT_ARRAY | VT_PTR;
    type->ref = s;
}

/* Parse a type declaration (except basic type), and return the type
   in 'type'. 'td' is a bitmask indicating which kind of type decl is
   expected. 'type' should contain the basic type. 'ad' is the
   attribute definition of the basic type. It can be modified by
   type_decl(). 
 */
static void type_decl(CType *type, AttributeDef *ad, int *token, int td)
{
    Sym *s;
    CType type1, *type2;
    int qualifiers;
    
    while (tok == '*') {
        qualifiers = 0;
    redo:
        next();
        switch(tok) {
        case TOK_CONST1:
        case TOK_CONST2:
        case TOK_CONST3:
            qualifiers |= VT_CONSTANT;
            goto redo;
        case TOK_VOLATILE1:
        case TOK_VOLATILE2:
        case TOK_VOLATILE3:
            qualifiers |= VT_VOLATILE;
            goto redo;
        case TOK_RESTRICT1:
        case TOK_RESTRICT2:
        case TOK_RESTRICT3:
            goto redo;
        }
        mk_pointer(type);
        type->t |= qualifiers;
    }
    
    /* XXX: clarify attribute handling */
    if (tok == TOK_ATTRIBUTE1 || tok == TOK_ATTRIBUTE2)
        parse_attribute(ad);

    /* recursive type */
    /* XXX: incorrect if abstract type for functions (e.g. 'int ()') */
    type1.t = 0; /* XXX: same as int */
    if (tok == '(') {
        next();
        /* XXX: this is not correct to modify 'ad' at this point, but
           the syntax is not clear */
        if (tok == TOK_ATTRIBUTE1 || tok == TOK_ATTRIBUTE2)
            parse_attribute(ad);
        type_decl(&type1, ad, token, td);
        skip(')');
    } else {
        /* type identifier */
        if (tok >= TOK_IDENT && (td & TYPE_DIRECT)) {
            *token = tok;
            next();
        } else {
            if (!(td & TYPE_ABSTRACT))
                expect("identifier");
            *token = 0;
        }
    }
    if (tok == '(') parse_function_parameters(type, ad);
    else if (tok == '[') parse_array_dimensions(type);
    if (tok == TOK_ATTRIBUTE1 || tok == TOK_ATTRIBUTE2)
        parse_attribute(ad);
    if (!type1.t)
        return;
    /* append type at the end of type1 */
    type2 = &type1;
    for(;;) {
        s = type2->ref;
        type2 = &s->type;
        if (!type2->t) {
            *type2 = *type;
            break;
        }
    }
    *type = type1;
}

/* compute the lvalue VT_LVAL_xxx needed to match type t. */
static int lvalue_type(int t)
{
    int bt, r;
    r = VT_LVAL;
    bt = t & VT_BTYPE;
    if (bt == VT_BYTE || bt == VT_BOOL)
        r |= VT_LVAL_BYTE;
    else if (bt == VT_SHORT)
        r |= VT_LVAL_SHORT;
    else
        return r;
    if (t & VT_UNSIGNED)
        r |= VT_LVAL_UNSIGNED;
    return r;
}

/* indirection with full error checking and bound check */
static void indir(void)
{
    if ((vtop->type.t & VT_BTYPE) != VT_PTR) {
        if ((vtop->type.t & VT_BTYPE) == VT_FUNC)
            return;
        expect("pointer");
    }
    if ((vtop->r & VT_LVAL) && cur_text_section)
        gv(RC_INT);
    vtop->type = *pointed_type(&vtop->type);
    /* Arrays and functions are never lvalues */
    if (!(vtop->type.t & VT_ARRAY)
        && (vtop->type.t & VT_BTYPE) != VT_FUNC) {
        vtop->r |= lvalue_type(vtop->type.t);
        /* if bound checking, the referenced pointer must be checked */
        if (do_bounds_check) 
            vtop->r |= VT_MUSTBOUND;
    }
}

/* pass a parameter to a function and do type checking and casting */
static void gfunc_param_typed(Sym *func, Sym *arg)
{
    int func_type;
    CType type;

    func_type = func->c;
    if (func_type == FUNC_OLD ||
        (func_type == FUNC_ELLIPSIS && arg == NULL)) {
        /* default casting : only need to convert float to double */
        if ((vtop->type.t & VT_BTYPE) == VT_FLOAT) {
            type.t = VT_DOUBLE;
            gen_cast(&type);
        }
    } else if (arg == NULL) {
        error("too many arguments to function");
    } else {
        type = arg->type;
        type.t &= ~VT_CONSTANT; /* need to do that to avoid false warning */
        gen_assign_cast(&type);
    }
}

/* parse an expression of the form '(type)' or '(expr)' and return its
   type */
static void parse_expr_type(CType *type)
{
    int n;
    AttributeDef ad;

    skip('(');
    if (parse_btype(type, &ad)) {
        type_decl(type, &ad, &n, TYPE_ABSTRACT);
    } else {
        expr_type(type);
    }
    skip(')');
}

static void parse_type(CType *type)
{
    AttributeDef ad;
    int n;

    if (!parse_btype(type, &ad)) {
        expect("type");
    }
    type_decl(type, &ad, &n, TYPE_ABSTRACT);
}

static void vpush_tokc(int t)
{
    CType type;
    type.t = t;
    vsetc(&type, VT_CONST, &tokc);
}

static void unary(void)
{
    int n, t, align, size, r;
    CType type;
    Sym *s;
    AttributeDef ad;

    /* XXX: GCC 2.95.3 does not generate a table although it should be
       better here */
 tok_next:
    switch(tok) {
    case TOK_EXTENSION:
        next();
        goto tok_next;
    case TOK_CINT:
    case TOK_CCHAR: 
    case TOK_LCHAR:
        vpushi(tokc.i);
        next();
        break;
    case TOK_CUINT:
        vpush_tokc(VT_INT | VT_UNSIGNED);
        next();
        break;
    case TOK_CLLONG:
        vpush_tokc(VT_LLONG);
        next();
        break;
    case TOK_CULLONG:
        vpush_tokc(VT_LLONG | VT_UNSIGNED);
        next();
        break;
    case TOK_CFLOAT:
        vpush_tokc(VT_FLOAT);
        next();
        break;
    case TOK_CDOUBLE:
        vpush_tokc(VT_DOUBLE);
        next();
        break;
    case TOK_CLDOUBLE:
        vpush_tokc(VT_LDOUBLE);
        next();
        break;
    case TOK___FUNCTION__:
        if (!gnu_ext)
            goto tok_identifier;
        /* fall thru */
    case TOK___FUNC__:
        {
            void *ptr;
            int len;
            /* special function name identifier */
            len = strlen(funcname) + 1;
            /* generate char[len] type */
            type.t = VT_BYTE;
            mk_pointer(&type);
            type.t |= VT_ARRAY;
            type.ref->c = len;
            vpush_ref(&type, data_section, data_section->data_offset, len);
            ptr = section_ptr_add(data_section, len);
            memcpy(ptr, funcname, len);
            next();
        }
        break;
    case TOK_LSTR:
//#ifdef TINYCC_TARGET_PE				4 lines added by patch 384 Navara  OYOY
//        t = VT_SHORT | VT_UNSIGNED;
//#else
        t = VT_INT;
//#endif
        goto str_init;
    case TOK_STR:
        /* string parsing */
        t = VT_BYTE;
    str_init:
        if (tccg_warn_write_strings)
            t |= VT_CONSTANT;
        type.t = t;
        mk_pointer(&type);
        type.t |= VT_ARRAY;
        memset(&ad, 0, sizeof(AttributeDef));
        decl_initializer_alloc(&type, &ad, VT_CONST, 2, 0, 0);
        break;
    case '(':
        next();
        /* cast ? */
        if (parse_btype(&type, &ad)) {
            type_decl(&type, &ad, &n, TYPE_ABSTRACT);
            skip(')');
            /* check ISOC99 compound literal */
            if (tok == '{') {
                    /* data is allocated locally by default */
                if (global_expr)
                    r = VT_CONST;
                else
                    r = VT_LOCAL;
                /* all except arrays are lvalues */
                if (!(type.t & VT_ARRAY))
                    r |= lvalue_type(type.t);
                memset(&ad, 0, sizeof(AttributeDef));
                decl_initializer_alloc(&type, &ad, r, 1, 0, 0);
            } else {
                unary();
                gen_cast(&type);
            }
        } else if (tok == '{') {
            /* save all registers */
            save_regs(0); 
            /* statement expression : we do not accept break/continue
               inside as GCC does */
            block(NULL, NULL, NULL, NULL, 0, 1);
            skip(')');
        } else {
            gexpr();
            skip(')');
        }
        break;
    case '*':
        next();
        unary();
        indir();
        break;
    case '&':
        next();
        unary();
        /* functions names must be treated as function pointers,
           except for unary '&' and sizeof. Since we consider that
           functions are not lvalues, we only have to handle it
           there and in function calls. */
        /* arrays can also be used although they are not lvalues */
        if ((vtop->type.t & VT_BTYPE) != VT_FUNC &&
            !(vtop->type.t & VT_ARRAY) && !(vtop->type.t & VT_LLOCAL))
            test_lvalue();
        mk_pointer(&vtop->type);
        gaddrof();
        break;
    case '!':
        next();
        unary();
        if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST)
            vtop->c.i = !vtop->c.i;
        else if ((vtop->r & VT_VALMASK) == VT_CMP)
            vtop->c.i = vtop->c.i ^ 1;
        else {
            save_regs(1);
            vseti(VT_JMP, gtst(1, 0));
        }
        break;
    case '~':
        next();
        unary();
        vpushi(-1);
        gen_op('^');
        break;
    case '+':
        next();
        /* in order to force cast, we add zero */
        unary();
        if ((vtop->type.t & VT_BTYPE) == VT_PTR)
            error("pointer not accepted for unary plus");
        vpushi(0);
        gen_op('+');
        break;
    case TOK_SIZEOF:
    case TOK_ALIGNOF1:
    case TOK_ALIGNOF2:
        t = tok;
        next();
        if (tok == '(') {
            parse_expr_type(&type);
        } else {
            unary_type(&type);
        }
        size = type_size(&type, &align);
        if (t == TOK_SIZEOF) {
            if (size < 0)
                error("sizeof applied to an incomplete type");
            vpushi(size);
        } else {
            vpushi(align);
        }
        vtop->type.t |= VT_UNSIGNED;
        break;

    case TOK_builtin_types_compatible_p:
        {
            CType type1, type2;
            next();
            skip('(');
            parse_type(&type1);
            skip(',');
            parse_type(&type2);
            skip(')');
            type1.t &= ~(VT_CONSTANT | VT_VOLATILE);
            type2.t &= ~(VT_CONSTANT | VT_VOLATILE);
            vpushi(is_compatible_types(&type1, &type2));
        }
        break;
    case TOK_builtin_constant_p:
        {
            Section *saved_text_section;
            int res;
            next();
            skip('(');
            saved_text_section = cur_text_section;
            cur_text_section = NULL;
            gexpr();
            res = (vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST;
            vpop();
            cur_text_section = saved_text_section;
            skip(')');
            vpushi(res);
        }
        break;
    case TOK_INC:
    case TOK_DEC:
        t = tok;
        next();
        unary();
        inc(0, t);
        break;
    case '-':
        next();
        vpushi(0);
        unary();
        gen_op('-');
        break;
    case TOK_LAND:
        if (!gnu_ext)
            goto tok_identifier;
        next();
        /* allow to take the address of a label */
        if (tok < TOK_UIDENT)
            expect("label identifier");
        s = label_find(tok);
        if (!s) {
            s = label_push(&global_label_stack, tok, LABEL_FORWARD);
        } else {
            if (s->r == LABEL_DECLARED)
                s->r = LABEL_FORWARD;
        }
        if (!s->type.t) {
            s->type.t = VT_VOID;
            mk_pointer(&s->type);
            s->type.t |= VT_STATIC;
        }
        vset(&s->type, VT_CONST | VT_SYM, 0);
        vtop->sym = s;
        next();
        break;
    default:
    tok_identifier:
        t = tok;
        next();
        if (t < TOK_UIDENT)
            expect("identifier");
        s = sym_find(t);
        if (!s) {
            if (tok != '(')
                error("'%s' undeclared", get_tok_str(t, NULL));
            /* for simple function calls, we tolerate undeclared
               external reference to int() function */
            if (tccg_warn_implicit_function_declaration)
                warning("implicit declaration of function '%s'",
                        get_tok_str(t, NULL));
            s = external_global_sym(t, &func_old_type, 0); 
        }
        if ((s->type.t & (VT_STATIC | VT_INLINE | VT_BTYPE)) ==
            (VT_STATIC | VT_INLINE | VT_FUNC)) {
            /* if referencing an inline function, then we generate a
               symbol to it if not already done. It will have the
               effect to generate code for it at the end of the
               compilation unit. Inline function as always
               generated in the text section. */
            if (!s->c)
                put_extern_sym(s, text_section, 0, 0);
            r = VT_SYM | VT_CONST;
        } else {
            r = s->r;
        }
        vset(&s->type, r, s->c);
        /* if forward reference, we must point to s */
        if (vtop->r & VT_SYM) {
            vtop->sym = s;
            vtop->c.ul = 0;
        }
        break;
    }
    
    /* post operations */
    while (1) {
        if (tok == TOK_INC || tok == TOK_DEC) {
            inc(1, tok);
            next();
        } else if (tok == '.' || tok == TOK_ARROW) {
            /* field */ 
            if (tok == TOK_ARROW) 
                indir();
            test_lvalue();
            gaddrof();
            next();
            /* expect pointer on structure */
            if ((vtop->type.t & VT_BTYPE) != VT_STRUCT)
                expect("struct or union");
            s = vtop->type.ref;
            /* find field */
            tok |= SYM_FIELD;
            while ((s = s->next) != NULL) {
                if (s->token == tok)
                    break;
            }
            if (!s)
                error("field not found");
            /* add field offset to pointer */
            vtop->type = char_pointer_type; /* change type to 'char *' */
            vpushi(s->c);
            gen_op('+');
            /* change type to field type, and set to lvalue */
            vtop->type = s->type;
            /* an array is never an lvalue */
            if (!(vtop->type.t & VT_ARRAY)) {
                vtop->r |= lvalue_type(vtop->type.t);
                /* if bound checking, the referenced pointer must be checked */
                if (do_bounds_check) 
                    vtop->r |= VT_MUSTBOUND;
            }
            next();
        } else if (tok == '[') {
            next();
            gexpr();
            gen_op('+');
            indir();
            skip(']');
        } else if (tok == '(') {
            SValue ret;
            Sym *sa;
            int nb_args;

            /* function call  */
            if ((vtop->type.t & VT_BTYPE) != VT_FUNC) {
                /* pointer test (no array accepted) */
                if ((vtop->type.t & (VT_BTYPE | VT_ARRAY)) == VT_PTR) {
                    vtop->type = *pointed_type(&vtop->type);
                    if ((vtop->type.t & VT_BTYPE) != VT_FUNC)
                        goto error_func;
                } else {
                error_func:
                    expect("function pointer");
                }
            } else {
                vtop->r &= ~VT_LVAL; /* no lvalue */
            }
            /* get return type */
            s = vtop->type.ref;
            next();
            sa = s->next; /* first parameter */
            nb_args = 0;
            /* compute first implicit argument if a structure is returned */
            if ((s->type.t & VT_BTYPE) == VT_STRUCT) {
                /* get some space for the returned structure */
                size = type_size(&s->type, &align);
                loc = (loc - size) & -align;
                ret.type = s->type;
                ret.r = VT_LOCAL | VT_LVAL;
                /* pass it as 'int' to avoid structure arg passing
                   problems */
                vseti(VT_LOCAL, loc);
                ret.c = vtop->c;
                nb_args++;
            } else {
                ret.type = s->type; 
                ret.r2 = VT_CONST;
                /* return in register */
                if (is_float(ret.type.t)) {
                    ret.r = REG_FRET; 
                } else {
                    if ((ret.type.t & VT_BTYPE) == VT_LLONG)
                        ret.r2 = REG_LRET;
                    ret.r = REG_IRET;
                }
                ret.c.i = 0;
            }
            if (tok != ')') {
                for(;;) {
                    expr_eq();
                    gfunc_param_typed(s, sa);
                    nb_args++;
                    if (sa)
                        sa = sa->next;
                    if (tok == ')')
                        break;
                    skip(',');
                }
            }
            if (sa)
                error("too few arguments to function");
            skip(')');
            if (cur_text_section) gfunc_call(nb_args);
            else vtop -= (nb_args + 1);
            /* return value */
            vsetc(&ret.type, ret.r, &ret.c);
            vtop->r2 = ret.r2;
        } else {
            break;
        }
    }
}

static void uneq(void)
{
    int t;
    
    unary();
    if (tok == '=' ||
        (tok >= TOK_A_MOD && tok <= TOK_A_DIV) ||
        tok == TOK_A_XOR || tok == TOK_A_OR ||
        tok == TOK_A_SHL || tok == TOK_A_SAR) {
        test_lvalue();
        t = tok;
        next();
        if (t == '=') {
            expr_eq();
        } else {
            vdup();
            expr_eq();
            gen_op(t & 0x7f);
        }
        vstore();
    }
}

static void expr_prod(void)
{
    int t;

    uneq();
    while (tok == '*' || tok == '/' || tok == '%') {
        t = tok;
        next();
        uneq();
        gen_op(t);
    }
}

static void expr_sum(void)
{
    int t;

    expr_prod();
    while (tok == '+' || tok == '-') {
        t = tok;
        next();
        expr_prod();
        gen_op(t);
    }
}

static void expr_shift(void)
{
    int t;

    expr_sum();
    while (tok == TOK_SHL || tok == TOK_SAR) {
        t = tok;
        next();
        expr_sum();
        gen_op(t);
    }
}

static void expr_cmp(void)
{
    int t;

    expr_shift();
    while ((tok >= TOK_ULE && tok <= TOK_GT) ||
           tok == TOK_ULT || tok == TOK_UGE) {
        t = tok;
        next();
        expr_shift();
        gen_op(t);
    }
}

static void expr_cmpeq(void)
{
    int t;

    expr_cmp();
    while (tok == TOK_EQ || tok == TOK_NE) {
        t = tok;
        next();
        expr_cmp();
        gen_op(t);
    }
}

static void expr_and(void)
{
    expr_cmpeq();
    while (tok == '&') {
        next();
        expr_cmpeq();
        gen_op('&');
    }
}

static void expr_xor(void)
{
    expr_and();
    while (tok == '^') {
        next();
        expr_and();
        gen_op('^');
    }
}

static void expr_or(void)
{
    expr_xor();
    while (tok == '|') {
        next();
        expr_xor();
        gen_op('|');
    }
}

/* XXX: fix this mess */
static void expr_land_const(void)
{
    expr_or();
    while (tok == TOK_LAND) {
        next();
        expr_or();
        gen_op(TOK_LAND);
    }
}

/* XXX: fix this mess */
static void expr_lor_const(void)
{
    expr_land_const();
    while (tok == TOK_LOR) {
        next();
        expr_land_const();
        gen_op(TOK_LOR);
    }
}

/* only used if non constant */
static void expr_land(void)
{
    int t;

    expr_or();
    if (tok == TOK_LAND) {
        t = 0;
        save_regs(1);
        for(;;) {
            t = gtst(1, t);
            if (tok != TOK_LAND) {
                vseti(VT_JMPI, t);
                break;
            }
            next();
            expr_or();
        }
    }
}

static void expr_lor(void)
{
    int t;

    expr_land();
    if (tok == TOK_LOR) {
        t = 0;
        save_regs(1);
        for(;;) {
            t = gtst(0, t);
            if (tok != TOK_LOR) {
                vseti(VT_JMP, t);
                break;
            }
            next();
            expr_land();
        }
    }
}

/* XXX: better constant handling */
static void expr_eq(void)
{
    int tt, u, r1, r2, rc, t1, t2, bt1, bt2;
    SValue sv;
    CType type, type1, type2;

    if (const_wanted) {
        int c1, c;
        expr_lor_const();
        if (tok == '?') {
            c = vtop->c.i;
            vpop();
            next();
            if (tok == ':' && gnu_ext) {
                c1 = c;
            } else {
                gexpr();
                c1 = vtop->c.i;
                vpop();
            }
            skip(':');
            expr_eq();
            if (c)
                vtop->c.i = c1;
        }
    } else {
        expr_lor();
        if (tok == '?') {
            next();
            if (vtop != vstack) {
                /* needed to avoid having different registers saved in
                   each branch */
                if (is_float(vtop->type.t))
                    rc = RC_FLOAT;
                else
                    rc = RC_INT;
                    gv(rc);
                    save_regs(1);
            }
            if (tok == ':' && gnu_ext) {
                gv_dup();
                tt = gtst(1, 0);
            } else {
                tt = gtst(1, 0);
                gexpr();
            }
            type1 = vtop->type;
            sv = *vtop; /* save value to handle it later */
            vtop--; /* no vpop so that FP stack is not flushed */
            skip(':');
            u = gjmp(0);
            gen_resolve_sym(tt);
            expr_eq();
            type2 = vtop->type;

            t1 = type1.t;
            bt1 = t1 & VT_BTYPE;
            t2 = type2.t;
            bt2 = t2 & VT_BTYPE;
            /* cast operands to correct type according to ISOC rules */
            if (is_float(bt1) || is_float(bt2)) {
                if (bt1 == VT_LDOUBLE || bt2 == VT_LDOUBLE) {
                    type.t = VT_LDOUBLE;
                } else if (bt1 == VT_DOUBLE || bt2 == VT_DOUBLE) {
                    type.t = VT_DOUBLE;
                } else {
                    type.t = VT_FLOAT;
                }
            } else if (bt1 == VT_LLONG || bt2 == VT_LLONG) {
                /* cast to biggest op */
                type.t = VT_LLONG;
                /* convert to unsigned if it does not fit in a long long */
                if ((t1 & (VT_BTYPE | VT_UNSIGNED)) == (VT_LLONG | VT_UNSIGNED) ||
                    (t2 & (VT_BTYPE | VT_UNSIGNED)) == (VT_LLONG | VT_UNSIGNED))
                    type.t |= VT_UNSIGNED;
            } else if (bt1 == VT_PTR || bt2 == VT_PTR) {
                /* XXX: test pointer compatibility */
                type = type1;
            } else if (bt1 == VT_FUNC || bt2 == VT_FUNC) {
                /* XXX: test function pointer compatibility */
                type = type1;
            } else if (bt1 == VT_STRUCT || bt2 == VT_STRUCT) {
                /* XXX: test structure compatibility */
                type = type1;
            } else if (bt1 == VT_VOID || bt2 == VT_VOID) {
                /* NOTE: as an extension, we accept void on only one side */
                type.t = VT_VOID;
            } else {
                /* integer operations */
                type.t = VT_INT;
                /* convert to unsigned if it does not fit in an integer */
                if ((t1 & (VT_BTYPE | VT_UNSIGNED)) == (VT_INT | VT_UNSIGNED) ||
                    (t2 & (VT_BTYPE | VT_UNSIGNED)) == (VT_INT | VT_UNSIGNED))
                    type.t |= VT_UNSIGNED;
            }
                
            /* now we convert second operand */
            gen_cast(&type);
            if (VT_STRUCT == (vtop->type.t & VT_BTYPE))
                gaddrof();
            rc = RC_INT;
            if (is_float(type.t)) {
                rc = RC_FLOAT;
            } else if ((type.t & VT_BTYPE) == VT_LLONG) {
                /* for long longs, we use fixed registers to avoid having
                   to handle a complicated move */
                rc = RC_IRET; 
            }
            
            r2 = gv(rc);
            /* this is horrible, but we must also convert first
               operand */
            tt = gjmp(0);
            gen_resolve_sym(u);
            /* put again first value and cast it */
            *vtop = sv;
            gen_cast(&type);
            if (VT_STRUCT == (vtop->type.t & VT_BTYPE))
                gaddrof();
            r1 = gv(rc);
            move_reg(r2, r1);
            vtop->r = r2;
            gen_resolve_sym(tt);
        }
    }
}

static void gexpr(void)
{
    while (1) {
        expr_eq();
        if (tok != ',')
            break;
        vpop();
        next();
    }
}

/* parse an expression and return its type without any side effect. */
static void expr_type(CType *type)
{
    Section *saved_text_section = cur_text_section;

    cur_text_section = NULL;
    gexpr();
    *type = vtop->type;
    vpop();
    cur_text_section = saved_text_section;
}

/* parse a unary expression and return its type without any side
   effect. */
static void unary_type(CType *type)
{
    Section *saved_text_section = cur_text_section;

    cur_text_section = NULL;
    unary();
    *type = vtop->type;
    vpop();
    cur_text_section = saved_text_section;
}

/* parse a constant expression and return value in vtop.  */
static void expr_const1(void)
{
    int a;
    a = const_wanted;
    const_wanted = 1;
    expr_eq();
    const_wanted = a;
}

/* parse an integer constant and return its value. */
static int expr_const(void)
{
    int c;
    expr_const1();
    if ((vtop->r & (VT_VALMASK | VT_LVAL | VT_SYM)) != VT_CONST)
        expect("constant expression");
    c = vtop->c.i;
    vpop();
    return c;
}

/* return the label token if current token is a label, otherwise
   return zero */
static int is_label(void)
{
    int last_tok;

    /* fast test first */
    if (tok < TOK_UIDENT)
        return 0;
    /* no need to save tokc because tok is an identifier */
    last_tok = tok;
    next();
    if (tok == ':') {
        next();
        return last_tok;
    } else {
        unget_tok(last_tok);
        return 0;
    }
}

static void block(int *bsym, int *csym, int *case_sym, int *def_sym, 
                  int case_reg, int is_expr)
{
    int a, b, c, d;
    Sym *s;

    /* generate line number info */
    if (do_debug && 
        (last_line_num != file->line_num || last_ind != gen_ind)) {
        put_stabn(N_SLINE, 0, file->line_num, gen_ind - func_ind);
        last_ind = gen_ind;
        last_line_num = file->line_num;
    }

    if (is_expr) {
        /* default return value is (void) */
        vpushi(0);
        vtop->type.t = VT_VOID;
    }

    if (tok == TOK_IF) {
        /* if test */
        next();
        skip('(');
        gexpr();
        skip(')');
        a = gtst(1, 0);
        block(bsym, csym, case_sym, def_sym, case_reg, 0);
        if (tok == TOK_ELSE) {
            next();
            d = gjmp(0);
            gen_resolve_sym(a);
            block(bsym, csym, case_sym, def_sym, case_reg, 0);
            gen_resolve_sym(d); /* patch else jmp */
        } else
            gen_resolve_sym(a);
    } else if (tok == TOK_WHILE) {
        next();
        d = gen_ind;
        skip('(');
        gexpr();
        skip(')');
        a = gtst(1, 0);
        b = 0;
        block(&a, &b, case_sym, def_sym, case_reg, 0);
        gjmp_addr(d);
        gen_resolve_sym(a);
        gen_resolve_sym_addr(b, d);
    } else if (tok == '{') {
        Sym *llabel;
        
        next();
        /* record local declaration stack position */
        s = local_stack;
        llabel = local_label_stack;
        /* handle local labels declarations */
        if (tok == TOK_LABEL) {
            next();
            for(;;) {
                if (tok < TOK_UIDENT)
                    expect("label identifier");
                label_push(&local_label_stack, tok, LABEL_DECLARED);
                next();
                if (tok == ',') {
                    next();
                } else {
                    skip(';');
                    break;
                }
            }
        }
        while (tok != '}') {
            decl(VT_LOCAL);
            if (tok != '}') {
                if (is_expr)
                    vpop();
                block(bsym, csym, case_sym, def_sym, case_reg, is_expr);
            }
        }
        /* pop locally defined labels */
        label_pop(&local_label_stack, llabel);
        /* pop locally defined symbols */
        sym_pop(&local_stack, s);
        next();
    } else if (tok == TOK_RETURN) {
        next();
        if (tok != ';') {
            gexpr();
            gen_assign_cast(&func_vt);
            if ((func_vt.t & VT_BTYPE) == VT_STRUCT) {
                CType type;
                /* if returning structure, must copy it to implicit
                   first pointer arg location */
#ifdef TCC_ARM_EABI
                int align, size;
                size = type_size(&func_vt,&align);
                if(size <= 4) {
                    if((vtop->r != (VT_LOCAL | VT_LVAL) || (vtop->c.i & 3))
                       && (align & 3))
                    {
                        int addr;
                        loc = (loc - size) & -4;
                        addr = loc;
                        type = func_vt;
                        vset(&type, VT_LOCAL | VT_LVAL, addr);
                        vswap();
                        vstore();
                        vset(&int_type, VT_LOCAL | VT_LVAL, addr);
                    }
                    vtop->type = int_type;
                    gv(RC_IRET);
                } else {
#endif
                type = func_vt;
                mk_pointer(&type);
                vset(&type, VT_LOCAL | VT_LVAL, func_vc);
                indir();
                vswap();
                /* copy structure value to pointer */
                vstore();
#ifdef TCC_ARM_EABI
                }
#endif
            } else if (is_float(func_vt.t)) {
                gv(RC_FRET);
            } else {
                gv(RC_IRET);
            }
            vtop--; /* NOT vpop() because on x86 it would flush the fp stack */
        }
        skip(';');
        rsym = gjmp(rsym); /* jmp */
    } else if (tok == TOK_BREAK) {
        /* compute jump */
        if (!bsym)
            error("cannot break");
        *bsym = gjmp(*bsym);
        next();
        skip(';');
    } else if (tok == TOK_CONTINUE) {
        /* compute jump */
        if (!csym)
            error("cannot continue");
        *csym = gjmp(*csym);
        next();
        skip(';');
    } else if (tok == TOK_FOR) {
        int e;
        next();
        skip('(');
        if (tok != ';') {
            gexpr();
            vpop();
        }
        skip(';');
        d = c = gen_ind;
        a = b = 0;
        if (tok != ';') {
            gexpr();
            a = gtst(1, 0);
        }
        skip(';');
        if (tok != ')') {
            e = gjmp(0);
            c = gen_ind;
            gexpr();
            vpop();
            gjmp_addr(d);
            gen_resolve_sym(e);
        }
        skip(')');
        block(&a, &b, case_sym, def_sym, case_reg, 0);
        gjmp_addr(c);
        gen_resolve_sym(a);
        gen_resolve_sym_addr(b, c);
    } else 
    if (tok == TOK_DO) {
        next();
        a = b = 0;
        d = gen_ind;
        block(&a, &b, case_sym, def_sym, case_reg, 0);
        skip(TOK_WHILE);
        skip('(');
        gen_resolve_sym(b);
        gexpr();
        c = gtst(0, 0);
        gen_resolve_sym_addr(c, d);
        skip(')');
        gen_resolve_sym(a);
        skip(';');
    } else
    if (tok == TOK_SWITCH) {
        next();
        skip('(');
        gexpr();
        /* XXX: other types than integer */
        case_reg = gv(RC_INT);
        vpop();
        skip(')');
        a = 0;
        b = gjmp(0); /* jump to first case */
        c = 0;
        block(&a, csym, &b, &c, case_reg, 0);
        /* if no default, jmp after switch */
        if (c == 0)
            c = gen_ind;
        /* default label */
        gen_resolve_sym_addr(b, c);
        /* break label */
        gen_resolve_sym(a);
    } else
    if (tok == TOK_CASE) {
        int v1, v2;
        if (!case_sym)
            expect("switch");
        /* since a case is like a label, we must skip it with a jmp */
        b = gjmp(0);
    next_case:
        next();
        v1 = expr_const();
        v2 = v1;
        if (gnu_ext && tok == TOK_DOTS) {
            next();
            v2 = expr_const();
            if (v2 < v1)
                warning("empty case range");
        }
        gen_resolve_sym(*case_sym);
        vseti(case_reg, 0);
        vpushi(v1);
        if (v1 == v2) {
            gen_op(TOK_EQ);
            *case_sym = 0;
        } else {
            gen_op(TOK_GE);
            *case_sym = gtst(1, 0);
            vseti(case_reg, 0);
            vpushi(v2);
            gen_op(TOK_LE);
        }
        skip(':');
        if (tok == TOK_CASE) {
            b = gtst(0, b);
            goto next_case;
        }
        *case_sym = gtst(1, *case_sym);
        gen_resolve_sym(b);
        is_expr = 0;
        goto block_after_label;
    } else 
    if (tok == TOK_DEFAULT) {
        next();
        skip(':');
        if (!def_sym)
            expect("switch");
        if (*def_sym)
            error("too many 'default'");
        *def_sym = gen_ind;
        is_expr = 0;
        goto block_after_label;
    } else
    if (tok == TOK_GOTO) {
        next();
        if (tok == '*' && gnu_ext) {
            /* computed goto */
            next();
            gexpr();
            if ((vtop->type.t & VT_BTYPE) != VT_PTR)
                expect("pointer");
            gen_goto();
        } else if (tok >= TOK_UIDENT) {
            s = label_find(tok);
            /* put forward definition if needed */
            if (!s) {
                s = label_push(&global_label_stack, tok, LABEL_FORWARD);
            } else {
                if (s->r == LABEL_DECLARED)
                    s->r = LABEL_FORWARD;
            }
            /* label already defined */
            if (s->r & LABEL_FORWARD) 
                s->next = (void *)gjmp((long)s->next);
            else
                gjmp_addr((long)s->next);
            next();
        } else {
            expect("label identifier");
        }
        skip(';');
    } else if (tok == TOK_ASM1 || tok == TOK_ASM2 || tok == TOK_ASM3) {
        asm_instr();
    } else {
        b = is_label();
        if (b) {
            /* label case */
            s = label_find(b);
            if (s) {
                if (s->r == LABEL_DEFINED)
                    error("duplicate label '%s'", get_tok_str(s->token, NULL));
                gen_resolve_sym((long)s->next);
                s->r = LABEL_DEFINED;
            } else {
                s = label_push(&global_label_stack, b, LABEL_DEFINED);
            }
            s->next = (void *)gen_ind;
            /* we accept this, but it is a mistake */
        block_after_label:
            if (tok == '}') {
                warning("deprecated use of label at end of compound statement");
            } else {
                if (is_expr)
                    vpop();
                block(bsym, csym, case_sym, def_sym, case_reg, is_expr);
            }
        } else {
            /* expression case */
            if (tok != ';') {
                if (is_expr) {
                    vpop();
                    gexpr();
                } else {
                    gexpr();
                    vpop();
                }
            }
            skip(';');
        }
    }
}

/* t is the array or struct type. c is the array or struct
   address. cur_index/cur_field is the pointer to the current
   value. 'size_only' is true if only size info is needed (only used
   in arrays) */
static void decl_designator(CType *type, Section *sec, unsigned long c, 
                            int *cur_index, Sym **cur_field, 
                            int size_only)
{
    Sym *s, *f;
    int notfirst, index, index_last, align, l, nb_elems, elem_size;
    CType type1;

    notfirst = 0;
    elem_size = 0;
    nb_elems = 1;
    if (gnu_ext && (l = is_label()) != 0)
        goto struct_field;
    while (tok == '[' || tok == '.') {
        if (tok == '[') {
            if (!(type->t & VT_ARRAY))
                expect("array type");
            s = type->ref;
            next();
            index = expr_const();
            if (index < 0 || (s->c >= 0 && index >= s->c))
                expect("invalid index");
            if (tok == TOK_DOTS && gnu_ext) {
                next();
                index_last = expr_const();
                if (index_last < 0 || 
                    (s->c >= 0 && index_last >= s->c) ||
                    index_last < index)
                    expect("invalid index");
            } else {
                index_last = index;
            }
            skip(']');
            if (!notfirst)
                *cur_index = index_last;
            type = pointed_type(type);
            elem_size = type_size(type, &align);
            c += index * elem_size;
            /* NOTE: we only support ranges for last designator */
            nb_elems = index_last - index + 1;
            if (nb_elems != 1) {
                notfirst = 1;
                break;
            }
        } else {
            next();
            l = tok;
            next();
        struct_field:
            if ((type->t & VT_BTYPE) != VT_STRUCT)
                expect("struct/union type");
            s = type->ref;
            l |= SYM_FIELD;
            f = s->next;
            while (f) {
                if (f->token == l)
                    break;
                f = f->next;
            }
            if (!f)
                expect("field");
            if (!notfirst)
                *cur_field = f;
            /* XXX: fix this mess by using explicit storage field */
            type1 = f->type;
            type1.t |= (type->t & ~VT_TYPE);
            type = &type1;
            c += f->c;
        }
        notfirst = 1;
    }
    if (notfirst) {
        if (tok == '=') {
            next();
        } else {
            if (!gnu_ext)
                expect("=");
        }
    } else {
        if (type->t & VT_ARRAY) {
            index = *cur_index;
            type = pointed_type(type);
            c += index * type_size(type, &align);
        } else {
            f = *cur_field;
            if (!f)
                error("too many field init");
            /* XXX: fix this mess by using explicit storage field */
            type1 = f->type;
            type1.t |= (type->t & ~VT_TYPE);
            type = &type1;
            c += f->c;
        }
    }
    decl_initializer(type, sec, c, 0, size_only);

    /* XXX: make it more general */
    if (!size_only && nb_elems > 1) {
        unsigned long c_end;
        uint8_t *src, *dst;
        int i;

        if (!sec)
            error("range init not supported yet for dynamic storage");
        c_end = c + nb_elems * elem_size;
        if (c_end > sec->data_allocated)
            section_realloc(sec, c_end);
        src = sec->data + c;
        dst = src;
        for(i = 1; i < nb_elems; i++) {
            dst += elem_size;
            memcpy(dst, src, elem_size);
        }
    }
}

#define EXPR_VAL   0
#define EXPR_CONST 1
#define EXPR_ANY   2

/* store a value or an expression directly in global data or in local array */
static void init_putv(CType *type, Section *sec, unsigned long c, 
                      int v, int expr_type)
{
    int saved_global_expr, bt, bit_pos, bit_size;
    void *ptr;
    unsigned long long bit_mask;
    CType dtype;

    switch(expr_type) {
    case EXPR_VAL:
        vpushi(v);
        break;
    case EXPR_CONST:
        /* compound literals must be allocated globally in this case */
        saved_global_expr = global_expr;
        global_expr = 1;
        expr_const1();
        global_expr = saved_global_expr;
        /* NOTE: symbols are accepted */
        if ((vtop->r & (VT_VALMASK | VT_LVAL)) != VT_CONST)
            error("initializer element is not constant");
        break;
    case EXPR_ANY:
        expr_eq();
        break;
    }

    dtype = *type;
    dtype.t &= ~VT_CONSTANT; /* need to do that to avoid false warning */

    if (sec) {
        /* XXX: not portable */
        /* XXX: generate error if incorrect relocation */
        gen_assign_cast(&dtype);
        bt = type->t & VT_BTYPE;
        ptr = sec->data + c;
        /* XXX: make code faster ? */
        if (!(type->t & VT_BITFIELD)) {
            bit_pos = 0;
            bit_size = 32;
            bit_mask = -1LL;
        } else {
            bit_pos = (vtop->type.t >> VT_STRUCT_SHIFT) & 0x3f;
            bit_size = (vtop->type.t >> (VT_STRUCT_SHIFT + 6)) & 0x3f;
            bit_mask = (1LL << bit_size) - 1;
        }
        if ((vtop->r & VT_SYM) &&
            (bt == VT_BYTE ||
             bt == VT_SHORT ||
             bt == VT_DOUBLE ||
             bt == VT_LDOUBLE ||
             bt == VT_LLONG ||
             (bt == VT_INT && bit_size != 32)))
            error("initializer element is not computable at load time");
        switch(bt) {
        case VT_BYTE:
            *(char *)ptr |= (vtop->c.i & bit_mask) << bit_pos;
            break;
        case VT_SHORT:
            *(short *)ptr |= (vtop->c.i & bit_mask) << bit_pos;
            break;
        case VT_DOUBLE:
            *(double *)ptr = vtop->c.d;
            break;
        case VT_LDOUBLE:
            *(long double *)ptr = vtop->c.ld;
            break;
        case VT_LLONG:
            *(long long *)ptr |= (vtop->c.ll & bit_mask) << bit_pos;
            break;
        default:
            if (vtop->r & VT_SYM) {
                greloc(sec, vtop->sym, c, R_DATA_32);
            }
            *(int *)ptr |= (vtop->c.i & bit_mask) << bit_pos;
            break;
        }
        vtop--;
    } else {
        vset(&dtype, VT_LOCAL|VT_LVAL, c);
        vswap();
        vstore();
        vpop();
    }
}

/* put zeros for variable based init */
static void init_putz(CType *t, Section *sec, unsigned long c, int size)
{
    if (sec) {
        /* nothing to do because globals are already set to zero */
    } else {
        vpush_global_sym(&func_old_type, TOK_memset);
        vseti(VT_LOCAL, c);
        vpushi(0);
        vpushi(size);
        gfunc_call(3);
    }
}

/* 't' contains the type and storage info. 'c' is the offset of the
   object in section 'sec'. If 'sec' is NULL, it means stack based
   allocation. 'first' is true if array '{' must be read (multi
   dimension implicit array init handling). 'size_only' is true if
   size only evaluation is wanted (only for arrays). */
static void decl_initializer(CType *type, Section *sec, unsigned long c, 
                             int first, int size_only)
{
    int index, array_length, n, no_oblock, nb, parlevel, i;
    int size1, align1, expr_type;
    Sym *s, *f;
    CType *t1;

    if (type->t & VT_ARRAY) {
        s = type->ref;
        n = s->c;
        array_length = 0;
        t1 = pointed_type(type);
        size1 = type_size(t1, &align1);

        no_oblock = 1;
        if ((first && tok != TOK_LSTR && tok != TOK_STR) || 
            tok == '{') {
            skip('{');
            no_oblock = 0;
        }

        /* only parse strings here if correct type (otherwise: handle
           them as ((w)char *) expressions */
        if ((tok == TOK_LSTR && 
//#ifdef TINYCC_TARGET_PE						4 lines added by patch 384 Navara  OYOY
//             (t1->t & VT_BTYPE) == VT_SHORT && (t1->t & VT_UNSIGNED)) ||
//#else
             (t1->t & VT_BTYPE) == VT_INT) ||
//#endif
            (tok == TOK_STR &&
             (t1->t & VT_BTYPE) == VT_BYTE)) {
            while (tok == TOK_STR || tok == TOK_LSTR) {
                int cstr_len, ch;
                CString *cstr;

                cstr = tokc.cstr;
                /* compute maximum number of chars wanted */
                if (tok == TOK_STR)
                    cstr_len = cstr->size;
                else
                    cstr_len = cstr->size / sizeof(int);			// original code
//                    cstr_len = cstr->size / sizeof(nwchar_t);		modded by patch 384 Navara  OYOY
                cstr_len--;
                nb = cstr_len;
                if (n >= 0 && nb > (n - array_length))
                    nb = n - array_length;
                if (!size_only) {
                    if (cstr_len > nb)
                        warning("initializer-string for array is too long");
                    /* in order to go faster for common case (char
                       string in global variable, we handle it
                       specifically */
                    if (sec && tok == TOK_STR && size1 == 1) {
                        memcpy(sec->data + c + array_length, cstr->data, nb);
                    } else {
                        for(i=0;i<nb;i++) {
                            if (tok == TOK_STR)
                                ch = ((unsigned char *)cstr->data)[i];
                            else
                                ch = ((int *)cstr->data)[i];				// original code
//                                ch = ((nwchar_t *)cstr->data)[i];			modded by patch 384 Navara  OYOY
                            init_putv(t1, sec, c + (array_length + i) * size1,
                                      ch, EXPR_VAL);
                        }
                    }
                }
                array_length += nb;
                next();
            }
            /* only add trailing zero if enough storage (no
               warning in this case since it is standard) */
            if (n < 0 || array_length < n) {
                if (!size_only) {
                    init_putv(t1, sec, c + (array_length * size1), 0, EXPR_VAL);
                }
                array_length++;
            }
        } else {
            index = 0;
            while (tok != '}') {
                decl_designator(type, sec, c, &index, NULL, size_only);
                if (n >= 0 && index >= n)
                    error("index too large");
                /* must put zero in holes (note that doing it that way
                   ensures that it even works with designators) */
                if (!size_only && array_length < index) {
                    init_putz(t1, sec, c + array_length * size1, 
                              (index - array_length) * size1);
                }
                index++;
                if (index > array_length)
                    array_length = index;
                /* special test for multi dimensional arrays (may not
                   be strictly correct if designators are used at the
                   same time) */
                if (index >= n && no_oblock)
                    break;
                if (tok == '}')
                    break;
                skip(',');
            }
        }
        if (!no_oblock)
            skip('}');
        /* put zeros at the end */
        if (!size_only && n >= 0 && array_length < n) {
            init_putz(t1, sec, c + array_length * size1, 
                      (n - array_length) * size1);
        }
        /* patch type size if needed */
        if (n < 0)
            s->c = array_length;
    } else if ((type->t & VT_BTYPE) == VT_STRUCT &&
               (sec || !first || tok == '{')) {
        int par_count;

        /* NOTE: the previous test is a specific case for automatic
           struct/union init */
        /* XXX: union needs only one init */

        /* XXX: this test is incorrect for local initializers
           beginning with ( without {. It would be much more difficult
           to do it correctly (ideally, the expression parser should
           be used in all cases) */
        par_count = 0;
        if (tok == '(') {
            AttributeDef ad1;
            CType type1;
            next();
            while (tok == '(') {
                par_count++;
                next();
            }
            if (!parse_btype(&type1, &ad1))
                expect("cast");
            type_decl(&type1, &ad1, &n, TYPE_ABSTRACT);
#if 0
            if (!is_assignable_types(type, &type1))
                error("invalid type for cast");
#endif
            skip(')');
        }
        no_oblock = 1;
        if (first || tok == '{') {
            skip('{');
            no_oblock = 0;
        }
        s = type->ref;
        f = s->next;
        array_length = 0;
        index = 0;
        n = s->c;
        while (tok != '}') {
            decl_designator(type, sec, c, NULL, &f, size_only);
            index = f->c;
            if (!size_only && array_length < index) {
                init_putz(type, sec, c + array_length, 
                          index - array_length);
            }
            index = index + type_size(&f->type, &align1);
            if (index > array_length)
                array_length = index;
            f = f->next;
            if (no_oblock && f == NULL)
                break;
            if (tok == '}')
                break;
            skip(',');
        }
        /* put zeros at the end */
        if (!size_only && array_length < n) {
            init_putz(type, sec, c + array_length, 
                      n - array_length);
        }
        if (!no_oblock)
            skip('}');
        while (par_count) {
            skip(')');
            par_count--;
        }
    } else if (tok == '{') {
        next();
        decl_initializer(type, sec, c, first, size_only);
        skip('}');
    } else if (size_only) {
        /* just skip expression */
        parlevel = 0;
        while ((parlevel > 0 || (tok != '}' && tok != ',')) && 
               tok != -1) {
            if (tok == '(')
                parlevel++;
            else if (tok == ')')
                parlevel--;
            next();
        }
    } else {
        /* currently, we always use constant expression for globals
           (may change for scripting case) */
        expr_type = EXPR_CONST;
        if (!sec)
            expr_type = EXPR_ANY;
        init_putv(type, sec, c, 0, expr_type);
    }
}

/* parse an initializer for type 't' if 'has_init' is non zero, and
   allocate space in local or global data space ('r' is either
   VT_LOCAL or VT_CONST). If 'v' is non zero, then an associated
   variable 'v' of scope 'scope' is declared before initializers are
   parsed. If 'v' is zero, then a reference to the new object is put
   in the value stack. If 'has_init' is 2, a special parsing is done
   to handle string constants. */
static void decl_initializer_alloc(CType *type, AttributeDef *ad, int r, 
                                   int has_init, int v, int scope)
{
    int size, align, addr, data_offset;
    int level;
    ParseState saved_parse_state;
    TokenString init_str;
    Section *sec;

    size = type_size(type, &align);
    /* If unknown size, we must evaluate it before
       evaluating initializers because
       initializers can generate global data too
       (e.g. string pointers or ISOC99 compound
       literals). It also simplifies local
       initializers handling */
    tok_str_new(&init_str);
    if (size < 0) {
        if (!has_init) 
            error("unknown type size");
        /* get all init string */
        if (has_init == 2) {
            /* only get strings */
            while (tok == TOK_STR || tok == TOK_LSTR) {
                tok_str_add_tok(&init_str);
                next();
            }
        } else {
            level = 0;
            while (level > 0 || (tok != ',' && tok != ';')) {
                if (tok < 0)
                    error("unexpected end of file in initializer");
                tok_str_add_tok(&init_str);
                if (tok == '{')
                    level++;
                else if (tok == '}') {
                    if (level == 0)
                        break;
                    level--;
                }
                next();
            }
        }
        tok_str_add(&init_str, -1, 0);
        tok_str_add(&init_str, 0, 0);
        
        /* compute size */
        save_parse_state(&saved_parse_state);

        macro_ptr = init_str.str;
        next();
        decl_initializer(type, NULL, 0, 1, 1);
        /* prepare second initializer parsing */
        macro_ptr = init_str.str;
        next();
        
        /* if still unknown size, error */
        size = type_size(type, &align);
        if (size < 0) 
            error("unknown type size");
    }
    /* take into account specified alignment if bigger */
    if (ad->aligned) {
        if (ad->aligned > align)
            align = ad->aligned;
    } else if (ad->packed) {
        align = 1;
    }
    if ((r & VT_VALMASK) == VT_LOCAL) {
        sec = NULL;
        if (do_bounds_check && (type->t & VT_ARRAY)) 
            loc--;
        loc = (loc - size) & -align;
        addr = loc;
        /* handles bounds */
        /* XXX: currently, since we do only one pass, we cannot track
           '&' operators, so we add only arrays */
        if (do_bounds_check && (type->t & VT_ARRAY)) {
            unsigned long *bounds_ptr;
            /* add padding between regions */
            loc--;
            /* then add local bound info */
            bounds_ptr = section_ptr_add(lbounds_section, 2 * sizeof(unsigned long));
            bounds_ptr[0] = addr;
            bounds_ptr[1] = size;
        }
        if (v) {
            /* local variable */
            sym_push(v, type, r, addr);
        } else {
            /* push local reference */
            vset(type, r, addr);
        }
    } else {
        Sym *sym;

        sym = NULL;
        if (v && scope == VT_CONST) {
            /* see if the symbol was already defined */
            sym = sym_find(v);
            if (sym) {
                if (!is_compatible_types(&sym->type, type))
                    error("incompatible types for redefinition of '%s'", 
                          get_tok_str(v, NULL));
                if (sym->type.t & VT_EXTERN) {
                    /* if the variable is extern, it was not allocated */
                    sym->type.t &= ~VT_EXTERN;
                    /* set array size if it was ommited in extern
                       declaration */
                    if ((sym->type.t & VT_ARRAY) && 
                        sym->type.ref->c < 0 &&
                        type->ref->c >= 0)
                        sym->type.ref->c = type->ref->c;
                } else {
                    /* we accept several definitions of the same
                       global variable. this is tricky, because we
                       must play with the SHN_COMMON type of the symbol */
                    /* XXX: should check if the variable was already
                       initialized. It is incorrect to initialized it
                       twice */
                    /* no init data, we won't add more to the symbol */
                    if (!has_init)
                        goto no_alloc;
                }
            }
        }

        /* allocate symbol in corresponding section */
        sec = ad->section;
        if (!sec) {
            if (has_init)
                sec = data_section;
            else if (tccg_nocommon)
                sec = bss_section;
        }
        if (sec) {
            data_offset = sec->data_offset;
            data_offset = (data_offset + align - 1) & -align;
            addr = data_offset;
            /* very important to increment global pointer at this time
               because initializers themselves can create new initializers */
            data_offset += size;
            /* add padding if bound check */
            if (do_bounds_check)
                data_offset++;
            sec->data_offset = data_offset;
            /* allocate section space to put the data */
            if (sec->sh_type != SHT_NOBITS && 
                data_offset > sec->data_allocated)
                section_realloc(sec, data_offset);
            /* align section if needed */
            if (align > sec->sh_addralign)
                sec->sh_addralign = align;
        } else {
            addr = 0; /* avoid warning */
        }

        if (v) {
            if (scope != VT_CONST || !sym) {
                sym = sym_push(v, type, r | VT_SYM, 0);
            }
            /* update symbol definition */
            if (sec) {
                put_extern_sym(sym, sec, addr, size);
            } else {
                Elf32_Sym *esym;
                /* put a common area */
                put_extern_sym(sym, NULL, align, size);
                /* XXX: find a nicer way */
                esym = &((Elf32_Sym *)symtab_section->data)[sym->c];
                esym->st_shndx = SHN_COMMON;
            }
        } else {
            CValue cval;

            /* push global reference */
            sym = get_sym_ref(type, sec, addr, size);
            cval.ul = 0;
            vsetc(type, VT_CONST | VT_SYM, &cval);
            vtop->sym = sym;
        }

        /* handles bounds now because the symbol must be defined
           before for the relocation */
        if (do_bounds_check) {
            unsigned long *bounds_ptr;

            greloc(bounds_section, sym, bounds_section->data_offset, R_DATA_32);
            /* then add global bound info */
            bounds_ptr = section_ptr_add(bounds_section, 2 * sizeof(long));
            bounds_ptr[0] = 0; /* relocated */
            bounds_ptr[1] = size;
        }
    }
    if (has_init) {
        decl_initializer(type, sec, addr, 1, 0);
        /* restore parse state if needed */
        if (init_str.str) {
            tok_str_free(init_str.str);
            restore_parse_state(&saved_parse_state);
        }
    }
 no_alloc: ;
}

void put_func_debug(Sym *sym)
{
    char buf[512];

    /* stabs info */
    /* XXX: we put here a dummy type */
    snprintf(buf, sizeof(buf), "%s:%c1", 
             funcname, sym->type.t & VT_STATIC ? 'f' : 'F');
    put_stabs_r(buf, N_FUN, 0, file->line_num, 0,
                cur_text_section, sym->c);
    last_ind = 0;
    last_line_num = 0;
}

/* parse an old style function declaration list */
/* XXX: check multiple parameter */
static void func_decl_list(Sym *func_sym)
{
    AttributeDef ad;
    int token;
    Sym *s;
    CType btype, type;

    /* parse each declaration */
    while (tok != '{' && tok != ';' && tok != ',' && tok != TOK_EOF) {
        if (!parse_btype(&btype, &ad)) 
            expect("declaration list");
        if (((btype.t & VT_BTYPE) == VT_ENUM ||
             (btype.t & VT_BTYPE) == VT_STRUCT) && 
            tok == ';') {
            /* we accept no variable after */
        } else {
            for(;;) {
                type = btype;
                type_decl(&type, &ad, &token, TYPE_DIRECT);
                /* find parameter in function parameter list */
                s = func_sym->next;
                while (s != NULL) {
                    if ((s->token & ~SYM_FIELD) == token)
                        goto found;
                    s = s->next;
                }
                error("declaration for parameter '%s' but no such parameter",
                      get_tok_str(token, NULL));
            found:
                /* check that no storage specifier except 'register' was given */
                if (type.t & VT_STORAGE)
                    error("storage class specified for '%s'", get_tok_str(token, NULL)); 
                convert_parameter_type(&type);
                /* we can add the type (NOTE: it could be local to the function) */
                s->type = type;
                /* accept other parameters */
                if (tok == ',')
                    next();
                else
                    break;
            }
        }
        skip(';');
    }
}

/* parse a function defined by symbol 'sym' and generate its code in
   'cur_text_section' */
static void gen_function(Sym *sym)
{
    gen_ind = cur_text_section->data_offset;
    /* NOTE: we patch the symbol size later */
    put_extern_sym(sym, cur_text_section, gen_ind, 0);
    funcname = get_tok_str(sym->token, NULL);
    func_ind = gen_ind;
    /* put debug symbol */
    if (do_debug)
        put_func_debug(sym);
    /* push a dummy symbol to enable local sym storage */
    sym_push2(&local_stack, SYM_FIELD, 0, 0);
    gfunc_prolog(&sym->type);
//	loc = 0;						this line removed by patch 305 TK  OYOY
    rsym = 0;
    block(NULL, NULL, NULL, NULL, 0, 0);
    gen_resolve_sym(rsym);
    gfunc_epilog();
    cur_text_section->data_offset = gen_ind;
    label_pop(&global_label_stack, NULL);
    sym_pop(&local_stack, NULL); /* reset local stack */
    /* end of function */
    /* patch symbol size */
    ((Elf32_Sym *)symtab_section->data)[sym->c].st_size = 
        gen_ind - func_ind;
    if (do_debug) {
        put_stabn(N_FUN, 0, 0, gen_ind - func_ind);
    }
    funcname = ""; /* for safety */
    func_vt.t = VT_VOID; /* for safety */
    gen_ind = 0; /* for safety */
}

static void gen_inline_functions(void)
{
    Sym *sym;
    CType *type;
    int *str, inline_generated;

    /* iterate while inline function are referenced */
    for(;;) {
        inline_generated = 0;
        for(sym = global_stack; sym != NULL; sym = sym->prev) {
            type = &sym->type;
            if (((type->t & VT_BTYPE) == VT_FUNC) &&
                (type->t & (VT_STATIC | VT_INLINE)) == 
                (VT_STATIC | VT_INLINE) &&
                sym->c != 0) {
                /* the function was used: generate its code and
                   convert it to a normal function */
                str = (int *)sym->r;
                sym->r = VT_SYM | VT_CONST;
                type->t &= ~VT_INLINE;

                macro_ptr = str;
                next();
                cur_text_section = text_section;
                gen_function(sym);
                macro_ptr = NULL; /* fail safe */

                tok_str_free(str);
                inline_generated = 1;
            }
        }
        if (!inline_generated)
            break;
    }

    /* free all remaining inline function tokens */
    for(sym = global_stack; sym != NULL; sym = sym->prev) {
        type = &sym->type;
        if (((type->t & VT_BTYPE) == VT_FUNC) &&
            (type->t & (VT_STATIC | VT_INLINE)) == 
            (VT_STATIC | VT_INLINE)) {
            str = (int *)sym->r;
            tok_str_free(str);
            sym->r = 0; /* fail safe */
        }
    }
}

/* 'l' is VT_LOCAL or VT_CONST to define default storage type */
static void decl(int l)
{
    int v, has_init, r;
    CType type, btype;
    Sym *sym;
    AttributeDef ad;
    
    while (1) {
        if (!parse_btype(&btype, &ad)) {
            /* skip redundant ';' */
            /* XXX: find more elegant solution */
            if (tok == ';') {
                next();
                continue;
            }
            if (l == VT_CONST &&
                (tok == TOK_ASM1 || tok == TOK_ASM2 || tok == TOK_ASM3)) {
                /* global asm block */
                asm_global_instr();
                continue;
            }
            /* special test for old K&R protos without explicit int
               type. Only accepted when defining global data */
            if (l == VT_LOCAL || tok < TOK_DEFINE)
                break;
            btype.t = VT_INT;
        }
        if (((btype.t & VT_BTYPE) == VT_ENUM ||
             (btype.t & VT_BTYPE) == VT_STRUCT) && 
            tok == ';') {
            /* we accept no variable after */
            next();
            continue;
        }
        while (1) { /* iterate thru each declaration */
            type = btype;
            type_decl(&type, &ad, &v, TYPE_DIRECT);
#if 0
            {
                char buf[500];
                type_to_str(buf, sizeof(buf), &type, get_tok_str(v, NULL));
                printf("type = '%s'\n", buf);
            }
#endif
            if ((type.t & VT_BTYPE) == VT_FUNC) {
                /* if old style function prototype, we accept a
                   declaration list */
                sym = type.ref;
                if (sym->c == FUNC_OLD)
                    func_decl_list(sym);
            }

            /* Open curly bracket here can only be a function body, because
               struct and union contents are handled by parse_btype() above. */
            if (tok == '{') {
                if (l == VT_LOCAL)
                    error("cannot use local functions");
                if ((type.t & VT_BTYPE) != VT_FUNC)
                    expect("function definition");

/*  OYOY -- The next 5 lines were added in patch 281 -- mauro persano
                // reject abstract declarators in function definition
                sym = type.ref;
                while ((sym = sym->next) != NULL)
                    if (!(sym->token & ~SYM_FIELD))		// this line was slightly modded again in a later rev
                       expect("identifier");
*/
                /* XXX: cannot do better now: convert extern inline to static inline */
                if ((type.t & (VT_EXTERN | VT_INLINE)) == (VT_EXTERN | VT_INLINE))
                    type.t = (type.t & ~VT_EXTERN) | VT_STATIC;
                
                sym = sym_find(v);
                if (sym) {
                    if ((sym->type.t & VT_BTYPE) != VT_FUNC)
                        goto func_error1;
                    /* specific case: if not func_call defined, we put
                       the one of the prototype */
                    /* XXX: should have default value */
                    if (sym->type.ref->r != FUNC_CDECL &&
                        type.ref->r == FUNC_CDECL)
                        type.ref->r = sym->type.ref->r;
                    if (!is_compatible_types(&sym->type, &type)) {
                    func_error1:
                        error("incompatible types for redefinition of '%s'", 
                              get_tok_str(v, NULL));
                    }
                    /* if symbol is already defined, then put complete type */
                    sym->type = type;
                } else {
                    /* put function symbol */
                    sym = global_identifier_push(v, type.t, 0);
                    sym->type.ref = type.ref;
                }

                /* static inline functions are just recorded as a kind
                   of macro. Their code will be emitted at the end of
                   the compilation unit only if they are used */
                if ((type.t & (VT_INLINE | VT_STATIC)) == 
                    (VT_INLINE | VT_STATIC)) {
                    TokenString func_str;
                    int block_level;
                           
                    tok_str_new(&func_str);
                    
                    block_level = 0;
                    for(;;) {
                        int t;
                        if (tok == TOK_EOF)
                            error("unexpected end of file");
                        tok_str_add_tok(&func_str);
                        t = tok;
                        next();
                        if (t == '{') {
                            block_level++;
                        } else if (t == '}') {
                            block_level--;
                            if (block_level == 0)
                                break;
                        }
                    }
                    tok_str_add(&func_str, -1, 0);
                    tok_str_add(&func_str, 0, 0);
                    sym->r = (long)func_str.str;
                } else {
                    /* compute text section */
                    cur_text_section = ad.section;
                    if (!cur_text_section)
                        cur_text_section = text_section;
                    sym->r = VT_SYM | VT_CONST;
                    gen_function(sym);
//#ifdef TINYCC_TARGET_PE							// 5 lines added by patch 354 Grischka  OYOY
//                    if (ad.dllexport) {
//                        ((Elf32_Sym *)symtab_section->data)[sym->c].st_other |= 1;
//                    }
//#endif
                }
                break;

            /* If there was no function body, it must be a varaible. */
            } else {
                if (btype.t & VT_TYPEDEF) {
                    /* save typedefed type  */
                    /* XXX: test storage specifiers ? */
                    sym = sym_push(v, &type, 0, 0);
                    sym->type.t |= VT_TYPEDEF;
                } else if ((type.t & VT_BTYPE) == VT_FUNC) {
                    /* external function definition */
                    /* specific case for func_call attribute */
                    if (ad.func_call)
                        type.ref->r = ad.func_call;
                    external_sym(v, &type, 0);
                } else {
                    /* not lvalue if array */
                    r = 0;
                    if (!(type.t & VT_ARRAY))
                        r |= lvalue_type(type.t);
                    has_init = (tok == '=');
                    if ((btype.t & VT_EXTERN) || 
                        ((type.t & VT_ARRAY) && (type.t & VT_STATIC) &&
                         !has_init && l == VT_CONST && type.ref->c < 0)) {
                        /* external variable */
                        /* NOTE: as GCC, uninitialized global static
                           arrays of null size are considered as
                           extern */
                        external_sym(v, &type, r);
                    } else {
                        type.t |= (btype.t & VT_STATIC); // Retain "static".
                        if (type.t & VT_STATIC)
                            r |= VT_CONST;
                        else
                            r |= l;
                        if (has_init)
                            next();
                        decl_initializer_alloc(&type, &ad, r, 
                                               has_init, v, l);
                    }
                }
                if (tok != ',') {
                    skip(';');
                    break;
                }
                next();
            }
        }
    }
}

/* better than nothing, but needs extension to handle '-E' option
   correctly too */
static void preprocess_init(TCCState *s1)
{
    s1->include_stack_ptr = s1->include_stack;
    /* XXX: move that before to avoid having to initialize
       file->ifdef_stack_ptr ? */
    s1->ifdef_stack_ptr = s1->ifdef_stack;
    file->ifdef_stack_ptr = s1->ifdef_stack_ptr;

    vtop = vstack;
//    s1->pack_stack[0] = 0;							2 lines added by patch 354 Grischka  OYOY
//    s1->pack_stack_ptr = s1->pack_stack;
}

/* compile the C file opened in 'file'. Return non zero if errors. */
static int tcc_compile(TCCState *s1)
{
    Sym *define_start;
    char buf[512];
    volatile int section_sym;

#ifdef INC_DEBUG
    printf("%s: **** new file\n", file->filename);
#endif
    preprocess_init(s1);

    funcname = "";
    anon_sym = SYM_FIRST_ANOM; 

    /* file info: full path + filename */
    section_sym = 0; /* avoid warning */
    if (do_debug) {
        section_sym = put_elf_sym(symtab_section, 0, 0, 
                                  ELF32_ST_INFO(STB_LOCAL, STT_SECTION), 0, 
                                  text_section->sh_num, NULL);
        getcwd(buf, sizeof(buf));
        pstrcat(buf, sizeof(buf), "/");
        put_stabs_r(buf, N_SO, 0, 0, 
                    text_section->data_offset, text_section, section_sym);
        put_stabs_r(file->filename, N_SO, 0, 0, 
                    text_section->data_offset, text_section, section_sym);
    }
    /* an elf symbol of type STT_FILE must be put so that STB_LOCAL
       symbols can be safely used */
    put_elf_sym(symtab_section, 0, 0, 
                ELF32_ST_INFO(STB_LOCAL, STT_FILE), 0, 
                SHN_ABS, file->filename);

    /* define some often used types */
    int_type.t = VT_INT;

    char_pointer_type.t = VT_BYTE;
    mk_pointer(&char_pointer_type);

    func_old_type.t = VT_FUNC;
    func_old_type.ref = sym_push(SYM_FIELD, &int_type, FUNC_CDECL, FUNC_OLD);

#if defined(TCC_ARM_EABI) && defined(TCC_ARM_VFP)
    float_type.t = VT_FLOAT;
    double_type.t = VT_DOUBLE;

    func_float_type.t = VT_FUNC;
    func_float_type.ref = sym_push(SYM_FIELD, &float_type, FUNC_CDECL, FUNC_OLD);
    func_double_type.t = VT_FUNC;
    func_double_type.ref = sym_push(SYM_FIELD, &double_type, FUNC_CDECL, FUNC_OLD);
#endif

#if 0
    /* define 'void *alloca(unsigned int)' builtin function */
    {
        Sym *s1;

        p = anon_sym++;
        sym = sym_push(p, mk_pointer(VT_VOID), FUNC_CDECL, FUNC_NEW);
        s1 = sym_push(SYM_FIELD, VT_UNSIGNED | VT_INT, 0, 0);
        s1->next = NULL;
        sym->next = s1;
        sym_push(TOK_alloca, VT_FUNC | (p << VT_STRUCT_SHIFT), VT_CONST, 0);
    }
#endif

    define_start = define_stack;

    if (setjmp(s1->error_jmp_buf) == 0) {
        s1->nb_errors = 0;
        s1->error_set_jmp_enabled = 1;

        fch = file->buf_ptr[0];
        next_tok_flags = TOK_FLAG_BOW | TOK_FLAG_BOL | TOK_FLAG_BOF;
        parse_flags = PARSE_FLAG_PREPROCESS | PARSE_FLAG_TOK_NUM;
        next();
        decl(VT_CONST);
        if (tok != TOK_EOF) expect("declaration");

        /* end of translation unit info */
        if (do_debug) {
            put_stabs_r(NULL, N_SO, 0, 0, text_section->data_offset,
                        text_section, section_sym);
        }
    }
    s1->error_set_jmp_enabled = 0;

    /* reset define stack, but leave -Dsymbols (may be incorrect if
       they are undefined) */
    free_defines(define_start); 

    gen_inline_functions();

    sym_pop(&global_stack, NULL);

    return s1->nb_errors != 0 ? -1 : 0;
}

/* Preprocess the current file */
/* XXX: add line and file infos, add options to preserve spaces */
static int tcc_preprocess(TCCState *s1)
{
    Sym *define_start;
    int last_is_space;

    preprocess_init(s1);

    define_start = define_stack;

    fch = file->buf_ptr[0];
    tok_flags = TOK_FLAG_BOL | TOK_FLAG_BOF;
    parse_flags = PARSE_FLAG_ASM_COMMENTS | PARSE_FLAG_PREPROCESS |
        PARSE_FLAG_LINEFEED;
    last_is_space = 1;
    next();
    for(;;) {
        if (tok == TOK_EOF)
            break;
        if (!last_is_space) {
            fputc(' ', tccg_outfile);
        }
        fputs(get_tok_str(tok, &tokc), tccg_outfile);
        if (tok == TOK_LINEFEED) {
            last_is_space = 1;
            /* XXX: suppress that hack */
            parse_flags &= ~PARSE_FLAG_LINEFEED;
            next();
            parse_flags |= PARSE_FLAG_LINEFEED;
        } else {
            last_is_space = 0;
            next();
        }
    }

    free_defines(define_start);

    return 0;
}

#ifdef LIBTCC
int tcc_compile_string(TCCState *s, char *str)
{
    BufferedFile bf1, *bf = &bf1;
    int ret, len;
    char *buf;

    /* init file structure */
    bf->fd = -1;
    /* XXX: avoid copying */
    len = strlen(str);
    buf = xmalloc(len + 1);
    memcpy(buf, str, len);
    buf[len] = CH_EOB;
    bf->buf_ptr = buf;
    bf->buf_end = buf + len;
    pstrcpy(bf->filename, sizeof(bf->filename), "<string>");
    bf->line_num = 1;
    file = bf;
    
    ret = tcc_compile(s);
    
    free(buf);

    /* currently, no need to close */
    return ret;
}
#endif

/* define a preprocessor symbol. A value can also be provided with the '=' operator */
void tcc_define_symbol(TCCState *s1, char *sym, char *value)
{
    BufferedFile bf1, *bf = &bf1;

    pstrcpy(bf->buffer, IO_BUF_SIZE, sym);
    pstrcat(bf->buffer, IO_BUF_SIZE, " ");
    /* default value */
    if (!value) 
        value = "1";
    pstrcat(bf->buffer, IO_BUF_SIZE, value);
    
    /* init file structure */
    bf->fd = -1;
    bf->buf_ptr = bf->buffer;
    bf->buf_end = bf->buffer + strlen(bf->buffer);
    *bf->buf_end = CH_EOB;
    bf->filename[0] = '\0';
    bf->line_num = 1;
    file = bf;
    
    s1->include_stack_ptr = s1->include_stack;

    /* parse with define parser */
    fch = file->buf_ptr[0];
    next_nomacro();
    parse_define();
    file = NULL;
}

/* undefine a preprocessor symbol */
void tcc_undefine_symbol(TCCState *s1, char *sym)
{
    TokenSym *ts;
    Sym *s;
    ts = tok_alloc(sym, strlen(sym));
    s = define_find(ts->tok);
    /* undefine symbol by putting an invalid name */
    if (s)
        define_undef(s);
}

#ifdef CONFIG_TCC_ASM

//#ifdef TINYCC_TARGET_I386				added by patch 295 Glockner  OYOY
#include "i386/asm.c"
//#endif				added by patch 295 Glockner  OYOY
#include "tccasm.c"

#else
static void asm_instr(void)
{
    error("inline asm() not supported");
}
static void asm_global_instr(void)
{
    error("inline asm() not supported");
}
#endif

#include "tccelf.c"

//#ifdef TINYCC_TARGET_COFF			3 lines added in patch 309 TK  OYOY
//#include "tcccoff.c"
//#endif

#ifdef TINYCC_TARGET_PE
#include "win32/tccpe.c"
#endif

/* do all relocations (needed before using tcc_get_symbol()) */
int tcc_relocate(TCCState *s1)
{
    Section *s;
    int i;

    s1->nb_errors = 0;
    
#ifdef TINYCC_TARGET_PE
    pe_add_runtime(s1);
#else
    tcc_add_runtime(s1);
#endif

    relocate_common_syms();

    tcc_add_linker_symbols(s1);

    build_got_entries(s1);
    
    /* compute relocation address : section are relocated in place. We
       also alloc the bss space */
    for(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
        if (s->sh_flags & SHF_ALLOC) {
            if (s->sh_type == SHT_NOBITS)
                s->data = xzmalloc(s->data_offset);
            s->sh_addr = (unsigned long)s->data;
        }
    }

    relocate_syms(s1, 1);

    if (s1->nb_errors != 0)
        return -1;

    /* relocate each section */
    for(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
        if (s->reloc)
            relocate_section(s1, s);
    }

    /* mark executable sections as executable in memory */
    for(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
        if ((s->sh_flags & (SHF_ALLOC | SHF_EXECINSTR)) == 
            (SHF_ALLOC | SHF_EXECINSTR)) {
#ifdef WIN32
            {
                int old_protect;
                VirtualProtect(s->data, s->data_offset,
                               PAGE_EXECUTE_READWRITE, &old_protect);
            }
#else
            {
                unsigned long start, end;
                start = (unsigned long)(s->data) & ~(PAGESIZE - 1);
                end = (unsigned long)(s->data + s->data_offset);
                end = (end + PAGESIZE - 1) & ~(PAGESIZE - 1);
                mprotect((void *)start, end - start, 
                         PROT_READ | PROT_WRITE | PROT_EXEC);
            }
#endif            
        }
    }
    return 0;
}

/* launch the compiled program with the given arguments */
int tcc_run(TCCState *s1, int argc, char **argv)
{
    int (*prog_main)(int, char **);

    if (tcc_relocate(s1) < 0)
        return -1;

    prog_main = tcc_get_symbol_err(s1, "main");
    
#ifdef CONFIG_TCC_BCHECK
    if (do_bounds_check) {
        void (*bound_init)(void);

        /* XXX: use .init section so that it also work in binary ? */
        bound_init = (void *)tcc_get_symbol_err(s1, "__bound_init");
        bound_init();
    }
#endif
    return (*prog_main)(argc, argv);
}

void add_dynarray_path(TCCState *s, char *pathname, struct dynarray *dd)
{
    char *c = pathname;

    for (;;) {
        if (!*c || *c==LIB_PATH_SEPCHAR) {
            int len = c-pathname;
            char *c2 = xmalloc(len+1);

            strncpy(c2, pathname, len);
            c2[len] = 0;
            pathname += len+1;
            dynarray_add((void ***)&dd->data, &dd->len, c2);
        }
        if (!*c++) break;
    }
}

// Initialize tcc state

TCCState *tcc_new(void)
{
    char *p, *r;
    TCCState *s;
    TokenSym *ts;
    int i, c;

    s = tcc_state = xzmalloc(sizeof(TCCState));
    tccg_output_type = TCC_OUTPUT_MEMORY;

    /* init isidnum table */
    for(i=0;i<256;i++)
        isidnum_table[i] = (i >= 'a' && i <= 'z') || (i >= 'A' && i <= 'Z')
            || i == '_' || isnum(i);

    /* add all tokens */
    table_ident = NULL;
    memset(hash_ident, 0, TOK_HASH_SIZE * sizeof(TokenSym *));
    
    tok_ident = TOK_IDENT;
    p = tcc_keywords;
    while (*p) {
        r = p;
        for(;;) {
            c = *r++;
            if (c == '\0')
                break;
        }
        ts = tok_alloc(p, r - p - 1);
        p = r;
    }

    /* we add dummy defines for some special macros to speed up tests
       and to have working defined() */
    define_push(TOK___LINE__, MACRO_OBJ, NULL, NULL);
    define_push(TOK___FILE__, MACRO_OBJ, NULL, NULL);
    define_push(TOK___DATE__, MACRO_OBJ, NULL, NULL);
    define_push(TOK___TIME__, MACRO_OBJ, NULL, NULL);

    /* standard defines */
    tcc_define_symbol(s, "__STDC__", NULL);
    tcc_define_symbol(s, "__STDC_VERSION__", "199901L");
#if defined(TINYCC_TARGET_I386)
    tcc_define_symbol(s, "__i386__", NULL);
#endif
//#if defined(TINYCC_TARGET_ARM)			10 lines added by patch 295 Glockner  OYOY
//    tcc_define_symbol(s, "__ARM_ARCH_4__", NULL);
//    tcc_define_symbol(s, "__arm_elf__", NULL);
//    tcc_define_symbol(s, "__arm_elf", NULL);
    //tcc_define_symbol(s, "arm_elf", NULL);
//    tcc_define_symbol(s, "__arm__", NULL);
//    tcc_define_symbol(s, "__arm", NULL);
    //tcc_define_symbol(s, "arm", NULL);
//    tcc_define_symbol(s, "__APCS_32__", NULL);
//#endif
#if defined(__linux__)
    tcc_define_symbol(s, "__linux__", NULL);
    tcc_define_symbol(s, "__linux", NULL);
    //tcc_define_symbol(s, "linux", NULL);
#endif
    /* tiny C specific defines */
    tcc_define_symbol(s, "__TINYC__", NULL);

    /* tiny C & gcc defines */
    tcc_define_symbol(s, "__SIZE_TYPE__", "unsigned int");
    tcc_define_symbol(s, "__PTRDIFF_TYPE__", "int");

    /* wchar type and default library paths */
//#ifdef TINYCC_TARGET_PE							4 lines added by patch 384 Navara  OYOY
//    tcc_define_symbol(s, "__WCHAR_TYPE__", "unsigned short");
//#else
    tcc_define_symbol(s, "__WCHAR_TYPE__", "int");
//#endif
    
    /* no section zero */
    dynarray_add((void ***)&s->sections, &s->nb_sections, NULL);

    /* create standard sections */
    text_section = new_section(s, ".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR);
    data_section = new_section(s, ".data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE);
    bss_section = new_section(s, ".bss", SHT_NOBITS, SHF_ALLOC | SHF_WRITE);

    /* symbols are always generated for linking stage */
    symtab_section = new_symtab(s, ".symtab", SHT_SYMTAB, 0,
                                ".strtab",
                                ".hashtab", SHF_PRIVATE); 
    strtab_section = symtab_section->link;
    
    /* private symbol table for dynamic symbols */
    s->dynsymtab_section = new_symtab(s, ".dynsymtab", SHT_SYMTAB, SHF_PRIVATE,
                                      ".dynstrtab", 
                                      ".dynhashtab", SHF_PRIVATE);
    s->alacarte_link = 1;

#ifdef CHAR_IS_UNSIGNED
    tccg_char_is_unsigned = 1;
#endif
#if defined(TINYCC_TARGET_PE)
//#if defined(TINYCC_TARGET_PE) && 0		above line modded by patch 377 Grischka  OYOY
    /* XXX: currently the PE linker is not ready to support that */
    tccg_leading_underscore = 1;
#endif
    return s;
}

void tcc_delete(TCCState *s1)
{
    int i, n;

    /* free -D defines */
    free_defines(NULL);

    /* free tokens */
    n = tok_ident - TOK_IDENT;
    for(i = 0; i < n; i++)
        free(table_ident[i]);
    free(table_ident);

    /* free all sections */

    free_section(symtab_section->hash);

    free_section(s1->dynsymtab_section->hash);
    free_section(s1->dynsymtab_section->link);
    free_section(s1->dynsymtab_section);

    for(i = 1; i < s1->nb_sections; i++)
        free_section(s1->sections[i]);
    free(s1->sections);
    
    /* free loaded dlls array */
    for(i = 0; i < s1->nb_loaded_dlls; i++)
        free(s1->loaded_dlls[i]);
    free(s1->loaded_dlls);

    /* library paths */
    for(i = 0; i < tccg_library_paths.len; i++)
        free(tccg_library_paths.data[i]);
    free(tccg_library_paths.data);

    /* cached includes */
    for(i = 0; i < s1->nb_cached_includes; i++)
        free(s1->cached_includes[i]);
    free(s1->cached_includes);

    for(i = 0; i < tccg_include_paths.len; i++)
        free(tccg_include_paths.data[i]);
    free(tccg_include_paths.data);

    for(i = 0; i < s1->sysinclude_paths.len; i++)
        free(s1->sysinclude_paths.data[i]);
    free(s1->sysinclude_paths.data);

    free(s1);
}

int tcc_add_file_internal(TCCState *s1, char *filename, int flags)
{
    char *ext = 0;
    int fd, ret;
    BufferedFile *saved_file;

    saved_file = file;

    // Treat filename "-" as /dev/stdin
    if (*filename=='-' && !filename[1]) file = tcc_open(s1, 0);
    else {
        char *filename1;

        /* find source file type with extension */
        filename1 = strrchr(filename, '/');
        if (filename1) filename1++;
        else filename1 = filename;
        ext = strrchr(filename1, '.');
        if (ext) ext++;

        file = tcc_open(s1, filename);
    }

    if (tccg_verbose > !file)
        printf("%s file '%s'\n", file ? "Read" : "Tried", filename);
    if (!file) {
        if (flags & AFF_PRINT_ERROR) {
            error_noabort("file '%s' not found", filename);
        }
        ret = -1;
        goto fail1;
    }

    if (flags & AFF_PREPROCESS) ret = tcc_preprocess(s1);
    else if (!ext || !strcmp(ext, "c")) {
        /* C file assumed */
        ret = tcc_compile(s1);
    } else 
#ifdef CONFIG_TCC_ASM
    if (!strcmp(ext, "S")) {
        /* preprocessed assembler */
        ret = tcc_assemble(s1, 1);
    } else if (!strcmp(ext, "s")) {
        /* non preprocessed assembler */
        ret = tcc_assemble(s1, 0);
    } else 
#endif
#ifdef TINYCC_TARGET_PE
    if (!strcmp(ext, "def")) {
        ret = pe_load_def_file(s1, fdopen(file->fd, "rb"));
    } else
#endif
    {
        Elf32_Ehdr ehdr;

        fd = file->fd;
        /* assume executable format: auto guess file type */
		if (read(fd, &ehdr, sizeof(ehdr)) != sizeof(ehdr)) {		// original code
//        ret = read(fd, &ehdr, sizeof(ehdr));						prev line replaced with 3 lines in patch 295 Glockner  OYOY
//        lseek(fd, 0, SEEK_SET);
//        if (ret <= 0) {
            error_noabort("could not read header");
            goto fail;
		}							// original code
        lseek(fd, 0, SEEK_SET);		// original code
//        } else if (ret != sizeof(ehdr)) {						prev 2 lines replaced with 3 lines in patch 295 Glockner  OYOY
//            goto try_load_script;
//        }

        if (ehdr.e_ident[0] == ELFMAG0 &&
            ehdr.e_ident[1] == ELFMAG1 &&
            ehdr.e_ident[2] == ELFMAG2 &&
            ehdr.e_ident[3] == ELFMAG3) {
            file->line_num = 0; /* do not display line number if error */
            if (ehdr.e_type == ET_REL) {
                ret = tcc_load_object_file(s1, fd, 0);
            } else if (ehdr.e_type == ET_DYN) {
                if (tccg_output_type == TCC_OUTPUT_MEMORY) {
#ifdef TINYCC_TARGET_PE
                    ret = -1;
#else
                    void *h;
                    h = dlopen(filename, RTLD_GLOBAL | RTLD_LAZY);
                    if (h)
                        ret = 0;
                    else
                        ret = -1;
#endif
                } else {
                    ret = tcc_load_dll(s1, fd, filename, 
                                       (flags & AFF_REFERENCED_DLL) != 0);
                }
            } else {
                error_noabort("unrecognized ELF file");
                goto fail;
            }
        } else if (memcmp((char *)&ehdr, ARMAG, 8) == 0) {
            file->line_num = 0; /* do not display line number if error */
            ret = tcc_load_archive(s1, fd);
        } else 
//#ifdef TINYCC_TARGET_COFF					5 lines added in patch 309 TK  OYOY
//        if (*(uint16_t *)(&ehdr) == COFF_C67_MAGIC) {
//            ret = tcc_load_coff(s1, fd);
//        } else
//#endif
        {
            /* as GNU ld, consider it is an ld script if not recognized */
//        try_load_script:							 added by patch 295 Glockner  OYOY
            ret = tcc_load_ldscript(s1);
            if (ret < 0) {
                error_noabort("unrecognized file type");
                goto fail;
            }
        }
    }
 the_end:
    tcc_close(file);
 fail1:
    file = saved_file;
    return ret;
 fail:
    ret = -1;
    goto the_end;
}

int tcc_add_file(TCCState *s, char *filename)
{
    return tcc_add_file_internal(s, filename, AFF_PRINT_ERROR);
}

/* find and load a dll. Return non zero if not found */
/* XXX: add '-rpath' option support ? */
static int tcc_add_dll(TCCState *s, char *filename, int flags)
{
    char buf[1024];
    int i = 0;

    for(;;) {
        int test;

        snprintf(buf, sizeof(buf), "%s/%s", 
                 tccg_library_paths.data[i], filename);
        test = tccg_library_paths.len == ++i;
        if (!tcc_add_file_internal(s, buf, test ? flags : (flags & ~AFF_PRINT_ERROR)))
            return 0;
        if (test) break;
    }
    return -1;
}

/* the library name is the same as the argument of the '-l' option */
int tcc_add_library(TCCState *s, char *libraryname)
{
    char buf[1024];
    
    /* first we look for the dynamic library if not static linking */
    if (!tccg_static_link) {
#ifdef TINYCC_TARGET_PE
        snprintf(buf, sizeof(buf), "%s.def", libraryname);
#else
        snprintf(buf, sizeof(buf), "lib%s.so", libraryname);
#endif
        if (tcc_add_dll(s, buf, 0) == 0)
            return 0;
    }

    snprintf(buf, sizeof(buf), "lib%s.a", libraryname);
    return tcc_add_dll(s, buf, AFF_PRINT_ERROR);
}

int tcc_add_symbol(TCCState *s, char *name, unsigned long val)
{
    add_elf_sym(symtab_section, val, 0, 
                ELF32_ST_INFO(STB_GLOBAL, STT_NOTYPE), 0,				// the "other" arg (symbol visibility = 0) was added by patch 354 Grischka  OYOY
                SHN_ABS, name);
    return 0;
}

int init_output_type(TCCState *s)
{
    if (!tccg_nostdinc) {
        char buf[1024];

        /* default include paths */
        /* XXX: reverse order needed if -isystem support */
        add_dynarray_path(s, CC_HEADERPATH, &(s->sysinclude_paths));
        snprintf(buf, sizeof(buf), "%s/include", tinycc_path);
        add_dynarray_path(s, buf, &(s->sysinclude_paths));
    }

    if (!tccg_nostdlib) {
        char buf[1024];
        snprintf(buf, sizeof(buf), "%s/lib", tinycc_path);
        add_dynarray_path(s, buf, &(tccg_library_paths));
        add_dynarray_path(s, CC_LIBPATH, &(tccg_library_paths));
    }

    /* if bound checking, then add corresponding sections */
#ifdef CONFIG_TCC_BCHECK
    if (do_bounds_check) {
        /* define symbol */
        tcc_define_symbol(s, "__BOUNDS_CHECKING_ON", NULL);
        /* create bounds sections */
        bounds_section = new_section(s, ".bounds", 
                                     SHT_PROGBITS, SHF_ALLOC);
        lbounds_section = new_section(s, ".lbounds", 
                                      SHT_PROGBITS, SHF_ALLOC);
    }
#endif

    if (tccg_char_is_unsigned) {
//        tcc_define_symbol(s, "__CHAR_UNSIGNED__", NULL);			added by patch 295 Glockner  OYOY
    }

    /* add debug sections */
    if (do_debug) {
        /* stab symbols */
        stab_section = new_section(s, ".stab", SHT_PROGBITS, 0);
        stab_section->sh_entsize = sizeof(Stab_Sym);
        stabstr_section = new_section(s, ".stabstr", SHT_STRTAB, 0);
        put_elf_str(stabstr_section, "");
        stab_section->link = stabstr_section;
        /* put first entry */
        put_stabs("", 0, 0, 0, 0);
    }

    /* add libc crt1/crti objects */
#ifndef TINYCC_TARGET_PE
    if ((tccg_output_type == TCC_OUTPUT_EXE || tccg_output_type == TCC_OUTPUT_DLL)
        && !tccg_nostdlib)
    {
        if (tccg_output_type != TCC_OUTPUT_DLL)
            tcc_add_dll(s, "crt1.o", AFF_PRINT_ERROR);
        tcc_add_dll(s, "crti.o", AFF_PRINT_ERROR);
    }
#endif
    return 0;
}
