/*
 *  main.c - Option parsing logic for qcc
 * 
 *  Copyright (c) 2001-2004 Fabrice Bellard
 *  Copyright (c) 2006-2007 Rob Landley
 *  Copyright (c) 2015 Bruce Ewing
 *
 *  Licensed under 2-clause BSD, see file LICENSE in this tarball
 */




static int set_flag(FlagDef *flags, int nb_flags, char *flg_name, int value)
{
 int i;
 FlagDef *p;

 if (flg_name[0] == 'n' && flg_name[1] == 'o' && flg_name[2] == '-') {
  flg_name += 3;
  value = !value;
 }
 for(i = 0, p = flags; i < nb_flags; i++, p++) {
  if (!strcmp(flg_name, p->name))
   goto found;
 }
 return -1;
 found:
 if (p->flags & FD_INVERT)
  value = !value;
 *p->var = value;
 return 0;
}

/* set/reset a warning */
int tcc_set_warning(char *warning_name, int value)
{
 int i;
 FlagDef *p;

 if (!strcmp(warning_name, "all")) {
  for(i = 0, p = warning_defs; i < countof(warning_defs); i++, p++) {
   if (p->flags & WD_ALL)
    *p->var = 1;
  }
  return 0;
 } else {
  return set_flag(warning_defs, countof(warning_defs), warning_name, value);
 }
}


// how_far indicates the amount of data to show from *r
// 0 = show one byte, 1 = show one "word", -1 = show two "words"
void show_error(int level, char *str1, char *r, int how_far)			// HIHI!!!! modify this to accept a line number
{
	char *p;
	int len;

	if (level >= 0)			// show filename & line number
	{
		if (cur_fname == NULL) write (2, "command line: ", 14);
		else
		{
			write (2, (char *) cur_fname, strlen((char *) cur_fname));
			write (2, ": (", 3);
// HIHI!! use ntc (and some scratch buffer???) to dump the line number
			write (2, ") ", 2);
		}
	}
	if (level == 0)
	{
		write (2, "Error: ", 7);
		++total_errs;
	}
	else if (level > 0)
	{
		write (2, "Warning: ", 9);
		if (cur_fname != NULL) ++total_warns;		// HIHI this test is wrong now
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


/* set/reset a flag */
int tcc_set_flag(char *flag_name, int value)
{
 return set_flag(flag_defs, countof(flag_defs), flag_name, value);
}

/* extract the basename of a file */
static char *tcc_basename(char *name)
{
 char *p;
 p = strrchr(name, '/');
#ifdef WIN32
 if (!p)
  p = strrchr(name, '\\');
#endif 
 if (!p)
  p = name;
 else 
  p++;
 return p;
}


// set "i" to 1 to include the trailing NUL char -- 0 otherwise
void dynarray_add(int type, uint8_t *r, int32_t i)
{
	uint8_t *p;
	// note: there is so much room to store data that it is not necessary to check for overflows
	p = da_buffers[type] + da_tot_entrylen[type];
	while (*r != 0) *(p++) = *(r++), ++i;
	*p = 0;
	++da_entry_count[type];
	da_tot_entrylen[type] += i;
}


// permanently pack the contents of the scattered da_buffers at the very top of wrksp
// -- adjust wrksp_top and wrk_rem
void pack_da_bufs()
{
	int i, j, k;
	uint8_t *p, *c, *s;
	i = 7;
	k = 0;
	s = wrksp + wrk_size;

	while (--i >= 0)
	{
		p = da_buffers[i];
		j = da_tot_entrylen[i];
		k += j;
		s -= j;
		c = s;
		while (--j >= 0) *(c++) = *(p++);			// copyup the data
		da_buffers[i] = s;							// and point at the new buffer location
	}
	wrksp_top = s;
	wrk_rem = wrk_size - k;
}


// build a set of #define statements as text, to prepend to all source files
void qcc_predefine_macro(uint8_t *mname, uint8_t *val)
{
	uint8_t tmp[128], *p;
	alt_strcpy (tmp, (uint8_t *) "#define ");
	p = tmp + 8;
	while (*mname != 0) *(p++) = *(mname++);
	*(p++) = ' ';
	*(p++) = '-';				// assume the default value = -1
	if (val == NULL) *(p++) = '1';
	else if (*val == 0) *(p++) = '1';
	else
	{
		--p;		// erase the '-'
		while (*val != 0) *(p++) = *(val++);
	}
	*(p++) = 0x1d;			// this value acts like a newline in the preprocessor
	*p = 0;
	dynarray_add(PREDEFINES, tmp, 0);
}

void qcc_preundef_symbol(uint8_t *symname)
{
	dynarray_add(PRE_UNDEFS, symname, 1);
}

void do_global_defines()
{
    /* standard defines */
	qcc_predefine_macro((uint8_t *) "__STDC__", NULL);
	qcc_predefine_macro((uint8_t *) "__STDC_VERSION__", (uint8_t *) "199901L");

	target_specific_defines();

#if defined(__linux__)
	qcc_predefine_macro("__linux__", NULL);
	qcc_predefine_macro("__linux", NULL);
    //tcc_define_symbol(s, "linux", NULL);
#endif
    /* qcc-specific defines */
	qcc_predefine_macro((uint8_t *) "__qcc__", NULL);
	qcc_predefine_macro((uint8_t *) "_inline", (uint8_t *) "inline");
	// XXX: there is at least _inline_ also -- but the ORDER of defining them is important!
	qcc_predefine_macro((uint8_t *) "__builtin_va_list", (uint8_t *) "void *");

    /* qcc & gcc defines */
	qcc_predefine_macro((uint8_t *) "__SIZE_TYPE__", (uint8_t *) "unsigned int");
	qcc_predefine_macro((uint8_t *) "__PTRDIFF_TYPE__", (uint8_t *) "int");

    /* wchar type and default library paths */
// XXX: obviously this is wrong on Windoze (unsigned shorts)
	qcc_predefine_macro((uint8_t *) "__WCHAR_TYPE__", (uint8_t *) "int");
	// HIHI!!! is THIS the spot where you do the UNDEFINES??
}


void show_version(void)
{
 printf("qcc version -1\n");
// printf("tinycc version " TINYCC_VERSION "\n");
}


void help()
{
 show_version();
 printf("QCC C Compiler - Copyright (C) 2001-2006 Fabrice Bellard, 2007 Rob Landley, 2015 Bruce Ewing\n"
     "usage: tcc [-v] [-c] [-o outfile] [-Bdir] [-bench] [-Idir] [-Dsym[=val]] [-Usym]\n"
     "     [-Wwarn] [-g] [-b] [-bt N] [-Ldir] [-llib] [-shared] [-static]\n"
     "     [infile1 infile2...] [-run infile args...]\n"
     "\n"
     "General options:\n"
     "  -v    Verbose compile, repeat for more verbosity\n"
     "  -c    compile only - generate an object file\n"
     "  -o outfile  set output filename\n"
     "  -Bdir    set tcc internal library path\n"
     "  -bench   output compilation statistics\n"
     "  -run  run compiled source\n"
     "  -fflag   set or reset (with 'no-' prefix) 'flag' (see man page)\n"
     "  -Wwarning   set or reset (with 'no-' prefix) 'warning' (see man page)\n"
     "  -w    disable all warnings\n"
     "Preprocessor options:\n"
     "  -Idir    add include path 'dir'\n"
     "  -Dsym[=val] define 'sym' with value 'val'\n"
     "  -Usym    undefine 'sym'\n"
     "  -E    preprocess only\n"
     "Linker options:\n"
     "  -Ldir    add library path 'dir'\n"
     "  -llib    link with dynamic or static library 'lib'\n"
     "  -shared  generate a shared library\n"
     "  -static  static linking\n"
     "  -rdynamic   export all global symbols to dynamic linker\n"
     "  -r    output relocatable .o file\n"
     "Debugger options:\n"
     "  -g    generate runtime debug info\n"
#ifdef CONFIG_TCC_BCHECK
     "  -b    compile with built-in memory and bounds checker (implies -g)\n"
#endif
     );
}

// if it's legal to have a space between a -Option and its argument, then don't specify TCC_OPTION_NOSEP
// XXX: -- and for now, let's assume it's ALWAYS legal
static TCCOption tcc_options[] = {
 { "h\n", TCC_OPTION_HELP, 0 },
 { "?\n", TCC_OPTION_HELP, 0 },
 { "I\n", TCC_OPTION_I, TCC_OPTION_HAS_ARG },
 { "D\n", TCC_OPTION_D, TCC_OPTION_HAS_ARG },
 { "E\n", TCC_OPTION_E, 0 },
 { "U\n", TCC_OPTION_U, TCC_OPTION_HAS_ARG },
 { "L\n", TCC_OPTION_L, TCC_OPTION_HAS_ARG },
 { "B\n", TCC_OPTION_B, TCC_OPTION_HAS_ARG },
 { "l\n", TCC_OPTION_l, TCC_OPTION_HAS_ARG },
 { "bench\n", TCC_OPTION_bench, 0 },
#ifdef CONFIG_TCC_BCHECK
 { "b\n", TCC_OPTION_b, 0 },
#endif
 { "g\n", TCC_OPTION_g, TCC_OPTION_HAS_ARG },
 { "c\n", TCC_OPTION_c, 0 },
 { "static\n", TCC_OPTION_static, 0 },
 { "shared\n", TCC_OPTION_shared, 0 },
 { "o\n", TCC_OPTION_o, TCC_OPTION_HAS_ARG },
 { "run\n", TCC_OPTION_run, 0 },
 { "rdynamic\n", TCC_OPTION_rdynamic, 0 },
 { "r\n", TCC_OPTION_r, 0 },
 { "Wl\n,", TCC_OPTION_Wl, TCC_OPTION_HAS_ARG },
 { "W\n", TCC_OPTION_W, TCC_OPTION_HAS_ARG },
 { "O\n", TCC_OPTION_O, TCC_OPTION_HAS_ARG },
 { "m\n", TCC_OPTION_m, TCC_OPTION_HAS_ARG },
 { "f\n", TCC_OPTION_f, TCC_OPTION_HAS_ARG },
 { "nostdinc\n", TCC_OPTION_nostdinc, 0 },
 { "nostdlib\n", TCC_OPTION_nostdlib, 0 },
 { "print-search-dirs\n", TCC_OPTION_print_search_dirs, 0 }, 
 { "v\n", TCC_OPTION_v, 0 },
 { "w\n", TCC_OPTION_w, 0 },
 { "pipe\n", TCC_OPTION_pipe, 0},
 { NULL },
};

int parse_args(int argc, char **argv)
{
	TCCOption *popt;
	uint8_t *p1, *r1, *r;
	int optind;

	optind = 0;
	while (optind < argc)
	{
		r = (uint8_t *) argv[optind++];
		if (r[0] != '-' || !r[1]) {
			/* add a new file */
			dynarray_add(SOURCE_FNAMES, r, 1);
			if (!multiple_files) {
				optind--;
				/* argv[0] will be this file */
				return optind;
			}
		}
		else {
			// find option in table (match only the first chars and then parse the option string further)
			popt = tcc_options - 1;		// this value gets preincremented
			do {
				++popt;
				p1 = popt->name;
				if (p1 == NULL)
				{
					show_error(0, "Invalid option -- ", r, 1);
					exit (1);
				}
				r1 = r + 1;						// r points to the '-' just before the option name
				while (*r1 == *p1) ++r1, ++p1;
			} while (*p1 != '\n');				// if p1 gets all the way to the \n, then it's a match

			if (popt->flags & TCC_OPTION_HAS_ARG) {
 				if (*r1 == '\0')
				{
					if (optind >= argc || (popt->flags & TCC_OPTION_NOSEP) != 0)
					{
						show_error(0, "Missing argument to ", r, 1);
						exit (1);
					}
					// if the arg does not immediately follow the option, then it must always be the next argv
					r1 = argv[optind++];
				}
			} else {
				if (*r1 != '\0') {
					help();
					exit(1);
				}
				r1 = NULL;			// HIHI can I just leave it pointing at the 0?
			}
    
	switch(popt->index) {
	case TCC_OPTION_HELP:
		help();
		exit(0);
	case TCC_OPTION_I:
		dynarray_add(INCLUDE_PATHS, r1, 1);
		//    add_dynarray_path(s, optarg, &tccg_include_paths);
		break;
	case TCC_OPTION_D:
		{
			// r1 is pointing at the symname
			p1 = strchr(r1, '=');		// HIHI!!! hmmmm -- what happens if you pass a NULL into strchr? The same as if r1 points to a 0?
			// turn an '=' into a 0, point to whatever comes after
			if (p1 != NULL) *(p1++) = 0;
			qcc_predefine_macro(r1, p1);		// if p1 is NULL, or *p1 is 0, the symbol is defaulted to "1"
		}
		break;
   case TCC_OPTION_E:
//    tccg_output_type = TCC_OUTPUT_PREPROCESS;
    break;
   case TCC_OPTION_U:
    qcc_preundef_symbol(r1);
    break;
   case TCC_OPTION_L:
	dynarray_add(LIB_PATHS, r1, 1);
    break;
   case TCC_OPTION_B:
    /* set tcc utilities path (mainly for tcc development) */
//    tinycc_path = r1;
    break;
   case TCC_OPTION_l:
	dynarray_add(LIB_FNAMES, r1, 1);
    break;
   case TCC_OPTION_bench:
    do_bench = 1;
    break;
#ifdef CONFIG_TCC_BCHECK
   case TCC_OPTION_b:
    do_bounds_check = 1;
    do_debug = 1;
    break;
#endif
   case TCC_OPTION_g:
    do_debug = 1;
    break;
   case TCC_OPTION_c:
    multiple_files = 1;
//    tccg_output_type = TCC_OUTPUT_OBJ;
    break;
   case TCC_OPTION_static:
//    tccg_static_link = 1;
    break;
   case TCC_OPTION_shared:
//    tccg_output_type = TCC_OUTPUT_DLL;
    break;
   case TCC_OPTION_o:
    multiple_files = 1;
    outfile = (char *) r1;			// this pointer is good forever
    break;
   case TCC_OPTION_r:
    /* generate a .o merging several output files */
    reloc_output = 1;
//    tccg_output_type = TCC_OUTPUT_OBJ;
    break;
   case TCC_OPTION_nostdinc:
//    tccg_nostdinc = 1;
    break;
   case TCC_OPTION_nostdlib:
//    tccg_nostdlib = 1;
    break;
   case TCC_OPTION_print_search_dirs:
    print_search_dirs = 1;
    break;
   case TCC_OPTION_run:
    multiple_files = 0;
//    tccg_output_type = TCC_OUTPUT_MEMORY;
    break;
   case TCC_OPTION_v:
    if (!tccg_verbose++) show_version();
    break;
   case TCC_OPTION_f:
    if (tcc_set_flag(r1, 1) < 0 && tccg_warn_unsupported)
     goto unsupported_option;
    break;
   case TCC_OPTION_W:
    if (tcc_set_warning(r1, 1) < 0 && tccg_warn_unsupported)
     goto unsupported_option;
    break;
   case TCC_OPTION_w:
//    tccg_warn_none = 1;				this line was added in patch 285 -- romain francoise  OYOY
    break;
   case TCC_OPTION_rdynamic:
//    tccg_rdynamic = 1;
    break;
   case TCC_OPTION_Wl:
/*    {
// HIHI!! I also want a -Tdata !!
     // r1 points to the option string
     if (strstart(r1, "-Ttext,", &p1)) {
      tccg_text_addr = strtoul(p1, NULL, 16);		HIHI can't use an unsigned long on a 64b target!
      tccg_has_text_addr = 1;
     } else {
      error("unsupported linker option '%s'", r1);
     }
    }		*/
    break;
   default:
    if (tccg_warn_unsupported) {
unsupported_option:
     show_error (1, "unsupported option '%s'", r, 1);
    }
   }		// end of switch on options
		}
	}
	if (da_entry_count[SOURCE_FNAMES] == 0 && !print_search_dirs) {
		if (!tccg_verbose) help();
		exit(1);
	}
	return optind;
}

/*
void bench(int64_t t)
{
	uint32_t mil, sec;
	t - microseconds() - t;
	mil = (t + 1000) / 1000;
	sec = mil / 1000;

	printf("%d idents, %d lines, %d bytes, %d.%03d s, %d lines/s, %0.1f MB/s\n", 
		tok_ident, total_lines, total_bytes,
		sec, mil, (total_lines * 1000 / mil), 
		((float) total_bytes / mil) / 1000); 
} */


int main(int argc, char *argv[])
{
	char *p;
	int r;
	int64_t t;
	t = microseconds();

	// parse argv[0] to determine how qcc was called -- ie. to decide what the user wants it to do
	// HIHI -- should use something like tcc_basename(argv[0]), of course
	p = argv[0];
	while (*p != 0) ++p;		// scan to the end of the string
// look at the preceding chars (discard extensions, find the executable name?)
	p -= 7;			// HIHI! just fakery for now

	onetime_init();

	parse_args(argc - 1, argv + 1);

	do_global_defines();

	dynarray_add(INCLUDE_PATHS, (uint8_t *) "", 1);		// need one extra NUL char on the end of the include path strings
	pack_da_bufs();

	r = 0;
	// XXX: the various possibilities for calling are: strip, as, ld, cc, qcc, ??
	// XXX: should compilation be the default, if the program name has been modified and does not give enough of a clue?
	// was it called as 'qcc' -- to do compilations?
//	if (*p == 'q' && p[1] == 'c' && p[2] == 'c')
	if (*p == 'e' && p[1] == 'w' && p[2] == 'c')
		r = compile ();

//	if (do_bench != 0) bench(t);
	return r;
}

