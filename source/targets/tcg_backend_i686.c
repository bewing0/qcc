// There are a few things that the backend must define for the compiler,
// since they are hardware dependent -- and the backend is the only piece
// of code that knows what the actual hardware *is*.

#ifdef TCG_TARGET_I686

#define TARGET_PTR_SIZE		4

// prototypes

void qcc_predefine_macro(char *mname, char *val);


void target_specific_defines()
{
	qcc_predefine_macro("__i386__", NULL);
}



// ALL THE REST OF THE TCG i686 BACKEND CODE GOES HERE

#endif
