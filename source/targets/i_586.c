//

#define TARGET_PTR_SIZE		4

// prototypes

void qcc_predefine_macro(char *mname, char *val);


void emit_to_target()
{
}

void target_specific_defines()
{
	qcc_predefine_macro("__i386__", NULL);
}
