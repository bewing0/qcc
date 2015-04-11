

void tspec_pass(struct pass_info *inf)
{
	int32_t i;
	i = 0;

}


// process typedefs, structs, unions, and enums
void typespecs()
{
	uint32_t j, wrk_avail;
	struct pass_info inf;
	// the input data is either in a file, or stored between wrksp_top and wrk_rem
	inf.infd = -1;
	if (outf_exists != 0)		// read in the tokenized data, if it was dumped into a file
	{
		// open the file and read 30K in binary mode
		wrksp_top = wrksp + wrk_rem - 30 * 1024;
		inf.infd = qcc_open_r("hi1", 1);
//		inf_infd = qcc_open_r(inout_fnames[??], 1);
		j = qcc_read(inf.infd, (char *) wrksp_top, 30 * 1024);
		wrksp_top[j] = TOK_ENDOFBUF;
		// since there was enough data to overflow into a file, leave half the workspace open for it
		wrk_avail = (wrk_rem - 30 * 1024) / 2;
		outf_exists = 0;							// reset the flag for this next pass
		outfd = -1;
	}
	else wrk_avail = wrksp_top - wrksp;				// OK to overwrite the tokenizer output
	emit_ptr = wrksp + wrk_avail;
	wrk_used_base = wrk_avail;

	inf.line_num = 1;
	// nxt_pass_info contains overestimated total length of all s/u/e/typedef definitions?
	// allocate a buffer for that at the bottom -- create the idxtype buffer now, size = idxidx from tokenizer?
	// then the emit pointer above that
	// make a new inf
	tspec_pass(&inf);
}


// final scan of tokenized code for syntax
void syntax_check()
{
	// apply all the grammar rules with a state machine
	// finalize all ambiguous operators (eg: * for mult or deref)
	// simplify expressions as much as possible
}


void declarations()
{
}



