// The compiler calls this the TCG "target", but TCG itself calls this the "frontend",
// and it calls something else the "backend target".
// So, to eliminate confusion this is officially the "TCG intermediate" layer.

// Basically, the TCG frontend is being treated like a compiler intermediate language.

// This whole TCG output mechanism is organized the same way as the compiler
// -- all possible backends are included, but only one selects itself with an internal ifdef
// -- and it's the backend that knows things like the size of a pointer

#ifdef QCC_TARGET_TCG


#include "targets/tcg_backend_i686.c"
#include "targets/tcg_backend_ix86_64.c"
#include "targets/tcg_backend_arm.c"


// this is the interface function between the compiler and the TCG frontend
// -- the compiler calls it to convert one compiler token at a time into TCG frontend statements
void emit_to_target()
{
}


// ALL THE CODE TO CONVERT COMPILER TOKENS TO TCG FRONTEND STATEMENTS GOES HERE

// also, all the TCG code that optimizes & etc., and then calls the TCG backend



#endif