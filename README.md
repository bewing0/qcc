# qcc
A superfast C compiler inspired by TinyCC

If you ever looked at Rob's fork of TCC, this program is constructed very much the same way.

The file with the most important source code is in source/qcc.c.
It uses #includes to incorporate all the code from all the other files.
So if you just want to read the code, that is a good place to start.

If you should happen to want to try compiling, or doing singlestep runs in an IDE or something,
then that is the only file you need to compile.

An example compilation might look something like:
cd source
xcc -I.. qcc.c
