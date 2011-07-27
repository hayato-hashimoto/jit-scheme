all: jitscheme
assembler.o: assembler.scm
	csc assembler.scm -unit assembler -c
jitscheme: jit.c jit.h main.scm assembler.o
	csc -embedded jit.c main.scm -uses assembler -prelude '(provide (quote assembler))' assembler.o -o jitscheme
clean:
	rm jitscheme
