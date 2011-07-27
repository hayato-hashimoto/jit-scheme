all: jitscheme
jitscheme: jit.c jit.h main.scm
	csc -embedded jit.c main.scm -o jitscheme
clean:
	rm jitscheme
