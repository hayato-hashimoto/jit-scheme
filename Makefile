all: jitscheme
jitscheme: jit.c main.scm
	csc -embedded jit.c main.scm -o jitscheme
clean:
	rm jitscheme
