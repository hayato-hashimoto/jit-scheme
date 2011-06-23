all: jitscheme
jitscheme:
	csc -embedded embed.c main.scm -o jitscheme
clean:
	rm jitscheme
