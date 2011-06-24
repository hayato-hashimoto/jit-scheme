all: jitscheme
jitscheme: embed.c main.scm
	csc -embedded embed.c main.scm -o jitscheme
clean:
	rm jitscheme
