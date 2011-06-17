#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <malloc.h>

long call_with_input_char (long cont);
long call_with_output_char (long c, long cont);
long call_with_memory (long size, long cont);
long call_with_freed_memory (long memory, long cont);

char *image;

int main (int argc, char** argv) {
 int fd = open (argv[1], O_RDWR);
 image  = mmap(10000, 24, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_SHARED, fd, 0);
 long r = ((long (*) (long,long,long,long,long,long,long,long,long,long,long)) image) (1, 2, 3, 4, 5, 6, (long) image, ((long) call_with_input_char), ((long) call_with_output_char), ((long) call_with_memory), ((long) call_with_freed_memory));
 printf (" => %ld (0x%lx) \n", r, r);
 return 1;
}

long call_with_input_char (long cont) {
  char c = getchar (); 
  return ((long (*) (long, long, long, long, long, long, long))  cont) (1, 2, 3, 4, 5, 6, ((long) c)); }

long call_with_output_char (long c, long cont) {
  putchar ((char) c);
  return ((long (*) ()) cont) ();}

long call_with_memory (long size, long cont) {
  char *m = malloc (size);
  return ((long (*) (long, long, long, long, long, long, long))  cont) (1, 2, 3, 4, 5, 6, ((long) m));}

long call_with_freed_memory (long memory, long cont) {
  free ((char *) memory);
  return ((long (*) ()) cont) ();}
