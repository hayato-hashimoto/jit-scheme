#include <chicken.h>
#include <assert.h>
#include <malloc.h>
#include <stdio.h>
#include <sys/mman.h>
#include <readline/readline.h>
#include <fcntl.h>

long long int service (long long int num, long long int arg) {
 printf ("hook\n");
 return 0;
}

int main (int argc, char** argv) {
  char str[1024], *text, *rtext;
  C_word *result;
  result = malloc(1024);
  rtext = malloc(1024);
  int status;
  C_word val = C_SCHEME_UNDEFINED;
  C_word *data[1];
  data[0] = &val;
  C_word k = CHICKEN_run (C_toplevel);
  int fd    = open (argv[1], O_RDWR);
  char* c   = (fd != -1) ? mmap (0, 4096, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_SHARED, fd, 0)
                         : mmap (0, 4096, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANON, fd, 0);
  char* mem = mmap (0, 4096, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
  printf ("code will be loaded at 0x%Lx\n", c);
 while (1) {
  sprintf (str, "(read-compile #x%Lx)", c);
  status = CHICKEN_eval_string (str, result);
  assert (status);
  int i, j;
  int len = *((int *) *result);
  char* binary = ((char *) *result) + 8;
  for (i=0;i<len;) {
  // printf ("0x%x: ", c+i); 
   for (j=0;j<16;j++) {
    *(c+i) = *(binary+i);
  //  printf ("%02hhx ", *(c+i));
    i++;}
  // printf ("\n"); 
  }
  long long int v = ((long long int (*) (char*, int, long long int (*) (long long int, long long int))) c)  (mem+10, 4096, service);
  printf (" => %d (0x%Lx)\n", v, v);
 }
  return 0;
}
