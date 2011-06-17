#include <chicken.h>
#include <assert.h>
#include <malloc.h>
#include <sys/mman.h>
#include <readline/readline.h>

long long int service (long long int num, long long int arg) {
 printf ("hook\n");
 return 0;
}

int main() {
  char str[1024], *text, *rtext;
  C_word *result;
  result = malloc(1024);
  rtext = malloc(1024);
  int status;
  C_word val = C_SCHEME_UNDEFINED;
  C_word *data[1];
  data[0] = &val;
  C_word k = CHICKEN_run (C_toplevel);
  char* c   = mmap (0, 4096, PROT_WRITE|PROT_EXEC|PROT_READ,MAP_PRIVATE|MAP_ANON,-1,0); 
  char* mem = mmap (0, 4096, PROT_WRITE|PROT_READ,MAP_PRIVATE|MAP_ANON,-1,0);
  printf ("code will be loaded at 0x%Lx\n", c);
 while (1) {
  text = readline("test> ");
  sprintf (str, "(opcode-compile '(lambda () %s) #x%Lx)", text, c);
  status = CHICKEN_eval_string_to_string (str, rtext, 1024);
  assert (status);
  sprintf (str, "(binary-compile '(lambda () %s) #x%Lx)", text, c);
  status = CHICKEN_eval_string (str, result);
  assert (status);
  rtext  = C_c_string(result);
  int i, j;
  int len = *((int *) *result);
  char* binary = ((char *) *result) + 8;
  for (i=0;i<len;) {
   printf ("0x%x: ", c+i); 
   for (j=0;j<16;j++) {
    *(c+i) = *(binary+i);
    printf ("%02hhx ", *(c+i));
    i++;}
   printf ("\n", c+i); 
  }
  long long int v = ((long long int (*) (char*, int, long long int (*) (long long int, long long int))) c)  (mem, 4096, service);
  printf (" => %d (0x%Lx)\n", v, v);
 }
  return 0;
}
