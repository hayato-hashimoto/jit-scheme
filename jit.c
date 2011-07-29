#include <chicken.h>
#include <malloc.h>
#include <stdio.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <sys/types.h>
#include "jit.h"

/* debug mode flag */
int debug = 0;

/* the address for the executable memory area */
char *exec_ptr;

/* the address for the heap memory area for compiled program */
char *heap_ptr;
char *heap0;

void assign (void *dest, void *src) {
  *(void * *)dest = src;}

/* Call setter function written in Chicken */
void C_to_Chicken (u_int64_t i) {
  C_to_Chicken_higher32 ((u_int32_t) (i >> 32));
  C_to_Chicken_lower32 ((u_int32_t) i);
}

/* may be called in the binary */

u_int64_t exec_eval (u_int64_t scm_obj) {

/* Save the necessary information and run foreign-to-binary function,
 * in some extent simular to the Chicken's foreign-safe-lambda.
 */
   u_int64_t ret;
   __asm__ volatile (
   "mov %%rbx, %0\n"
   : "=r" (heap_ptr)
   :
   : "rbx");

/* Call Chicken function */
  C_to_Chicken (scm_obj);
  system_eval();
  ret = (((u_int64_t) Chicken_to_C_higher32()) << 32) + Chicken_to_C_lower32();

/* Aftermath, we need restoration. */
  __asm__ volatile (
    "mov %0, %%rbx\n"
   : 
   : "r" (heap_ptr)
   : "rbx");
  return ret;
}

/* execute machine code binary code. */ 
u_int64_t exec_binary (int len, char *code) {
  u_int64_t ret;
  char *code_heap_ptr = heap_ptr;
  /* copy exec_ptr to lexical variable, which is needed for callback (binary -> other -> binary). */
  char *code_exec_ptr = exec_ptr;

  /* copy the machine code to the excutable memory */
  memcpy (code_exec_ptr, code, len);
  exec_ptr += len;

  // printf ("Current heap block is at %Lx\n", code_heap_ptr);
  __asm__ volatile (
    "mov %0, %%rbx"
   : 
   : "r" (code_heap_ptr)
   : "rbx");

  /* fire the code :) */ 
  ret = ((u_int64_t (*) (char*)) code_exec_ptr) (code_heap_ptr + 32);

  __asm__ volatile (
   "mov %%rbx, %0"
   : "=r" (code_heap_ptr)
   :
   : "rbx");

  if (debug && code_heap_ptr - heap_ptr != 0) {
    printf ("heap used: 0x%x\n", code_heap_ptr - heap_ptr);
    printf ("heap used (total): 0x%x\n", code_heap_ptr - heap0);}
  heap_ptr = code_heap_ptr;
  C_to_Chicken (ret);
  return 0;
}

u_int32_t binary_address_higher32 () {
  u_int32_t ret = (u_int32_t) (((u_int64_t) exec_ptr)  >> 32);
  //  printf ("higher:%x\n", ret);
  return ret;
}

u_int32_t binary_address_lower32 () {
  u_int32_t ret = (u_int32_t) (u_int64_t) exec_ptr;
  //  printf ("lower:%x\n", ret);
  return ret;
}

void prepare (char* filename) {
/* map the executable area to an (existing) file for the debug purposes */  
  int fd    = open (filename, O_RDWR);
  if (fd != -1) debug = 1;
  exec_ptr  = (debug) ? mmap (0, 8192, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_SHARED, fd, 0)
                      : mmap (0, 8192, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

  if (debug) { printf ("executable area set at %Lx\n", exec_ptr); }
  heap0 = heap_ptr = mmap (0, 40960, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  heap_ptr += 32;
  if (debug) { printf ("heap area set at %Lx\n", heap_ptr); }
}

int main (int argc, char** argv) {
  prepare (argv[1]);
  C_word k = CHICKEN_run (C_toplevel);
}

