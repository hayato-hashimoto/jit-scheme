#include <chicken.h>
#include <assert.h>

int main() {
  char buffer [256];
  int status;
  C_word val = C_SCHEME_UNDEFINED;
  C_word *data[1];
  data[0] = &val;
  C_word k = CHICKEN_run (C_toplevel);

  //status = CHICKEN_read ("(bar 99)", &val);
  //assert (status);

  //C_gc_protect (data, 1);

  //printf ("data: %08x\n", val);

  CHICKEN_continue (k);

  return 0;
}
