def calc () {
  return (lambda(a, b, c){return a(a, b, c);}) (
    lambda (cont, arg1, arg2) {if (arg1 = 0) {return arg2;} else {return cont(cont, arg1 - 1, arg1 + arg2)}},
    200000, 1);}
def loop (n) {
  calc();
  return (n = 0) ? calc(): loop(n - 1);}
print loop (8);
