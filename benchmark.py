def l (cont, arg1, arg2):
 if (arg1 == 0): return arg2
 else : return cont(cont, arg1 - 1, arg1 + arg2)

def calc ():
 return (lambda a, b, c: a(a, b, c)) (
   l, 600, 1);

def loop (n):
  calc();
  if (n == 0): return calc()
  else : return loop (n - 1);

print (loop(200));
