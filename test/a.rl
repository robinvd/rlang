# binary 1 ; (a : x, b : y) : y =
#   b

import "test/prelude.rl"

extern "stdio.h" putchar (Char) -> ();
extern "stdio.h" puts (Ptr Char) -> ();
extern "stdlib.h" malloc (Num) -> (Ptr Num);

main() -> Num =
  puts("hello, world\n\0");
  let f: Num = fac(4)
  putchar('\n');
  f

l() -> Num = 
  let x : Char = 'c'
  let x : Num = 1
  x = 3;
  x

pair() -> (Num, Num) = 
  let pair: (Num, Num) = (1,2) # malloc(32);
  pair

# fst(x: (Num,Num)) -> Num =
#   # let (x,y): (Num,Num) = x
#   x

