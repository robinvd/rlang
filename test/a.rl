# binary 1 ; (a : x, b : y) : y =
#   b

import "test/prelude.rl"

export
main() -> Num =
  let str: Ptr Char = "hello\0"
  poke(str, 0, 'b');
  println(str);
  # println("hello, world");
  # let x: (Ptr Char) = malloc(32)
  # poke("test", 0, 'h');
  # println(x);
  let p: (Num,Num) = pair()
  p -> 1

l() -> Num = 
  let x : Char = 'c'
  let x : Num = 1
  x = 3;
  x

pair() -> (Num,Num) = 
  let pair: (Num, Num) = (1,2)
  pair

# fst(x: (Num,Num)) -> Num =
#   # let (x,y): (Num,Num) = x
#   x

