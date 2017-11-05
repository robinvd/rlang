# binary 1 ; (a : x, b : y) : y =
#   b

extern "stdio.h" putchar (Char) -> ();
extern "stdlib.h" malloc (Num) -> (Ptr Num);

puts(a: Num) -> () = ()

myFree(a: Ptr Num) -> () = ()

main() -> Num =
  # puts(0);
  5 + 1

test() -> Num =
  if 1 then
    putchar('a');
    2
  else
    # malloc(5);
    putchar('b');
    putchar('\n');
    # f(n, 1)
    3
  end

f(x : (Num) -> Num, a : Num) -> Num = x(2) + a

c() -> Char = 'c'

n(a:Num) -> Num = a+1

# l() -> Num = 
#   let x : Num = 1
#   x + 2

# pair() -> (Num, Num) = 
#   malloc(32);
#   (1,2)

