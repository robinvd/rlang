binary 1 ; (a : x, b : y) -> y = b

binary 1 + (a : Num, b : Num) -> Num = plus(a, b)
binary 1 - (a : Num, b : Num) -> Num = min(a, b)
binary 1 == (a : x, b : x) -> Num = eq(a, b)
binary 1 % (a : x, b : x) -> Num = mod(a, b)

extern "stdio.h" puts (String) -> ()
extern "stdio.h" plus (Num, Num) -> Num

id(a : x) -> x = a

main() -> Num = fib(6)

fib(x : Num) -> Num = if x == 0
  then 0
  else if x == 1
    then 1
    else fib(x - 2) + fib(x - 1) # plus(fib(min(x, 2)), fib(min(x, 1)))
    end
  end

fwithargs(x : Num, y : Num) -> Num = plus(x, y)

a() -> Num = main()

vartest() -> Num = id(0)

test() -> String =
  puts("test");
  plus(2, 2);
  "done with test func"
