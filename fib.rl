
binary 1 + (a : Num, b : Num) -> Num = plus(a, b)
binary 1 - (a : Num, b : Num) -> Num = min(a, b)
binary 1 == (a : x, b : x) -> Num = eq(a, b)

fib(x : Num) -> Num =
  if x == 0 
    then 0
    else if x == 1
      then 1
      else fib(x-1) + fib(x-2)
    end
  end

main() -> Num = fib(11)
