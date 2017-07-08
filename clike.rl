
binary 1 ; (a : x, b : y) : y = b

binary 1 + (a : Int, b : Int) : Int = plus(a, b)
binary 1 == (a : Int, b : Int) : Int = eq(a,b)

import stdrl

main() : Int = fib(10)

fib(x : Int) : Int = 
  if x == 0 then 
    0
  else 
    if x == 1 then
      1
    else 
      fib(x - 2) + fib(x - 1) # plus(fib(min(x, 2)), fib(min(x, 1)))
    end
  end

w() : Int =
  let x : Int = 0
  while not(equals(x, 2))
    set(x, x+1)
  end
