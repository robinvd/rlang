
binary 1 ; (a : x, b : y) : y = b

# binary 1 + (a : Int, b : Int) : Int = plus(a, b)
# binary 1 - (a : Int, b : Int) : Int = min(a, b)
# binary 1 == (a : Int, b : Int) : Int = eq(a,b)

import stdrl

main() : Int = fib(3)

fac(x : Int) : Int = x + fac(x - 1)

fib(x : Int) : Int = 13 + 1 + 2 + 6 + x
# fib(x : Int) : Int =
#   if x == 0 then
#     0
#   else
#     if x == 1 then
#       1
#     else
#       fib(x - 2) + fib(x - 1)
#     end
#   end
