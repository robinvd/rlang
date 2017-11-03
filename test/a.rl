# binary 1 ; (a : x, b : y) : y =
#   b

# extern "stdio.h" puts (String) -> ()

puts(a: Num) -> () = ()

main() -> Num =
  puts(0);
  5 + test()

test() -> Num =
  if 1 then
    puts(2);
    2
  else
    3
  end

f(x : (Num) -> Num, a : Num) -> Num = x(2) + a

c() -> Char = 'c'
