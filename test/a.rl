# binary 1 ; (a : x, b : y) : y =
#   b

extern "" puts(Ptr Char) -> ();
extern "" exitWithCode(Num) -> ();
# extern "" getline() -> (Ptr Char);
extern "" getchar() -> (Char);
extern "" putchar(Char) -> ();
# extern "" malloc(Int) -> (Ptr Char);
# extern "" toInt(Num) -> (Int);
extern "" cmpChar(Char, Char) -> (Bool);
# extern "" poke(Ptr Char, Num, Char) -> ();

# import "test/prelude.rl"

# data StrLen = StrLen (Ptr Char, Num, Num)
# data Double a = Double (a,a)
# data DInt = DInt (Num, Num)

export
main() -> () =
  # let str: StrLen = StrLen("abc", 4, 4)
  # puts("abc\n");
  # let a: DInt = DInt(97,98)
  # let b: Double(Num Num) = Double(97,98)
  # putchar('c');
  # let line: Ptr Char = getline()
  # let test: Bool = cmpChar('1','2')
  # if test then
    # 1
  # else
    # 2
  # end;
  # exitWithCode(0)
  putchar('a');
  ()

# getline() -> Ptr Char = 
#   let buffer: Ptr Char = malloc(toInt(10))
#   f(buffer, 0);
#   buffer
# 
# f(buffer: Ptr Char, current: Num) -> () =
#   let in: Char = getchar()
#   if cmpChar(in, '\n') then
#     ()
#   else
#     poke(buffer, current, in);
#     f(buffer, current + 1)
#   end

# test() -> Num = 0
# l() -> Num = 
#   let x : Char = 'c'
#   let x : Num = 1
#   x = 3;
#   x
# 
# pair() -> (Num,Num) = 
#   let pair: (Num, Num) = (1,2)
#   pair

# fst(x: (Num,Num)) -> Num =
#   # let (x,y): (Num,Num) = x
#   x

