# Sol
# Basically the same as FPRISC but has a VM (almost like its own system with a HAL forwarding stuff to the host via the haskell implementation and std libs) instead of a compiler.
# Sol = FPRISC + VM + Automatic STM and IO + extra syntax sugar for paths
# Can also do > like > print "hello" anywhere for a sequential effect
# Paths are logical URLs that map to a file on the host. This allows STM to be done quite easily per script session

MyType = Type (MyInt Int | MyString String Int). # tagged union, two variants
Length = Nat.                                    # alias: parsed, skipped

inc x = x + 1.                                   # binding, function

# multi-clause + guards --> if-else chain
classify x | x == 0 = "zero".
classify x | x < 0 = "negative".
classify _ = "positive".

# let-sugar with ; and nested (parenthesized) case
g2 x =
  y = case x of
    MyInt i -> (case i of 0 -> 1 | _ -> 2)
  | MyString s l -> 3;
  z = 1;
  y + z.

# row-polymorphic record pattern {a} + guard; works on ANY shape with field a
myfunc : {a : String | r} -> String -> Bool.
myfunc {a} b | a != "" = a == b.
myfunc _ _ = False.

myrecord : MyRecord.
MyRecord = {a : String, b : Int}.
myrecord = {a = "x", b = 1}.
myrecord' = {myrecord | b = 2}.                  # record update sugar

nested = {p = {q = 1, r = "deep"}, s = 0}.
nested' = {nested | p.q = 42}.                   # nested-path update --> lens-style rebuild

safe_div n d | d == 0 = Err "div by zero".
safe_div n d = Ok (n / d).

# |> pipeline with a lambda (lambda-lifted, no closure machinery)
pipeline k x =
  x |> inc |> fn y -> y * k.

# |>? monadic pipeline: desugars to case on Ok/Err
chained x =
  Ok x |>? safe_div 100 |>? fn v -> Ok (v + 1).

b = "mystring".
mystring = "g2 = {g2 (MyString b (String.len b))}, third = {[10, 20, 30] ! 3}".

> print mystring.

check = case myfunc {a = "x", c = 9} "x" of True -> "matched" | False -> "no".

main =
  print mystring >>
  print (classify (0 - 5)) >>
  print "b = {myrecord'.b}, deep = {nested'.p.q}" >>
  print (case chained 4 of Ok v -> "ok {v}" | Err e -> "err {e}") >>
  print (pipeline 10 2) >>
  print check.
