
structure LangCommon = 
struct

type var = Variable.variable

exception TypeError of string
exception ParseError
exception Stuck

fun id x = x

fun listeq eq (x::xs) (y::ys) = (eq x y) andalso (listeq eq xs ys)
  | listeq eq [] [] = true
  | listeq _ _ _ = false
  
fun zip2 f (a,b) (c,d) = (f a c, f b d)
fun trn ((a,b),(c,d)) = ((a,c),(b,d))
fun bimap f g (a,b) = (f a, g b)

fun unzip [] = ([],[])
  | unzip ((x,y)::r) = let val (xs,ys) = unzip r in (x::xs,y::ys) end

fun zip _ [] [] _ = []
  | zip f (x::xs) (y::ys) ex = (f (x,y)) :: (zip f xs ys ex)
  | zip _ _ _ ex = raise ex

end
