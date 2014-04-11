
structure LangCommon = 
struct

type var = Variable.variable
datatype LR = Left | Right

fun projLR Left  (a,_) = a
  | projLR Right (_,b) = b

fun injLR Left  a t = (a,t)
  | injLR Right b t = (t,b)

exception TypeError
exception ParseError
exception Stuck

fun assertSame eq (a,b) = if eq a b then a else raise TypeError


fun listeq eq (x::xs) (y::ys) = (eq x y) andalso (listeq eq xs ys)
  | listeq eq [] [] = true
  | listeq _ _ _ = false
  
fun zip2 f (a,b) (c,d) = (f a c, f b d)
fun trn ((a,b),(c,d)) = ((a,c),(b,d))
fun bimap f g (a,b) = (f a, g b)

end
