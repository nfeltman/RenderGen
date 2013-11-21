
structure LangCommon = 
struct

type var = string
datatype LR = Left | Right

fun projLR Left  (a,_) = a
  | projLR Right (_,b) = b

fun injLR Left  a t = (a,t)
  | injLR Right b t = (t,b)

exception UnboundVar of var
type 'a context = (var * 'a) list

val empty = []

fun extendContext g v t = (v,t) :: g

fun lookup [] v = raise (UnboundVar v)
  | lookup ((v2,t)::g) v = if v = v2 then t else lookup g v


fun listeq eq (x::xs) (y::ys) = (eq x y) andalso (listeq eq xs ys)
  | listeq eq [] [] = true
  | listeq _ _ _ = false
  
fun zip2 f (a,b) (c,d) = (f a c, f b d)
fun trn ((a,b),(c,d)) = ((a,c),(b,d))

end
