
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

exception UnboundVar
exception WrongStage of var

fun assertSame eq (a,b) = if eq a b then a else raise TypeError


type ('a,'b) context = ('a * 'b) list
val empty = []
fun extendContext g v t = (v,t) :: g
fun lookup [] v = raise (UnboundVar)
  | lookup ((v2,t)::g) v = if v = v2 then t else lookup g v
datatype ('a,'b) doubleEntry = Bind1 of 'a | Bind2 of 'b
fun extendContext1 g v t = (v, Bind1 t) :: g
fun extendContext2 g v t = (v, Bind2 t) :: g
fun lookup1 g v = case lookup g v of Bind1 t => t | Bind2 _ => raise (WrongStage v)
fun lookup2 g v = case lookup g v of Bind2 t => t | Bind1 _ => raise (WrongStage v)
val extendLookup1 = (extendContext1,lookup1)
val extendLookup2 = (extendContext2,lookup2)

fun listeq eq (x::xs) (y::ys) = (eq x y) andalso (listeq eq xs ys)
  | listeq eq [] [] = true
  | listeq _ _ _ = false
  
fun zip2 f (a,b) (c,d) = (f a c, f b d)
fun trn ((a,b),(c,d)) = ((a,c),(b,d))
fun bimap f g (a,b) = (f a, g b)

end
