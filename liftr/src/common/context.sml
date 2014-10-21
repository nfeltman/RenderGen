
structure Contexts = 
struct

exception UnboundVar of string
exception WrongStage

type ('a,'b) context = ('a * 'b) list

val empty = []

fun extendContext g v t = (v,t) :: g
fun lookup [] v = raise (UnboundVar "")
  | lookup ((v2,t)::g) v = if v = v2 then t else lookup g v
  

structure DoubleContext = struct  
datatype ('a,'b) doubleEntry = Bind1 of 'a | Bind2 of 'b
fun extendContext1 g v t = (v, Bind1 t) :: g
fun extendContext2 g v t = (v, Bind2 t) :: g
fun lookup1 g v = case lookup g v of Bind1 t => t | Bind2 _ => raise (WrongStage)
fun lookup2 g v = case lookup g v of Bind2 t => t | Bind1 _ => raise (WrongStage)
val extendLookup1 = (extendContext1,lookup1)
val extendLookup2 = (extendContext2,lookup2)
end

structure TripleContext = struct  
datatype ('a,'b,'c) tripleEntry = Bind1 of 'a | Bind2 of 'b | Bind3 of 'c
fun extendContext1 g v t = (v, Bind1 t) :: g
fun extendContext2 g v t = (v, Bind2 t) :: g
fun extendContext3 g v t = (v, Bind3 t) :: g
fun lookup1 g v = case lookup g v of Bind1 t => t | _ => raise (WrongStage)
fun lookup2 g v = case lookup g v of Bind2 t => t | _ => raise (WrongStage)
fun lookup3 g v = case lookup g v of Bind3 t => t | _ => raise (WrongStage)
val extendLookup1 = (extendContext1,lookup1)
val extendLookup2 = (extendContext2,lookup2)
val extendLookup2 = (extendContext3,lookup3)
end

end
