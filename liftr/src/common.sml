
structure LangCommon = 
struct

datatype var = string
datatype LR = Left | Right

exception UnboundVar of var
type 'a context = (var * 'a) list

fun extendContext g v t = (v,t) :: g

fun lookup [] v = raise (UnboundVar v)
  | lookup ((v2,t)::g) v = if v = v2 then t else lookup g v

end
