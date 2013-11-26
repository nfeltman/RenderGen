
signature TypeProvider = 
sig
type t
val Tint : t
end

structure Prims = 
struct

datatype binops = Plus | Minus | Times

functor PrimTyper (P : TypeProvider) = 
struct

fun getTypes bo = 
	case bo of 
	  Plus => (P.Tint, P.Tint, P.Tint)
	| Minus => (P.Tint, P.Tint, P.Tint)
	| Times => (P.Tint, P.Tint, P.Tint)

end

end
