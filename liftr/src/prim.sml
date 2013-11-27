
signature TypeProvider = 
sig
type t
val Tint : t
val Tbool : t
end

structure Prims = 
struct

datatype binops = Iplus | Iminus | Itimes | Iless | Igreater | Iequal | Ilesseq | Igreatereq | Band | Bor

functor PrimTyper (P : TypeProvider) = 
struct

fun getTypes bo = 
	case bo of 
	  Iplus => (P.Tint, P.Tint, P.Tint)
	| Iminus => (P.Tint, P.Tint, P.Tint)
	| Itimes => (P.Tint, P.Tint, P.Tint)
	| Iless => (P.Tint, P.Tint, P.Tbool)
	| Igreater => (P.Tint, P.Tint, P.Tbool)
	| Iequal => (P.Tint, P.Tint, P.Tbool)
	| Ilesseq => (P.Tint, P.Tint, P.Tbool)
	| Igreatereq => (P.Tint, P.Tint, P.Tbool)
	| Band => (P.Tbool, P.Tbool, P.Tbool)
	| Bor => (P.Tbool, P.Tbool, P.Tbool)

end

end
