
signature TypeProvider = 
sig
type t
val Tint : t
val Tbool : t
end

structure Prims = 
struct

datatype binops = Iplus | Iminus | Itimes | Iless | Igreater 
				| Iequal | Ilesseq | Igreatereq | Imod | Idiv
				| Band | Bor

functor PrimTyper (P : TypeProvider) = 
struct

fun getTypes bo = 
	case bo of 
	  Iplus => (P.Tint, P.Tint, P.Tint)
	| Iminus => (P.Tint, P.Tint, P.Tint)
	| Imod => (P.Tint, P.Tint, P.Tint)
	| Itimes => (P.Tint, P.Tint, P.Tint)
	| Idiv => (P.Tint, P.Tint, P.Tint)
	| Iless => (P.Tint, P.Tint, P.Tbool)
	| Igreater => (P.Tint, P.Tint, P.Tbool)
	| Iequal => (P.Tint, P.Tint, P.Tbool)
	| Ilesseq => (P.Tint, P.Tint, P.Tbool)
	| Igreatereq => (P.Tint, P.Tint, P.Tbool)
	| Band => (P.Tbool, P.Tbool, P.Tbool)
	| Bor => (P.Tbool, P.Tbool, P.Tbool)

end

structure PrimEval = 
struct

datatype primValue = Vint of int | Vbool of bool

exception PrimStuck

fun evalPrim (primop, Vint j, Vint k) = (
	case primop of
	  Iplus => Vint (j+k)
	| Iminus => Vint (j-k)
	| Itimes => Vint (j*k)
	| Idiv => Vint (j div k)
	| Imod => Vint (j mod k)
	| Iless => Vbool (j<k)
	| Igreater => Vbool (j>k)
	| Iequal => Vbool (j=k)
	| Ilesseq => Vbool (j<=k)
	| Igreatereq => Vbool (j>=k)
	| _ => raise PrimStuck)
  | evalPrim (primop, Vbool j, Vbool k) = (
	case primop of
	  Band => Vbool (j andalso k)
	| Bor => Vbool (j orelse k)
	| _ => raise PrimStuck)
  | evalPrim _ = raise PrimStuck
end

end
