structure Prims = 
struct

exception PrimTypeError
exception PrimStuck

datatype primType = Tint | Tbool | Tstr
datatype primValue = Vint of int | Vbool of bool | Vstr of string

fun assertBool Tbool = ()
  | assertBool _ = raise PrimTypeError 

fun getValType (Vint _) = Tint
  | getValType (Vbool _) = Tbool
  | getValType (Vstr _) = Tstr

datatype binops = Iplus | Iminus | Itimes | Iless | Igreater 
				| Iequal | Ilesseq | Igreatereq | Imod | Idiv
				| Band | Bor
			
fun getBinopType bo = 
	case bo of 
	  Iplus => (Tint, Tint, Tint)
	| Iminus => (Tint, Tint, Tint)
	| Imod => (Tint, Tint, Tint)
	| Itimes => (Tint, Tint, Tint)
	| Idiv => (Tint, Tint, Tint)
	| Iless => (Tint, Tint, Tbool)
	| Igreater => (Tint, Tint, Tbool)
	| Iequal => (Tint, Tint, Tbool)
	| Ilesseq => (Tint, Tint, Tbool)
	| Igreatereq => (Tint, Tint, Tbool)
	| Band => (Tbool, Tbool, Tbool)
	| Bor => (Tbool, Tbool, Tbool)

fun unbool (Vbool b) = b
  | unbool _ = raise PrimStuck

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

