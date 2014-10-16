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

datatype binops = O2plus | O2minus | O2times | O2less | O2greater 
				| O2equal | O2lesseq | O2greatereq | O2mod | O2div
				| O2and | O2or | O2cat
			
fun getBinopType bo = 
	case bo of 
	  O2plus => (Tint, Tint, Tint)
	| O2minus => (Tint, Tint, Tint)
	| O2mod => (Tint, Tint, Tint)
	| O2times => (Tint, Tint, Tint)
	| O2div => (Tint, Tint, Tint)
	| O2less => (Tint, Tint, Tbool)
	| O2greater => (Tint, Tint, Tbool)
	| O2equal => (Tint, Tint, Tbool)
	| O2lesseq => (Tint, Tint, Tbool)
	| O2greatereq => (Tint, Tint, Tbool)
	| O2and => (Tbool, Tbool, Tbool)
	| O2or => (Tbool, Tbool, Tbool)
	| O2cat => (Tstr, Tstr, Tstr)

fun unbool (Vbool b) = b
  | unbool _ = raise PrimStuck

fun evalPrim (primop, Vint j, Vint k) = (
	case primop of
	  O2plus => Vint (j+k)
	| O2minus => Vint (j-k)
	| O2times => Vint (j*k)
	| O2div => Vint (j div k)
	| O2mod => Vint (j mod k)
	| O2less => Vbool (j<k)
	| O2greater => Vbool (j>k)
	| O2equal => Vbool (j=k)
	| O2lesseq => Vbool (j<=k)
	| O2greatereq => Vbool (j>=k)
	| _ => raise PrimStuck)
  | evalPrim (primop, Vbool j, Vbool k) = (
	case primop of
	  O2and => Vbool (j andalso k)
	| O2or => Vbool (j orelse k)
	| _ => raise PrimStuck)
  | evalPrim (primop, Vstr j, Vstr k) = (
	case primop of
	  O2cat => Vstr (j ^ k)
	| _ => raise PrimStuck)
  | evalPrim _ = raise PrimStuck
  
end

