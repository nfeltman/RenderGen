
structure ValueComparison = 
struct

local
open PSFSemantics
open Lambda12
open SourceLang
structure E = ErasureSemantics
structure D = DiagonalSemantics

infixr 9 `
fun a ` b = a b

in

exception ConversionError

fun splitErasureValue1 v = 
		case v of 
		  E.V1 (VFint i) => (Vint i, Vtuple [])
		| E.V1 (VFbool b) => (Vbool b, Vtuple [])
		| E.V1 VFunit => (Vtuple [], Vtuple [])
		| E.V1 (VFroll v) => 
			(case splitErasureValue1 v of 
			(u,w) => (Vroll u, Vroll w))
		| E.V1 (VFtuple vs) => 
			(case unzip ` map splitErasureValue1 vs of 
			(us,ws) => (Vtuple us, Vtuple ws))
		| E.V1 (VFinj (side,v)) => 
			(case splitErasureValue1 v of 
			(u,w) => (Vinj (side,u), w))
		| E.V1 (VFlam (x,e)) => raise ConversionError
		| E.V1next v0 => (Vtuple[], splitErasureValue2 v0)

and splitErasureValue2 v = 
		case v of 
		  E.V2 (VFint i) => Vint i
		| E.V2 (VFbool b) => Vbool b
		| E.V2 VFunit => Vtuple []
		| E.V2 (VFroll v) => Vroll (splitErasureValue2 v)
		| E.V2 (VFtuple vs) => Vtuple (map splitErasureValue2 vs)
		| E.V2 (VFinj (side,v)) => Vinj (side, splitErasureValue2 v)
		| E.V2 (VFlam (x,e)) => raise ConversionError
		
fun splitDiagValue1 v = 
		case v of 
		  D.V1 (VFint i) => (Vint i, D.E Funit) : value * DiagonalSemantics.expr
		| D.V1 (VFbool b) => (Vbool b, D.E Funit)
		| D.V1 VFunit => (Vtuple [], D.E Funit)
		| D.V1 (VFroll v) => 
			(case splitDiagValue1 v of 
			(u,w) => (Vroll u, D.E ` Froll ((),w)))
		| D.V1 (VFtuple vs) => 
			(case unzip ` map splitDiagValue1 vs of 
			(us,ws) => (Vtuple us, D.E ` Ftuple ws)) 
		| D.V1 (VFinj (side,v)) => 
			(case splitDiagValue1 v of 
			(u,w) => (Vinj (side,u), w))
		| D.V1 (VFlam (x,e)) => raise ConversionError
		| D.V1hat y => (Vtuple [], D.E ` Fvar y)
		
fun convertDiagValue2 v = 
		case v of 
		  D.V2 (VFint i) => Vint i
		| D.V2 (VFbool b) => Vbool b
		| D.V2 VFunit => Vtuple []
		| D.V2 (VFroll v) => Vroll (convertDiagValue2 v)
		| D.V2 (VFtuple vs) => Vtuple (map convertDiagValue2 vs)
		| D.V2 (VFinj (side,v)) => Vinj (side, convertDiagValue2 v)
		| D.V2 (VFlam (x,e)) => raise ConversionError
		
fun valueEq v1 v2 = 
	case (v1,v2) of 
	  (Vint i1, Vint i2) => i1 = i2
	| (Vbool b1, Vbool b2) => b1 = b2
	| (Vinj (s1,u1), Vinj (s2,u2)) => (s1 = s2) andalso (valueEq u1 u2)
	| (Vtuple vs1, Vtuple vs2) => LangCommon.listeq valueEq vs1 vs2
	| (Vroll vs1, Vroll vs2) => valueEq vs1 vs2
	| (_,_) => false
end
	
end