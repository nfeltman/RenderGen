
structure ValueComparison = 
struct

open PSFSemantics
structure E = ErasureSemantics
structure D = DiagonalSemantics

fun splitErasureValue1 v = 
		case v of 
		  E.V1int i => (Vint i, Vtuple [])
		| E.V1bool b => (Vbool b, Vtuple [])
		| E.V1unit => (Vtuple [], Vtuple [])
		| E.V1tuple (v1,v2) => 
			(case (splitErasureValue1 v1, splitErasureValue1 v2) of 
			((u1,w1),(u2,w2)) => (Vtuple [u1,u2], Vtuple [w1,w2]))
		| E.V1next v0 => (Vtuple[], splitErasureValue2 v0)

and splitErasureValue2 v = 
		case v of 
		  E.V2int i => Vint i
		| E.V2bool b => Vbool b
		| E.V2unit => Vtuple []
		| E.V2tuple (v1,v2) => Vtuple [splitErasureValue2 v1, splitErasureValue2 v2]
		
fun convertDiagValue v = 
		case v of 
		  D.Vint i => Vint i
		| D.Vbool b => Vbool b
		| D.Vunit => Vtuple []
		| D.Vtuple (v1,v2) => Vtuple [convertDiagValue v1, convertDiagValue v2]
		
fun valueEq v1 v2 = 
	case (v1,v2) of 
	  (Vint i1, Vint i2) => i1 = i2
	| (Vbool b1, Vbool b2) => b1 = b2
	| (Vinj (s1,u1), Vinj (s2,u2)) => (s1 = s2) andalso (valueEq u1 u2)
	| (Vtuple vs1, Vtuple vs2) => LangCommon.listeq valueEq vs1 vs2
	| (_,_) => false

end