
structure ValueComparison = 
struct

local
open PSFSemantics
open Lambda12
open SourceLang
structure E = ErasureSemantics
structure D = DiagonalSemantics
open ValuesBase

infixr 9 `
fun a ` b = a b

val Vprim = V o VFprim
val Vinj = V o VFinj
val Vroll = V o VFroll
val Vtuple = V o VFtuple
fun Ftuple x = SEprod ` BranchlessFrag.Etuple x

in

exception ConversionError

fun convertValue v = 
		case v of 
		  E.V1 (VFprim i) => Vprim i
		| E.V1 (VFroll v) => Vroll (convertValue v)
		| E.V1 (VFtuple vs) => Vtuple (map convertValue vs)
		| E.V1 (VFinj (side,v)) => Vinj (side, convertValue v)
		| E.V1 (VFlam _) => raise ConversionError
		| E.V1next _ => raise ConversionError
		| E.V1mono _ => raise ConversionError

fun splitErasureValue1 v = 
		case v of 
		  E.V1 (VFprim v) => (Vprim v, Vtuple [])
		| E.V1 (VFroll v) => 
			(case splitErasureValue1 v of 
			(u,w) => (Vroll u, w))
		| E.V1 (VFtuple vs) => 
			(case unzip ` map splitErasureValue1 vs of 
			(us,ws) => (Vtuple us, Vtuple ws))
		| E.V1 (VFinj (side,v)) => 
			(case splitErasureValue1 v of 
			(u,w) => (Vinj (side,u), w))
		| E.V1 (VFlam _) => raise ConversionError
		| E.V1next v => (Vtuple[], convertValue v)
		| E.V1mono v => (convertValue v, Vtuple[])

		
fun splitDiagValue1 v = 
		case v of 
		  D.V1 (VFprim i) => (Vprim i, D.E ` Ftuple []) : value * DiagonalSemantics.expr
		| D.V1 (VFroll v) => 
			(case splitDiagValue1 v of 
			(u,w) => (Vroll u, w))
		| D.V1 (VFtuple vs) => 
			(case unzip ` map splitDiagValue1 vs of 
			(us,ws) => (Vtuple us, D.E ` Ftuple ws)) 
		| D.V1 (VFinj (side,v)) => 
			(case splitDiagValue1 v of 
			(u,w) => (Vinj (side,u), w))
		| D.V1 (VFlam _) => raise ConversionError
		| D.V1mono v => (splitDiagValueM v, D.E ` Ftuple[])
		| D.V1hat y => (Vtuple [], D.E ` Fvar y)
		
and splitDiagValueM v = 
		case v of 
		  D.VM (VFprim i) => Vprim i
		| D.VM (VFroll v) => Vroll (splitDiagValueM v)
		| D.VM (VFtuple vs) => Vtuple (map splitDiagValueM vs)
		| D.VM (VFinj (side,v)) => Vinj (side, splitDiagValueM v)
		| D.VM (VFlam _) => raise ConversionError
		
fun convertDiagValue2 v = 
		case v of 
		  D.VM (VFprim i) => Vprim i
		| D.VM (VFroll v) => Vroll (convertDiagValue2 v)
		| D.VM (VFtuple vs) => Vtuple (map convertDiagValue2 vs)
		| D.VM (VFinj (side,v)) => Vinj (side, convertDiagValue2 v)
		| D.VM (VFlam _) => raise ConversionError
		
fun valueEq (V v1) (V v2) = 
	case (v1,v2) of 
	  (VFprim i1, VFprim i2) => i1 = i2
	| (VFinj (s1,u1), VFinj (s2,u2)) => (s1 = s2) andalso (valueEq u1 u2)
	| (VFtuple vs1, VFtuple vs2) => LangCommon.listeq valueEq vs1 vs2
	| (VFroll vs1, VFroll vs2) => valueEq vs1 vs2
	| (_,_) => false
end
	
end