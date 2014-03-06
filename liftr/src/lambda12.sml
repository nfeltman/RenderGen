
structure Lambda12 = 
struct

open LangCommon

datatype 't typeF	= TFint
					| TFbool
					| TFunit
					| TFprod of 't * 't

datatype ('e,'t) exprF	= Fvar of var
						| Funit
						| Fint of int
						| Fbool of bool
						| Ftuple of 'e * 'e
						| Fpi of LR * 'e
						| Fif of 'e * 'e * 'e
						| Flet of 'e * (var * 'e)
						| Ferror of 't
						| Fbinop of Prims.binops * 'e * 'e

fun mapType _ TFint = TFint
  | mapType _ TFbool = TFbool
  | mapType _ TFunit = TFunit
  | mapType f (TFprod (t1,t2)) = TFprod (f t1, f t2)
						
fun mapExpr fe ft exp =
	case exp of
	  Fvar v => Fvar v
	| Funit => Funit
	| Fint i => Fint i
	| Fbool b => Fbool b
	| Ftuple (e1,e2) => Ftuple (fe e1, fe e2)
	| Fpi (lr, e) => Fpi (lr, fe e)
	| Fif (e1,e2,e3) => Fif (fe e1, fe e2, fe e3)
	| Flet (e1, (x,e2)) => Flet (fe e1, (x, fe e2))
	| Ferror (t) => Ferror (ft t)
	| Fbinop (bo,e1,e2) => Fbinop(bo, fe e1, fe e2)
	

datatype type1	= T1 of type1 typeF
				| T1fut of type2

and type2		= T2 of type2 typeF

datatype expr1	= E1 of (expr1,type1) exprF
				| E1next of expr2
				| E1hold of expr1

and expr2		= E2 of (expr2,type2) exprF
				| E2prev of expr1

				
datatype contEntry = Stage1 of type1 | Stage2 of type2 | Func1 of type1 * type1 (* | Func2 of type2 * type2 *)
type cont = contEntry context
(*
datatype topLevelFunc = FuncDec1 of var * type1 * type1 * var * expr1 (*| FuncDec2 of var * type2 * type2 * var * expr2 *)
type program = topLevelFunc list
*)

end
