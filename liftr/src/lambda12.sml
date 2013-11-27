
structure Lambda12 = 
struct

open LangCommon

datatype type1	= T1int
				| T1bool
				| T1unit
				| T1prod of type1 * type1
				| T1sum of type1 * type1
		(*		| T1func of type1 * type1 *)
				| T1fut of type2
				
and type2		= T2int
				| T2bool
				| T2unit
				| T2prod of type2 * type2
				| T2sum of type2 * type2
		(*		| T2func of type2 * type2 *)
				
				
datatype expr1	= E1var of var
		(*		| E1lam of type1 * (var * expr1)
				| E1app of expr1 * expr1 *)
				| E1call of var * expr1
				| E1unit
				| E1tuple of expr1 * expr1
				| E1pi of LR * expr1
				| E1inj of LR * type1 * expr1
				| E1case of expr1 * (var * expr1) * (var * expr1)
				| E1next of expr2
				| E1binop of Prims.binops * expr1 * expr1

and expr2		= E2var of var
		(*		| E2lam of type2 * (var * expr2)
				| E2app of expr2 * expr2 *)
		(*		| E2call of var * expr2 *)
				| E2unit
				| E2tuple of expr2 * expr2
				| E2pi of LR * expr2
				| E2inj of LR * type2 * expr2
				| E2case of expr2 * (var * expr2) * (var * expr2)
				| E2prev of expr1
				| E2binop of Prims.binops * expr2 * expr2

datatype contEntry = Stage1 of type1 | Stage2 of type2 | Func1 of type1 * type1 (* | Func2 of type2 * type2 *)
type cont = contEntry context

datatype topLevelFunc = FuncDec1 of var * type1 * type1 * var * expr1 (*| FuncDec2 of var * type2 * type2 * var * expr2 *)
type program = topLevelFunc list

end
