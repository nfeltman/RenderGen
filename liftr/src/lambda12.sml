
structure Lambda12 = 
struct

open LangCommon

datatype type1	= T1unit
				| T1prod of type1 * type1
				| T1sum of type1 * type1
				| T1func of type1 * type1
				| T1fut of type2
				
and type2		= T2unit
				| T2prod type1 * type1
				| T2sum type1 * type1
				| T2func type1 * type1
				
				
datatype expr1	= E1var of var
				| E1lam of var * type1 * expr1
				| E1app of expr1 * expr1
				| E1unit
				| E1tuple of expr1 * expr1
				| E1pi of LR * expr1
				| E1inj of LR * type1 * expr1
				| E1case of expr1 * (var * expr1) * (var * expr1)
				| E1next of expr2

and expr2		= E2var of var
				| E2lam of var * type1 * expr2
				| E2app of expr2 * expr2
				| E2unit
				| E2tuple of expr2 * expr2
				| E2pi of LR * expr2
				| E2inj of LR * type2 * expr2
				| E2case of expr2 * (var * expr2) * (var * expr2)
				| E2op1 of Op1 * expr2
				| E2prev of expr1
				| E2save of expr2

datatype contEntry = Stage1 of type1 | Stage2 of type2
type cont = contEntry context

fun typeCheck1 gamma exp = 
	case exp of 
	  E1var (v) =>
	| E1lam (v,t,e) =>
	| E1app (e1,e2) => 
	| E1unit => 
	| E1tuple (e1,e2) => 
	| E1case (e1,(v2,e2),(v3,e3)) => 
	| E1op1 (op, e) =>
	| E1next e => 

end
