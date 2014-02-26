
structure Lambda12c = 
struct

open LangCommon

datatype ty		= Tint
				| Tunit
				| Tbool
				| Tprod of ty * ty
		(*		| Tsum of ty * ty*)
				| Tfut of ty
				
datatype expr	= Evar of var
		(*		| Ecall of var * expr *)
				| Eunit
				| Eint of int
				| Ebool of bool
				| Etuple of expr * expr
				| Epi of LR * expr
				| Eif of expr * expr * expr
		(*		| Einj of LR * ty * expr 
				| Ecase of expr * (var * expr) * (var * expr) *)
				| Elet of expr * (var * expr)
				| Enext of expr
				| Eprev of expr
				| Eerror of ty
				| Ebinop of Prims.binops * expr * expr
(*
datatype topLevelFunc = FuncDec1 of var * ty * ty * var * expr (*| FuncDec2 of var * ty * ty * var * expr *)
type program = topLevelFunc list
*)

end
