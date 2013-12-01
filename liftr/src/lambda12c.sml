
structure Lambda12c = 
struct

open LangCommon

datatype ty		= Tint
				| Tunit
				| Tbool
				| Tprod of ty * ty
				| Tsum of ty * ty
				| Tfut of ty
				
datatype expr	= Evar of var
				| Ecall of var * expr
				| Eunit
				| Etuple of expr * expr
				| Epi of LR * expr
				| Einj of LR * ty * expr
				| Ecase of expr * (var * expr) * (var * expr)
				| Enext of expr
				| Eprev of expr
<<<<<<< HEAD
				| Eerror of ty
=======
				| Ebinop of Prims.binops * expr * expr
>>>>>>> 42bffa4e7912b8d71bffd749d495656dd889b880

datatype topLevelFunc = FuncDec1 of var * ty * ty * var * expr (*| FuncDec2 of var * ty * ty * var * expr *)
type program = topLevelFunc list

end
