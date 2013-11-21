
structure Lambda12c = 
struct

open LangCommon

datatype ty		= Tunit
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

datatype topLevelFunc = FuncDec1 of var * ty * ty * var * expr (*| FuncDec2 of var * ty * ty * var * expr *)
type program = topLevelFunc list

end
