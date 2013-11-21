
structure LambdaPSF = 
struct

open LangCommon

datatype ty		= Tprod of ty list
				| Tsum of ty * ty
				| Tfunc of ty * ty
				
datatype expr	= Evar of var
				| Elam of ty * (var * expr)
				| Eapp of expr * expr
				| Etuple of expr list
				| Epi of int * expr
				| Einj of LR * ty * expr
				| Ecase of expr * (var * expr) * (var * expr)
				| Elet of expr * (var * expr)
				| Eerror
				
type cont = ty context

end
