
structure LambdaPSF = 
struct

open LangCommon

datatype ty		= Tunit
				| Tprod of ty * ty
				| Tsum of ty * ty
				| Tfunc of ty * ty
				
datatype expr	= Evar of var
				| Elam of var * ty * expr
				| Eapp of expr * expr
				| Eunit
				| Etuple of expr * expr
				| Epi of LR * expr
				| Einj of LR * ty * expr
				| Ecase of expr * (var * expr) * (var * expr)
				
type cont = ty context

end
