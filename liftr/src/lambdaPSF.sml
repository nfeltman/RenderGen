
structure LambdaPSF = 
struct

open LangCommon

datatype ty		= Tint
				| Tbool
				| Tprod of ty list
				| Tsum of ty * ty
				| Tfunc of ty * ty
				| Tvar of int
				| Trec of ty
				
datatype expr	= Evar of var
				| Elam of ty * (var * expr)
				| Eapp of expr * expr
				| Etuple of expr list
				| Epi of int * expr
				| Einj of LR * ty * expr
				| Ecase of expr * (var * expr) * (var * expr)
				| Eif of expr * expr * expr
				| Elet of expr * (var * expr)
				| Ebinop of Prims.binops * expr * expr
				| Eroll of expr
				| Eunroll of expr
				| Eerror of ty
				
type cont = ty context

end
