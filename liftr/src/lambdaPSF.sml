
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
				
datatype 't expr	= Evar of var
					| Eint of int
					| Ebool of bool
					| Elam of 't * (var * 't expr)
					| Eapp of 't expr * 't expr
					| Etuple of 't expr list
					| Epi of int * 't expr
					| Einj of LR * 't * 't expr
					| Ecase of 't expr * (var * 't expr) * (var * 't expr)
					| Eif of 't expr * 't expr * 't expr
					| Elet of 't expr * (var * 't expr)
					| Ebinop of Prims.binops * 't expr * 't expr
					| Eroll of 't expr
					| Eunroll of 't expr
					| Eerror of 't
				
type cont = ty context

end
