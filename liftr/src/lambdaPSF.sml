
structure LambdaPSF = 
struct

open LangCommon

datatype ty		= T2unit
				| T1prod type1 * type1
				| T1sum type1 * type1
				| T1func type1 * type1
				
datatype expr	= E1var of var
				| E1lam of var * ty * expr
				| E1app of expr * expr
				| E1unit
				| E1tuple of expr * expr
				| E1case of expr * (var * expr) * (var * expr)
				| E1op1 of Op1 * expr
				
type cont = ty context

end
