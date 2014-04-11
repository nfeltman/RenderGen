
structure Lambda12 = 
struct

open SourceLang

datatype type1	= T1 of type1 typeF
				| T1fut of type2

and type2		= T2 of type2 typeF

datatype expr1	= E1 of (expr1,var,type1) exprF
				| E1next of expr2
				| E1hold of expr1

and expr2		= E2 of (expr2,var,type2) exprF
				| E2prev of expr1

end
