
structure Lambda12 = 
struct

local
open LangCommon
open SourceLang
in

datatype type1	= T1 of type1 TypesBase.typeF
				| T1fut of type2

and type2		= T2 of type2 TypesBase.typeF

datatype expr1	= E1 of (expr1,var,type1) exprF
				| E1next of expr2
				| E1hold of expr1

and expr2		= E2 of (expr2,var,type2) exprF
				| E2prev of expr1

end
end
