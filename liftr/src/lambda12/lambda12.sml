
structure Lambda12 = 
struct

datatype type1	= T1 of type1 TypesBase.typeF
				| T1fut of type2

and type2		= T2 of type2 TypesBase.typeF

datatype expr1	= E1 of (expr1,LangCommon.var,type1) SourceLang.exprF
				| E1next of expr2
				| E1hold of expr1

and expr2		= E2 of (expr2,LangCommon.var,type2) SourceLang.exprF
				| E2prev of expr1

end
