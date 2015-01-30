
structure Lambda12 = 
struct

datatype type1	= T1 of type1 TypesBase.typeF
				| T1fut of type2
				| T1now of type2

and type2		= T2 of type2 TypesBase.typeF

datatype patternM = PM of (LangCommon.var, patternM) SourceLang.pattern
datatype pattern12	= P of (LangCommon.var, pattern12) SourceLang.pattern
					| Pmono of patternM
					| Pnext of patternM

datatype expr1	= E1 of (expr1,LangCommon.var,pattern12 * expr1,type1) SourceLang.exprF
				| E1next of expr2
				| E1hold of expr1
				| E1mono of exprM
				| E1pushPrim of expr1
				| E1pushSum of expr1

and expr2		= E2 of (expr2,LangCommon.var,patternM * expr2,type2) SourceLang.exprF
				| E2prev of expr1

and exprM		= EM of (exprM,LangCommon.var,patternM * exprM,type2) SourceLang.exprF

end
