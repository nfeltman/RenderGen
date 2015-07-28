
structure Lambda12 = 
struct

datatype type12	= Tcore of type12 TypesBase.typeF
				| Tfut of type12
				| Tnow of type12

datatype pattern12	= P of (LangCommon.var, pattern12) SourceLang.pattern
					| Pmono of pattern12
					| Pnext of pattern12

datatype expr12	= L12core of (expr12,LangCommon.var,pattern12 * expr12,type12) SourceLang.exprF
				| L12stage of stage
and 	stage	= E1next of expr12
				| E1hold of expr12
				| E1mono of expr12
				| E1pushPrim of expr12
				| E1pushSum of expr12
				| E2prev of expr12

end
