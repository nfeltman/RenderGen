
structure LambdaPSF = 
struct

infixr 9 `
fun a ` b = a b

open LangCommon
structure S = SourceLang

datatype pattern = P of (var,pattern) S.pattern
datatype expr 	= E of (expr,var,pattern * expr,unit) S.exprF

fun Evar x = E ` S.Fvar x
fun Eprim x = E ` S.SEdata ` DataFrag.EprimVal x
fun Elam x = E ` S.Flam x
fun Eapp x = E ` S.Fapp x
fun Etuple x = E ` S.SEprod ` BranchlessFrag.Etuple x
fun Epi x = E ` S.SEprod ` BranchlessFrag.Epi x
fun Einj x = E ` S.SEdata ` DataFrag.Einj x
fun Ecase x = E ` S.SEdata ` DataFrag.Ecase x 
fun Eif x = E ` S.SEdata ` DataFrag.Eif x
fun Elet x = E ` S.Flet x
fun Ebinop x = E ` S.SEdata ` DataFrag.Ebinop x
fun Eroll x = E ` S.Froll x
fun Eunroll x = E ` S.Funroll x
fun Eerror x = E ` S.SEdata ` DataFrag.Eerror x
		
end
