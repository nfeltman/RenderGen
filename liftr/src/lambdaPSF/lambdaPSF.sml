
structure LambdaPSF = 
struct

infixr 9 `
fun a ` b = a b

open LangCommon
structure S = SourceLang

val PPtuple = S.Ptuple
val PPvar = S.Pvar

datatype expr = E of (expr,var,unit) S.exprF

fun Evar x = E ` S.Fvar x
fun Eint x = E ` S.Fint x
fun Ebool x = E ` S.Fbool x
fun Elam x = E ` S.Flam x
fun Eapp x = E ` S.Fapp x
fun Etuple x = E ` S.Ftuple x
fun Epi x = E ` S.Fpi x
fun Einj x = E ` S.Finj x
fun Ecase x = E ` S.Fcase x 
fun Eif x = E ` S.Fif x
fun Elet x = E ` S.Flet x
fun Ebinop x = E ` S.Fbinop x
fun Eroll x = E ` S.Froll x
fun Eunroll x = E ` S.Funroll x
fun Eerror x = E ` S.Ferror x
		
end
