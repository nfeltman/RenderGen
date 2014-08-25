
structure Lambda12c = 
struct

local
open LangCommon
structure S = SourceLang
in

datatype ty		= Tstandard of ty S.typeF
				| Tfut of ty
				| Tref of string

datatype stage	= ThisStage | NextStage
type patt		= string S.pattern
datatype expr	= Estandard of (expr,string,ty) S.exprF
				| Enext of expr
				| Eprev of expr
				| Ehold of expr
				| Eletty of stage * string * ty * expr
				| Eletdata of stage * string * (string * ty option) list * expr
				| Eletr of string * ty * ty * (patt * expr) * expr

				
val Tint = Tstandard S.TFint
val Tbool = Tstandard S.TFbool
val Tvar = Tstandard o S.TFvar
val Trec = Tstandard o S.TFrec
val Tprod = Tstandard o S.TFprod
fun Tsum (a,b) = Tstandard (S.TFsum [a,b])
val Tarr = Tstandard o S.TFarr

val Evar = Estandard o S.Fvar
val Eint = Estandard o S.Fint
val Ebool = Estandard o S.Fbool
val Etuple = Estandard o S.Ftuple
fun Einjl (t, e) = Estandard (S.Finj ([],[t],e))
fun Einjr (t, e) = Estandard (S.Finj ([t],[],e))
val Eroll = Estandard o S.Froll
val Eunroll = Estandard o S.Funroll
val Epi = Estandard o S.Fpi
val Eif = Estandard o S.Fif
fun Ecase (e,bs) = Estandard (S.Fcase (e,bs))
val Elam = Estandard o S.Flam
val Eapp = Estandard o S.Fapp
val Elet = Estandard o S.Flet
val Eerror = Estandard o S.Ferror
val Ebinop = Estandard o S.Fbinop

val Ptuple = S.Ptuple
val Pvar = S.Pvar
end
	
end
