
structure Lambda12c = 
struct

local
open LangCommon
structure S = SourceLang
in

datatype ty		= Tstandard of ty S.typeF
				| Tfut of ty

type patt		= string S.pattern
datatype expr	= Estandard of (expr,string,ty) S.exprF
				| Enext of expr
				| Eprev of expr
				| Ehold of expr

				
val Tint = Tstandard S.TFint
val Tunit = Tstandard S.TFunit
val Tbool = Tstandard S.TFbool
val Tvar = Tstandard o S.TFvar
val Trec = Tstandard o S.TFrec
val Tprod = Tstandard o S.TFprod
val Tsum = Tstandard o S.TFsum
val Tarr = Tstandard o S.TFarr

val Evar = Estandard o S.Fvar
val Eunit = Estandard S.Funit
val Eint = Estandard o S.Fint
val Ebool = Estandard o S.Fbool
val Etuple = Estandard o S.Ftuple
val Einj = Estandard o S.Finj
val Eroll = Estandard o S.Froll
val Eunroll = Estandard o S.Funroll
val Epi = Estandard o S.Fpi
val Eif = Estandard o S.Fif
val Ecase = Estandard o S.Fcase
val Elam = Estandard o S.Flam
val Eapp = Estandard o S.Fapp
val Elet = Estandard o S.Flet
val Eerror = Estandard o S.Ferror
val Ebinop = Estandard o S.Fbinop

val Ptuple = S.Ptuple
val Pvar = S.Pvar

fun bind v e1 e2 = Elet (e1,(Pvar v,e2))
(* add lifts maybe *)
fun Eletr (f,t1,t2,b,e) = 
	let
		val tY = Tarr(Tvar 0,Tarr(t1,t2))
	in
	bind f 
		(bind "r" 
			(Elam(Trec tY, (Pvar "y", 
				bind f
					(Elam (t1,(Pvar "v", Eapp (Eapp (Eunroll (Evar "y"), Evar "y"), Evar "v"))))
					(Elam (t1,b))
			)))
			(Eapp(Evar "r", Eroll(tY, Evar "r")))
		)
		e
	end
end
	
end
