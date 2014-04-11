
structure Lambda12c = 
struct

open LangCommon

datatype ty		= Tstandard of ty Lambda12.typeF
				| Tfut of ty
				
datatype expr	= Estandard of (expr,string,ty) Lambda12.exprF
				| Enext of expr
				| Eprev of expr
				| Ehold of expr

				
val Tint = Tstandard Lambda12.TFint
val Tunit = Tstandard Lambda12.TFunit
val Tbool = Tstandard Lambda12.TFbool
val Tprod = Tstandard o Lambda12.TFprod
val Tsum = Tstandard o Lambda12.TFsum
val Tarr = Tstandard o Lambda12.TFarr

val Evar = Estandard o Lambda12.Fvar
val Eunit = Estandard Lambda12.Funit
val Eint = Estandard o Lambda12.Fint
val Ebool = Estandard o Lambda12.Fbool
val Etuple = Estandard o Lambda12.Ftuple
val Einj = Estandard o Lambda12.Finj
val Epi = Estandard o Lambda12.Fpi
val Eif = Estandard o Lambda12.Fif
val Ecase = Estandard o Lambda12.Fcase
val Elam = Estandard o Lambda12.Flam
val Eapp = Estandard o Lambda12.Fapp
val Elet = Estandard o Lambda12.Flet
val Eerror = Estandard o Lambda12.Ferror
val Ebinop = Estandard o Lambda12.Fbinop

end
