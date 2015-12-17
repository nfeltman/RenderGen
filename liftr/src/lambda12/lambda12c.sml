
structure Lambda12c = 
struct

local
open LangCommon
open TypesBase
structure S = SourceLang
in

datatype ty		= Tstandard of ty typeF
				| Tfut of ty
				| Tnow of ty
				| Tref of string

datatype stage	= ThisStage | NextStage | MonoStage
datatype patt	= P of (string,patt) S.pattern
				| Pmono of patt
				| Pnext of patt
datatype expr	= Estandard of (expr,string,patt * expr,ty) S.exprF
				| Enext of expr
				| Eprev of expr
				| Emono of expr
				| Ehold of expr
				| EpushPrim of expr
				| EpushSum of expr
				| Eletdecs of decl list * expr
and 	decl 	= Dval of patt * expr
				| Dfun of string * ty * (patt * expr)
				| Dty of string * ty
				| Ddata of string * (string * ty option) list
				| Drec of string * ty * ty * (patt * expr)
				| Dnext of decl list
				| Dmono of decl list

fun trim s = substring (s,1,size(s)-2)
				
val Tint = Tstandard (TFprim Prims.Tint)
val Tbool = Tstandard (TFprim Prims.Tbool)
val Tstr = Tstandard (TFprim Prims.Tstr)
val Tvar = Tstandard o TFvar
val Trec = Tstandard o TFrec
val Tprod = Tstandard o TFprod
fun Tsum (a,b) = Tstandard (TFsum [a,b])
val Tarr = Tstandard o TFarr

val Evar = Estandard o S.Fvar
val Eint = Estandard o S.SEdata o S.EprimVal o Prims.Vint
val Ebool = Estandard o S.SEdata o S.EprimVal o Prims.Vbool
val Estr = Estandard o S.SEdata o S.EprimVal o Prims.Vstr
val Etuple = Estandard o S.SEprod o S.Etuple
val Eroll = Estandard o S.Froll
val Eunroll = Estandard o S.Funroll
val Epi = Estandard o S.SEprod o S.Epi
val Eif = Estandard o S.SEdata o S.Eif
fun Ecase (e,bs) = Estandard (S.SEdata (S.Ecase (e,bs)))
val Elam = Estandard o S.Flam
val Eapp = Estandard o S.Fapp
val Elet = Estandard o S.Flet
val Eerror = Estandard o S.SEdata o S.Eerror
val Ebinop = Estandard o S.SEdata o S.Ebinop

val Punused = P S.Punused
val Ptuple = P o S.Ptuple
val Proll = P o S.Proll
val Pvar = P o S.Pvar
end
	
end
