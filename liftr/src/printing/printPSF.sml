
structure PrintPSF = 
struct

open LangCommon
open Prims
open PrettyPrinter

fun opToString bo = (
		case bo of 
		  Iplus => "+"
		| Iminus => "-"
		| Itimes => "*"
		| Idiv => "/"
		| Imod => "mod"
		| Iless => "<"
		| Igreater => ">"
		| Iequal => "=="
		| Ilesseq => "<="
		| Igreatereq => ">="
		| Band => "and"
		| Bor=> "or")

structure S = TypesBase
fun convertSourceTypes convert ty = 
		case ty of
		  S.TFint => Eatom "int"
		| S.TFbool => Eatom "bool"
		| S.TFvar i => Eatom (Int.toString i) 
		| S.TFrec t => EprimApp ("mu", convert t)
		| S.TFprod [] => Eatom ("unit")
		| S.TFprod ts => Einfix ("*", map convert ts)
		| S.TFsum ts => Einfix ("+", map convert ts)
		| S.TFarr (t1, t2) => Einfix ("->", [convert t1, convert t2])
		
structure S = SourceLang
fun convertSourcePattern (S.Pvar x) = Pvar (Variable.toString x)
  | convertSourcePattern (S.Ptuple xs) = Ptuple (map convertSourcePattern xs)
fun convertSource convert convertTy ex = 
	let
		fun convertBranch (x,e) = (convertSourcePattern x, convert e)
	in
		case ex of 
		  S.Fvar v => Eatom (Variable.toString v)
		| S.Fint i => Eatom (Int.toString i)
		| S.Fbool b => Eatom (if b then "true" else "false")
		| S.Flam (t,b) => Elam (convertTy t, convertBranch b)
		| S.Fapp (e1,e2) => Eapp (convert e1, convert e2)
		| S.Ftuple es => Etuple (map convert es)
		| S.Fpi (i, e) => EprimApp ("#" ^ (Int.toString (i+1)), convert e)
		| S.Finj (ts, us, e) => Eapp (EprimApp("inj", Einfix ("+", (map convertTy ts) @ (Eatom "#" :: map convertTy us))), convert e)
		| S.Fcase (e,bs) => Ecase (convert e, map convertBranch bs)
		| S.Fif (e1,e2,e3) => Eif (convert e1, convert e2, convert e3)
		| S.Flet (e, b) => Elet (convert e, convertBranch b)
		| S.Fbinop (bo, e1, e2) => Einfix (opToString bo, [convert e1, convert e2])
		| S.Froll (t,e) => Eapp (EprimApp ("roll", convertTy t), convert e)
		| S.Funroll e => EprimApp ("unroll", convert e)
		| S.Ferror t => EprimApp ("error", convertTy t)
	end
	
structure S = Lambda12
fun convertTyStage1 (S.T1 t) = convertSourceTypes convertTyStage1 t
  | convertTyStage1 (S.T1fut t) = EprimApp("$", convertTyStage2 t)
and convertTyStage2 (S.T2 t) = convertSourceTypes convertTyStage2 t
fun convertStage1 (S.E1 e) = convertSource convertStage1 convertTyStage1 e
  | convertStage1 (S.E1next e) = EbraceApp("next", convertStage2 e)
  | convertStage1 (S.E1hold e) = EprimApp("holdInt", convertStage1 e)
and convertStage2 (S.E2 e) = convertSource convertStage2 convertTyStage2 e
  | convertStage2 (S.E2prev e) = EbraceApp("prev", convertStage1 e)
  
fun convertDiag (DiagonalSemantics.E e) = convertSource convertDiag (fn _ => Eatom "_") e
fun convertPSF (LambdaPSF.E e) = convertSource convertPSF (fn () => Eatom "_") e


end
