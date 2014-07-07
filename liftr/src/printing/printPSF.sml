
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
				
structure S = LambdaPSF
fun convertPSFPattern (S.PPvar x) = Pvar (Variable.toString x)
  | convertPSFPattern (S.PPtuple xs) = Ptuple (map convertPSFPattern xs)
fun convertPSF e = 
	let
		fun convertBranch (x,e) = (convertPSFPattern x, convertPSF e)
		val convert = convertPSF
	in
		case e of 
		  S.Evar v => Eatom (Variable.toString v)
		| S.Eint i => Eatom (Int.toString i)
		| S.Ebool b => Eatom (if b then "true" else "false")
		| S.Elam (_,b) => Elam (Eatom "_", convertBranch b)
		| S.Eapp (e1,e2) => Eapp (convert e1, convert e2)
		| S.Etuple es => Etuple (map convert es)
		| S.Epi (i, e) => EprimApp ("#"^(Int.toString (i+1)), convert e)
		| S.Einj (lr, _, e) => EprimApp (case lr of Left => "inL" | Right => "inR", convert e)
		| S.Ecase (e,b1,b2) => Ecase (convert e, convertBranch b1, convertBranch b2)
		| S.Eif (e1,e2,e3) => Eif (convert e1, convert e2, convert e3)
		| S.Elet (e, b) => Elet (convert e, convertBranch b)
		| S.Ebinop (bo, e1, e2) => Einfix (opToString bo, [convert e1, convert e2])
		| S.Eroll e => EprimApp ("roll", convert e)
		| S.Eunroll e => EprimApp ("unroll", convert e)
		| S.Eerror t => Eatom "error"
	end

structure S = TypesBase
fun convertSourceTypes convert ty = 
		case ty of
		  S.TFint => Eatom "int"
		| S.TFbool => Eatom "bool"
		| S.TFunit => Eatom "~unit~"
		| S.TFvar i => Eatom (Int.toString i) 
		| S.TFrec t => EprimApp ("mu", convert t)
		| S.TFprod [] => Eatom ("unit")
		| S.TFprod ts => Einfix ("*", map convert ts)
		| S.TFsum (t1, t2) => Einfix ("+", [convert t1, convert t2])
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
		| S.Funit => Etuple []
		| S.Flam (t,b) => Elam (convertTy t, convertBranch b)
		| S.Fapp (e1,e2) => Eapp (convert e1, convert e2)
		| S.Ftuple es => Etuple (map convert es)
		| S.Fpi (i, e) => EprimApp ("#" ^ (Int.toString (i+1)), convert e)
		| S.Finj (i, t, e) => Eapp (EprimApp(case i of Left => "inl" | Right => "inr", convertTy t), convert e)
		| S.Fcase (e1,b2,b3) => Ecase (convert e1, convertBranch b2, convertBranch b3)
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

(*
fun printTypeHelper (p : string -> unit) level ty = 
	let
		val g = printTypeHelper p
		fun prio nextLevel f = 
			if nextLevel > level 
			then (p "("; f (); p ")")
			else f ()
	in
		case ty of
		  Tint => p "int"
		| Tbool => p "bool"
		| Tprod [] => p "unit"
		| Tprod (t0::ts) => prio 1 (fn () => (g 0 t0; app (fn t => (p " * "; g 0 t)) ts))
		| Tsum (t1,t2) => prio 1 (fn () => (g 0 t1; p " + "; g 0 t2))
		| Tfunc (t1,t2) => prio 2 (fn () => (g 1 t1; p " -> "; g 2 t2))
		| Tvar i => p (Int.toString i)
		| Trec t => prio 1 (fn () => (p "rec."; g 2 t))
	end*)


end
