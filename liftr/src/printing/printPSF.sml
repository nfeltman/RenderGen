
structure PrintPSF = 
struct

open LangCommon
open Prims

datatype patt	= Pvar of string
				| Ptuple of patt list
datatype expr	= Eatom of string
				| Elam of expr * (patt * expr)
				| Eapp of expr * expr
				| Etuple of expr list
				| Ecase of expr * (patt * expr) * (patt * expr)
				| Eif of expr * expr * expr
				| Elet of expr * (patt * expr)
				| Ebinop of string * expr * expr
				| EprimApp of string * expr
				| EbraceApp of string * expr

fun opToString bo = (
		case bo of 
		  Iplus => "+"
		| Iminus => "-"
		| Itimes => "*"
		| Iless => "<"
		| Igreater => ">"
		| Iequal => "=="
		| Ilesseq => "<="
		| Igreatereq => ">="
		| Band => "and"
		| Bor=> "or")
				
structure S = LambdaPSF
fun convertPSF e = 
	let
		fun convertPattern (S.PPvar x) = Pvar (Variable.toString x)
		  | convertPattern (S.PPtuple xs) = Ptuple (map convertPattern xs)
		fun convertBranch (x,e) = (convertPattern x, convertPSF e)
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
		| S.Ebinop (bo, e1, e2) => Ebinop (opToString bo, convert e1, convert e2)
		| S.Eroll e => EprimApp ("roll", convert e)
		| S.Eunroll e => EprimApp ("unroll", convert e)
		| S.Eerror t => Eatom "error"
	end

structure S = TypesBase
fun convertSourceTypes convert ty = 
		case ty of
		  S.TFint => Eatom "int"
		| S.TFbool => Eatom "bool"
		| S.TFunit => Eatom "unit"
		| S.TFvar i => Eatom (Int.toString i) 
		| S.TFrec t => EprimApp ("mu", convert t)
		| S.TFprod (t1, t2) => Ebinop ("*", convert t1, convert t2)
		| S.TFsum (t1, t2) => Ebinop ("+", convert t1, convert t2)
		| S.TFarr (t1, t2) => Ebinop ("->", convert t1, convert t2)
		
structure S = SourceLang
fun convertSource convert convertTy ex = 
	let
		fun convertPattern (S.Pvar x) = Pvar (Variable.toString x)
		  | convertPattern (S.Ptuple (x1,x2)) = Ptuple [convertPattern x1, convertPattern x2]
		fun convertBranch (x,e) = (convertPattern x, convert e)
	in
		case ex of 
		  S.Fvar v => Eatom (Variable.toString v)
		| S.Fint i => Eatom (Int.toString i)
		| S.Fbool b => Eatom (if b then "true" else "false")
		| S.Funit => Etuple []
		| S.Flam (t,b) => Elam (convertTy t, convertBranch b)
		| S.Fapp (e1,e2) => Eapp (convert e1, convert e2)
		| S.Ftuple (e1,e2) => Etuple [convert e1, convert e2]
		| S.Fpi (i, e) => EprimApp (case i of Left => "#1" | Right => "#2", convert e)
		| S.Finj (i, t, e) => Eapp (EprimApp(case i of Left => "inl" | Right => "inr", convertTy t), convert e)
		| S.Fcase (e1,b2,b3) => Ecase (convert e1, convertBranch b2, convertBranch b3)
		| S.Fif (e1,e2,e3) => Eif (convert e1, convert e2, convert e3)
		| S.Flet (e, b) => Elet (convert e, convertBranch b)
		| S.Fbinop (bo, e1, e2) => Ebinop (opToString bo, convert e1, convert e2)
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

fun convertTerm ex = 
	let
		val L = PrettyPrinter.Pliteral
		fun S n e = PrettyPrinter.Psubterm (n, convertTerm e)
		val toString = Variable.toString
		fun cPatt (Pvar x) = x
		  | cPatt (Ptuple xs) = "("^(String.concatWith "," (map cPatt xs))^")"
	in
		case ex of
		  Eatom s => (0, [L s])
		| Elam (t, (v,e)) => (2, [L ("fn "^cPatt v^" : "), S 2 t, L(" => "),  S 2 e])
		| Eapp (e1, e2) => (1, [S 1 e1, L " ", S 0 e2])
		| Etuple [] => (0, [L "()"])
		| Etuple (e0::es) => (0, L "(" :: S 2 e0 :: foldr (fn (e,prev) => L ", " :: S 2 e :: prev) [L ")"] es)
		| Ecase (e1,(v2,e2),(v3,e3)) => (2, [L "case ", S 2 e1, L (" of "^cPatt v2^" => "), S 2 e2, L (" | "^cPatt v3^" => "), S 2 e3])
		| Eif (e1,e2,e3) => (2, [L "if ", S 2 e1, L " then ", S 2 e2, L " else ", S 2 e3])
		| Elet (e1,(v,e2)) => (2, [L ("let "^cPatt v^" = "), S 2 e1, L " in ", S 2 e2])
		| Ebinop (bo, e1, e2) => (1, [S 2 e1, L bo, S 2 e2])
		| EprimApp (f, e) => (1, [L (f^" "), S 0 e])
		| EbraceApp (f, e) => (1, [L (f^" "), L "{", S 2 e, L "}"])
	end
end
