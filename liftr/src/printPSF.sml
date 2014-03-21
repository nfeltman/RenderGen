
structure PrintPSF = 
struct

open LangCommon
open Prims

datatype expr	= Eatom of string
				| Elam of (string * expr)
				| Eapp of expr * expr
				| Etuple of expr list
				| Ecase of expr * (string * expr) * (string * expr)
				| Eif of expr * expr * expr
				| Elet of expr * (string * expr)
				| Ebinop of Prims.binops * expr * expr
				| EprimApp of string * expr
				| EbraceApp of string * expr

structure S = LambdaPSF
fun convertPSF e = 
	let
		fun convertBranch (x,e) = (Variable.toString x, convertPSF e)
		val convert = convertPSF
	in
		case e of 
		  S.Evar v => Eatom (Variable.toString v)
		| S.Eint i => Eatom (Int.toString i)
		| S.Ebool b => Eatom (if b then "true" else "false")
		| S.Elam (_,b) => Elam (convertBranch b)
		| S.Eapp (e1,e2) => Eapp (convert e1, convert e2)
		| S.Etuple es => Etuple (map convert es)
		| S.Epi (i, e) => EprimApp ("#"^(Int.toString (i+1)), convert e)
		| S.Einj (lr, _, e) => EprimApp (case lr of Left => "inL" | Right => "inR", convert e)
		| S.Ecase (e,b1,b2) => Ecase (convert e, convertBranch b1, convertBranch b2)
		| S.Eif (e1,e2,e3) => Eif (convert e1, convert e2, convert e3)
		| S.Elet (e, b) => Elet (convert e, convertBranch b)
		| S.Ebinop (bo, e1, e2) => Ebinop (bo, convert e1, convert e2)
		| S.Eroll e => EprimApp ("roll", convert e)
		| S.Eunroll e => EprimApp ("unroll", convert e)
		| S.Eerror t => Eatom "error"
	end

structure S = SourceLang
fun convertSource convert ex = 
	let
		fun convertBranch (x,e) = (Variable.toString x, convert e)
	in
		case ex of 
		  S.Fvar v => Eatom (Variable.toString v)
		| S.Fint i => Eatom (Int.toString i)
		| S.Fbool b => Eatom (if b then "true" else "false")
		| S.Funit => Etuple []
		| S.Flam (_,b) => Elam (convertBranch b)
		| S.Fapp (e1,e2) => Eapp (convert e1, convert e2)
		| S.Ftuple (e1,e2) => Etuple [convert e1, convert e2]
		| S.Fpi (i, e) => EprimApp (case i of Left => "#1" | Right => "#2", convert e)
		| S.Finj (i, _, e) => EprimApp (case i of Left => "inl" | Right => "inr", convert e)
		| S.Fcase (e1,b2,b3) => Ecase (convert e1, convertBranch b2, convertBranch b3)
		| S.Fif (e1,e2,e3) => Eif (convert e1, convert e2, convert e3)
		| S.Flet (e, b) => Elet (convert e, convertBranch b)
		| S.Fbinop (bo, e1, e2) => Ebinop (bo, convert e1, convert e2)
		| S.Ferror _ => Eatom "error"
	end
	
structure S = Lambda12
fun convertStage1 (S.E1 e) = convertSource convertStage1 e
  | convertStage1 (S.E1next e) = EbraceApp("next", convertStage2 e)
  | convertStage1 (S.E1hold e) = EprimApp("holdInt", convertStage1 e)
and convertStage2 (S.E2 e) = convertSource convertStage2 e
  | convertStage2 (S.E2prev e) = EbraceApp("next", convertStage1 e)

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
	in
		case ex of
		  Eatom s => (0, [L s])
		| Elam (v, e) => (2, [L ("fn "^v^" => "),  S 2 e])
		| Eapp (e1, e2) => (1, [S 1 e1, L " ", S 0 e2])
		| Etuple [] => (0, [L "()"])
		| Etuple (e0::es) => (0, L "(" :: S 2 e0 :: foldr (fn (e,prev) => L ", " :: S 2 e :: prev) [L ")"] es)
		| Ecase (e1,(v2,e2),(v3,e3)) => (2, [L "case ", S 2 e1, L (" of "^v2^" => "), S 2 e2, L (" | "^v3^" => "), S 2 e3])
		| Eif (e1,e2,e3) => (2, [L "if ", S 2 e1, L " then ", S 2 e2, L " else ", S 2 e3])
		| Elet (e1,(v,e2)) => (2, [L ("let "^v^" = "), S 2 e1, L " in ", S 2 e2])
		| Ebinop (bo, e1, e2) => (1, [S 2 e1, L (case bo of Iplus => "+"| Iminus => "-"| Itimes => "*"| Iless => "<"| Igreater => ">"| Iequal => "=="| Ilesseq => "<="| Igreatereq => ">="| Band => "and"| Bor=> "or"), S 2 e2])
		| EprimApp (f, e) => (1, [L (f^" "), S 0 e])
		| EbraceApp (f, e) => (1, [L (f^" "), L "{", S 2 e, L "}"])
	end
end
