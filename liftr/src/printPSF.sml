
structure PrintPSF = 
struct

open LangCommon
open LambdaPSF

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
	end

fun printType p = printTypeHelper p 2

fun printTermHelper (p : string -> unit) level e = 
	let
		val g = printTermHelper p
		fun prio nextLevel f = 
			if nextLevel > level 
			then (p "("; f (); p ")")
			else f ()
		val toString = Variable.toString
	in
		case e of
		  Evar v => p (toString v)
		| Elam (t, (v, e)) => prio 2 (fn () => (p "fn "; p (toString v); p " : "; printType p t; p " => "; g 2 e))
		| Eapp (e1, e2) => prio 1 (fn () => (g 1 e1; p " "; g 0 e2))
		| Etuple [] => p "()"
		| Etuple (e0::es) => (p "("; g 2 e0; app (fn e => (p ", "; g 2 e)) es; p ")")
		| Epi (i,e) => prio 1 (fn () => (p "#"; p (Int.toString (i+1)); p " "; g 0 e))
		| Einj (Left, t, e) => prio 1 (fn () => (p "inL ("; printType p t; p ")"; g 0 e))
		| Einj (Right, t, e) => prio 1 (fn () => (p "inR ("; printType p t; p ") "; g 0 e))
		| Ecase (e1,(v2,e2),(v3,e3)) => prio 2 (fn () => (p "case "; g 2 e1; p " of "; p (toString v2); p " => "; g 2 e2; p " | "; p (toString v3); p " => "; g 2 e3))
		| Elet (e1,(v,e2)) => prio 2 (fn () => (p "let "; p (toString v); p " = "; g 2 e1; p " in "; g 2 e2))
		| Ebinop (bo, e1, e2) => prio 1 (fn () => (g 2 e1; p " op "; g 2 e2))
		| Eroll e =>  prio 1 (fn () => (p "roll "; g 0 e))
		| Eunroll e =>  prio 1 (fn () => (p "unroll "; g 0 e))
		| Eerror _ => p "error"
	end
	
fun printTerm p = printTermHelper p 2

fun test () = (printType print (Tfunc (Tfunc (Tprod [],Tprod []),Tfunc (Tprod [],Tprod []))); print "\n")

(* Tprod of ty list
				| Tsum of ty * ty
				| Tfunc of ty * ty *)

end
