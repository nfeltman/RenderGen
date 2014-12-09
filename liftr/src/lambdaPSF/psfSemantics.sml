
structure PSFSemantics = 
struct

open LangCommon
open Contexts
open LambdaPSF
structure S = SourceLang

datatype value	= V of (value,(var, value) context,pattern,expr) ValuesBase.valueF
				| Vdummy

structure Values = EmbedValues 
(struct
	type v = value
	type c = (var, value) context
	type r = pattern
	type e = expr
	fun outof (V v) = v | outof Vdummy = raise Stuck
	val into = V
end)

structure EvaluatorPSF = S.Evaluator (Values)

fun forPattern g (P (S.Pvar x)) t = extendContext g x t
  | forPattern g (P (S.Ptuple xs)) (V (ValuesBase.VFtuple ts)) = forPattList g xs ts
  | forPattern g (P (S.Ptuple xs)) (V _) = raise Stuck
  | forPattern g (P (S.Ptuple xs)) Vdummy = forPattList g xs (map (fn _ => Vdummy) xs)
and forPattList g [] [] = g
  | forPattList g (x::xs) (t::ts) = forPattern (forPattList g xs ts) x t
  | forPattList _ _ _ = raise Stuck

fun evaluate env (E (S.Fpi (i,e))) =(
		case evaluate env e of
		  Vdummy => Vdummy
		| V (ValuesBase.VFtuple ts) => List.nth (ts,i)
		| V _ => raise Stuck)
  | evaluate env (E (S.Funroll e)) =(
		case evaluate env e of
		  Vdummy => Vdummy
		| V (ValuesBase.VFroll v) => v
		| V _ => raise Stuck)
  | evaluate env (E (S.Fapp (e1,e2))) =(
		case (evaluate env e1, evaluate env e2) of
		  (Vdummy, _) => Vdummy
		| (V (ValuesBase.VFlam (env,(patt,e))), v) => 
			evaluate (forPattern env patt v) e
		| (_,_) => raise Stuck)
  | evaluate env (E exp) = EvaluatorPSF.evalF env evaluate (forPattern,lookup) exp
  | evaluate env Edummy = Vdummy

fun extendPattern g p v = forPattern g p v

end
