
structure DiagonalSemantics = 
struct

local
open LangCommon
open Lambda12
open SourceLang
open ValuesBase
open Contexts

infixr 9 `
fun a ` b = a b

fun map1 f (a,b) = (f a, b)
fun map2 f (a,b) = (a, f b)
fun comp1 f (g, h) = (f o g, h)

in


(* first stage values *)				
datatype value1	= V1 of (value1,cont,pattern12,expr1) valueF
				| V1hat of var
				| V1mono of valueM
				
and 	 valueM = VM of (valueM,cont,patternM,exprM) valueF
				
withtype   cont = (var, (value1,valueM) Contexts.DoubleContext.doubleEntry) Contexts.context

(* second stage values/expressions *)
datatype expr	= E of (expr,var,patternM,unit) exprF
datatype value2	= V2 of (value2,(var, value2) context,patternM,expr) valueF

fun unV1 (V1 v) = v
  | unV1 _ = raise Stuck
fun unVM (VM v) = v
fun unhat (V1hat y) = y
  | unhat _ = raise Stuck
fun unmono (V1mono v) = v
  | unmono _ = raise Stuck
fun unV2 (V2 v) = v

structure ValuesM = EmbedValues (struct
	type v = valueM
	type c = cont
	type r = patternM
	type e = exprM
	fun outof (VM v) = v
	val into = VM
end)
structure EvaluatorM = Evaluator (ValuesM)
fun ext1 g (P p) t = foldPattern (DoubleContext.extendContext1, ext1, untuple o unV1, Stuck) g p t
  | ext1 g (Pmono p) t = ext2 g p (unmono t)
and ext2 g (PM p) t = foldPattern (DoubleContext.extendContext2, ext2, ValuesM.untuple, Stuck) g p t

fun eval1 env (E1 exp) = 
	let
		val (eval,V,unV) = (eval1 env, V1, unV1)
		fun evalBranch env (g,v) (x,e) = comp1 g ` eval1 (ext1 env x v) e 
	in
		case exp of 
		  Fvar v => (id, DoubleContext.lookup1 env v)
		| FprimVal pv => (id, V ` VFprim pv)
		| Flam (_, b) => (id, V ` VFlam (env,b))
		| Fapp (e1,e2) => 
			let 
				val (g1,(env,branch)) = map2 (unlam o unV) (eval e1)
			in
				comp1 g1 (evalBranch env (eval e2) branch)
			end
		| Ftuple es => map2 (V o VFtuple) (List.foldr (fn ((g,v),(gs,vs))=>(g o gs,v::vs)) (id,[]) (map eval es))
		| Fpi (i, e) => (case eval e of (g,v) => (g, List.nth(untuple ` unV v,i)))
		| Finj (ts, _, e) => map2 (fn x => V ` VFinj (length ts,x)) (eval e)
		| Fcase (e, bs) => 
			let
				val (g,(i,v)) = map2 (uninj o unV) (eval e)
			in
				evalBranch env (g,v) ` List.nth (bs,i)
			end
		| Fif (e1, e2, e3) => 
			let
				val (g,b) = map2 (Prims.unbool o unprimV o unV) (eval e1)
			in
				comp1 g ` eval (if b then e2 else e3)
			end
		| Flet (e,b) => evalBranch env (eval e) b
		| Fbinop (bo,e1,e2) => 
			let
				val (g1,v1) = eval e1
				val (g2,v2) = eval e2
			in
				(g1 o g2, V (VFprim (Prims.evalPrim (bo, unprimV (unV v1), unprimV (unV v2)))))
			end
		| Froll (_,e) => map2 (V o VFroll) (eval e)
		| Funroll e => map2 (unroll o unV) (eval e)
		| Ferror _ => raise Stuck
	end
  | eval1 env (E1next e) = 
		let
			val y = Variable.newvar "y" 
		in 
			(fn r => E ` Flet (trace2 env e,(PM (Pvar y),r)), V1hat y) 
		end
  | eval1 env (E1hold e) =
		let
			val (g,v) = eval1 env e
			val y = Variable.newvar "y" 
		in
			(fn r=> g ` E ` Flet (E ` FprimVal ` unprimV ` unV1 v, (PM (Pvar y), r)), V1hat y)
		end
  | eval1 env (E1mono e) = 
		let
			fun promoteType (T2 t) = T1 (TypesBase.mapType promoteType t)
			fun promotePattern (PM p) = P ` mapPattern promotePattern p
			fun promoteToE1 (EM e) = E1 (SourceLang.mapExpr promoteToE1 promoteType promotePattern e)
		in
			(id, V1mono ` evalM env e)
		end
  | eval1 env (E1pushPrim e) = map2 (V1 o VFprim o unprimV o unVM o unmono) (eval1 env e)
  | eval1 env (E1pushProd e) = map2 (V1 o VFtuple o (map V1mono) o untuple o unVM o unmono) (eval1 env e)
  | eval1 env (E1pushSum e) = 
		let 
			val (g,(i,v)) = map2 (uninj o unVM o unmono) (eval1 env e)
		in
			(g, V1 (VFinj (i, V1mono v)))
		end
  | eval1 env (E1pushArr e) = 
		let 
			val (g,(c,b)) = map2 (unlam o unVM o unmono) (eval1 env e)
			val y = Variable.newvar "y"
		in
			(g,
			V1 (VFlam (c, (Pmono (PM (Pvar y)), 
					E1mono ( EM (Flet (EM (Fvar y),b)))))))
		end
		
and evalM env (EM exp) = EvaluatorM.evalF env evalM (ext2, DoubleContext.lookup2) exp
and trace2 env (E2 exp) = E (mapExpr (trace2 env) (fn _ => ()) id exp)
  | trace2 env (E2prev e) = (op `) ` map2 (E o Fvar o unhat) ` eval1 env e
  
  
structure Values2 = EmbedValues (struct
	type v = value2
	type c = (var, value2) context
	type r = patternM
	type e = expr
	fun outof (V2 v) = v
	val into = V2
end)
fun ext g (PM p) t = foldPattern (extendContext, ext, Values2.untuple, Stuck) g p t

structure Evaluator2 = Evaluator (Values2)

fun eval2 env (E exp) = Evaluator2.evalF env eval2 (ext,lookup) exp
end

end
