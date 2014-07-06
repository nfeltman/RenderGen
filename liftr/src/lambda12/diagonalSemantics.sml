
structure DiagonalSemantics = 
struct

local
open LangCommon
open Lambda12
open SourceLang
structure P = Prims.PrimEval

infixr 9 `
fun a ` b = a b

fun map1 f (a,b) = (f a, b)
fun map2 f (a,b) = (a, f b)
in

(* first stage values *)				
datatype value1	= V1 of (value1,(var, value1) context,var pattern,expr1) valueF
				| V1hat of var

(* second stage values/expressions *)
datatype expr	= E of (expr,var,unit) exprF
datatype value2	= V2 of (value2,(var, value2) context,var pattern,expr) valueF

fun unV1 (V1 v) = v
  | unV1 _ = raise Stuck
fun unhat (V1hat y) = y
  | unhat _ = raise Stuck
fun unV2 (V2 v) = v

fun eval1 env (E1 exp) = 
	let
		fun comp1 f (g, h) = (f o g, h)
		val (eval,V,unV) = (eval1 env, V1, unV1)
		fun evalBranch env (g,v) (x,e) = 
			comp1 g ` eval1 (forPattern (extendContext, untuple o unV1,Stuck) env x v) e 
	in
		case exp of 
		  Fvar v => (id, lookup env v)
		| Funit => (id, V VFunit)
		| Fint i => (id, V ` VFint i)
		| Fbool b => (id, V ` VFbool b)
		| Flam (_, b) => (id, V ` VFlam (env,b))
		| Fapp (e1,e2) => 
			let 
				val (g1,(env,branch)) = map2 (unlam o unV) (eval e1)
			in
				comp1 g1 (evalBranch env (eval e2) branch)
			end
		| Ftuple es => map2 (V o VFtuple) (List.foldr (fn ((g,v),(gs,vs))=>(g o gs,v::vs)) (id,[]) (map eval es))
		| Fpi (i, e) => (case eval e of (g,v) => (g, List.nth(untuple ` unV v,i)))
		| Finj (side, _, e) => map2 (fn x => V ` VFinj (side,x)) (eval e)
		| Fcase (e, b1, b2) => 
			let
				val (g,(side,v)) = map2 (uninj o unV) (eval e)
			in
				case side of
				  Left  => evalBranch env (g,v) b1
				| Right => evalBranch env (g,v) b2
			end
		| Fif (e1, e2, e3) => 
			let
				val (g,b) = map2 (unbool o unV) (eval e1)
			in
				comp1 g ` eval (if b then e2 else e3)
			end
		| Flet (e1,(x,e2)) => evalBranch env (eval e1) (x,e2)
		| Fbinop (bo,e1,e2) => 
			let
				val (g1,v1) = eval e1
				val (g2,v2) = eval e2
				val convertP = convertPrim o unV
			in
				(g1 o g2, V (unconvertPrim (P.evalPrim (bo, convertP v1, convertP v2))))
			end
		| Froll (_,e) => map2 (V o VFroll) (eval e)
		| Funroll e => map2 (unroll o unV) (eval e)
		| Ferror _ => raise Stuck
	end
  | eval1 env (E1next e) = 
		let
			val y = Variable.newvar "y" 
		in 
			(fn r => E ` Flet (trace2 env e,(Pvar y,r)), V1hat y) 
		end
  | eval1 env (E1hold e) =
		let
			val (g,v) = eval1 env e
			val y = Variable.newvar "y" 
		in
			(fn r=> g ` E ` Flet (E ` Fint ` unint ` unV1 v, (Pvar y, r)), V1hat y)
		end
	
and trace2 env (E2 exp) = E (mapExpr (trace2 env) (fn _ => ()) exp)
  | trace2 env (E2prev e) = (op `) ` map2 (E o Fvar o unhat) ` eval1 env e

fun eval2 env (E exp) = evalF env eval2 (extendContext,lookup) V2 unV2 exp
end

end
