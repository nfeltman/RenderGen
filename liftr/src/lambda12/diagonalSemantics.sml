
structure DiagonalSemantics = 
struct

local
open LangCommon
open Lambda12
open SourceLang
open ValuesBase

infixr 9 `
fun a ` b = a b

fun map1 f (a,b) = (f a, b)
fun map2 f (a,b) = (a, f b)
fun comp1 f (g, h) = (f o g, h)

in

(* second stage values/expressions *)
datatype expr	= E of (expr,var,pattern12 * expr,unit) exprF
datatype valueMono = VM of (valueMono,valueMono) valueF

(* first stage values *)
datatype value1	= V1 of (value1, (expr -> expr) * value1) valueF
				| V1hat of var
				| V1mono of valueMono

structure FirstStageValues = DoubleContext (MainDict) (type t1 = value1) (type t2 = valueMono) 
structure Contexts1 = ProjectDoubleContext (FirstStageValues)

fun unV1 (V1 v) = v
  | unV1 _ = raise Stuck
fun unVM (VM v) = v
fun unhat (V1hat y) = y
  | unhat _ = raise Stuck
fun unmono (V1mono v) = v
  | unmono _ = raise Stuck

structure ValuesM = EmbedValues (struct
	type v = valueMono
	type f = valueMono
	fun outof (VM v) = v
	val into = VM
end)
structure EvaluatorM = Evaluator (type t = valueMono) (ValuesM)

fun ext1helper (gNow,gNext) x t = (Contexts1.C1.extend gNow x t, gNext)
fun ext1 g (P p) t = foldPattern (ext1helper, ext1, untuple o unV1, Stuck) g p t
  | ext1 (gNow,gNext) (Pmono p) t = (ext2 gNow p (unmono t), gNext)
  | ext1 (gNow,gNext) (Pnext p) t = (gNow, fn r => gNext ` E ` Flet (E ` Fvar (unhat t),(p,r)))
and ext2 g (P p) t = foldPattern (Contexts1.C2.extend, ext2, ValuesM.untuple, Stuck) g p t
  | ext2 g _ _ = raise Stuck

fun evalM env (L12core exp) = EvaluatorM.evalF env evalM (ext2, Contexts1.C2.lookup) exp
  | evalM _ (L12stage _) = raise Stuck

fun eval1 env (L12core exp) = 
	let
		val (eval,V,unV) = (eval1 env, V1, unV1)
		fun evalBranchClean v (x,e) = 
				let 
					val (env2, gPatt) = ext1 (env,id) x v
				in
					comp1 gPatt ` eval1 env2 e
				end
		fun evalBranch env (gArg,v) (x,e) = 
				let 
					val (env, gPatt) = ext1 (env,id) x v
				in
					comp1 (gArg o gPatt) ` eval1 env e
				end
	in
		case exp of 
		  Fvar v => (id, Contexts1.C1.lookup env v)
		| SEdata (DataFrag.EprimVal pv) => (id, V ` VFprim pv)
		| Flam (_, b) => (id, V ` VFlam (fn v => evalBranchClean v b))
		| Fapp (e1,e2) => 
			let 
				val (g1,f) = map2 (unlam o unV) (eval e1)
				val (g2,v) = eval e2
			in
				comp1 (g1 o g2) (f v)
			end
		| SEprod (BranchlessFrag.Etuple es) => map2 (V o VFtuple) (List.foldr (fn ((g,v),(gs,vs))=>(g o gs,v::vs)) (id,[]) (map eval es))
		| SEprod (BranchlessFrag.Epi (i, e)) => (case eval e of (g,v) => (g, List.nth(untuple ` unV v,i)))
		| SEdata (DataFrag.Einj (ts, _, e)) => map2 (fn x => V ` VFinj (length ts,x)) (eval e)
		| SEdata (DataFrag.Ecase (e, bs)) => 
			let
				val (g,(i,v)) = map2 (uninj o unV) (eval e)
			in
				evalBranch env (g,v) ` List.nth (bs,i)
			end
		| SEdata (DataFrag.Eif (e1, e2, e3)) => 
			let
				val (g,b) = map2 (Prims.unbool o unprimV o unV) (eval e1)
			in
				comp1 g ` eval (if b then e2 else e3)
			end
		| Flet (e,b) => evalBranch env (eval e) b
		| SEdata (DataFrag.Ebinop (bo,e1,e2)) => 
			let
				val (g1,v1) = eval e1
				val (g2,v2) = eval e2
			in
				(g1 o g2, V (VFprim (Prims.evalPrim (bo, unprimV (unV v1), unprimV (unV v2)))))
			end
		| Froll (_,e) => map2 (V o VFroll) (eval e)
		| Ffix (_,_,b) => 
			let 
				fun f v = evalBranchClean (V ` VFtuple [V ` VFlam f, v]) b 
			in 
				(id, V ` VFlam f)
			end
		| Funroll e => map2 (unroll o unV) (eval e)
		| SEdata (DataFrag.Eerror _) => raise Stuck
	end
  | eval1 env (L12stage expr) = 
  	case expr of
	  E1next e => 
		let
			val y = Variable.newvar "y" 
		in 
			(fn r => E ` Flet (trace2 env e,(P (Pvar y),r)), V1hat y) 
		end
	| E1hold e =>
		let
			val (g,v) = eval1 env e
			val y = Variable.newvar "y" 
		in
			(fn r=> g ` E ` Flet (E ` SEdata ` DataFrag.EprimVal ` unprimV ` unVM ` unmono v, (P (Pvar y), r)), V1hat y)
		end
	| E1mono e => (id, V1mono ` evalM env e)
	| E1pushPrim e => map2 (V1 o VFprim o unprimV o unVM o unmono) (eval1 env e)
	| E1pushSum e => 
		let 
			val (g,(i,v)) = map2 (uninj o unVM o unmono) (eval1 env e)
		in
			(g, V1 (VFinj (i, V1mono v)))
		end
	| E2prev _ => raise Stuck
		
and trace2 env (L12core exp) = E (mapExpr (trace2 env) (fn _ => ()) id exp)
  | trace2 env (L12stage (E2prev e)) = (op `) ` map2 (E o Fvar o unhat) ` eval1 env e
  | trace2 env (L12stage _) = raise Stuck
  
structure Context2 = BasicContext (MainDict) (type t = valueMono) 

fun ext g (P p) t = foldPattern (Context2.extend, ext, ValuesM.untuple, Stuck) g p t
  | ext _ _ _ = raise Stuck

structure Evaluator2 = Evaluator (type t = valueMono) (ValuesM)

fun eval2 env (E exp) = Evaluator2.evalF env eval2 (ext,Context2.lookup) exp
end

end
