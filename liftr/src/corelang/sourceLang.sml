
structure SourceLang = 
struct

open LangCommon
open Contexts
open TypesBase
open ValuesBase

datatype 'r pattern 		= Pvar of 'r
							| Ptuple of ('r pattern) list
							
datatype ('e,'r,'t) exprF	= Fvar of 'r
							| Funit
							| Fint of int
							| Fbool of bool
							| Flam of 't * ('r pattern * 'e)
							| Fapp of 'e * 'e
							| Ftuple of 'e list
							| Fpi of int * 'e
							| Finj of LR * 't * 'e
							| Fcase of 'e * ('r pattern * 'e) * ('r pattern * 'e)
							| Fif of 'e * 'e * 'e
							| Flet of 'e * ('r pattern * 'e)
							| Froll of 't * 'e
							| Funroll of 'e
							| Ferror of 't
							| Fbinop of Prims.binops * 'e * 'e
						
fun mapExpr fe ft exp =
	case exp of
	  Fvar v => Fvar v
	| Funit => Funit
	| Fint i => Fint i
	| Fbool b => Fbool b
	| Flam (t, (x,e)) => Flam (ft t, (x, fe e))
	| Fapp (e1,e2) => Fapp (fe e1, fe e2)
	| Ftuple es => Ftuple (map fe es)
	| Fpi (lr, e) => Fpi (lr, fe e)
	| Finj (lr, t, e) => Finj (lr, ft t, fe e)
	| Fcase (e1,(x2,e2),(x3,e3)) => Fcase (fe e1, (x2, fe e2), (x3, fe e3))
	| Fif (e1,e2,e3) => Fif (fe e1, fe e2, fe e3)
	| Flet (e1, (x,e2)) => Flet (fe e1, (x, fe e2))
	| Ferror (t) => Ferror (ft t)
	| Fbinop (bo,e1,e2) => Fbinop(bo, fe e1, fe e2)
	| Froll (t, e) => Froll (ft t, fe e)
	| Funroll e => Funroll (fe e)
	
fun replaceVars recRep G f exp =
	let
		val rep = recRep G
		fun forPattern g (Pvar x) = let val y = f x in (Pvar y,extendContext g x y) end
		  | forPattern g (Ptuple xs) =
				let 
					fun f (x,(ys,g2)) = 
						let val (y,g3) = forPattern g2 x in (y::ys,g3) end
					val (ys, g2) = foldr f ([],g) xs
				in 
					(Ptuple ys, g2)
				end
		fun forBranch (x,e) = let val (y,g) = forPattern G x in (y, recRep g e) end
	in
		case exp of
		  Fvar v => Fvar (lookup G v)
		| Funit => Funit
		| Fint i => Fint i
		| Fbool b => Fbool b
		| Flam (t, b) => Flam (t, forBranch b)
		| Fapp (e1,e2) => Fapp (rep e1, rep e2)
		| Ftuple es => Ftuple (map rep es)
		| Fpi (lr, e) => Fpi (lr, rep e)
		| Finj (lr, t, e) => Finj (lr, t, rep e)
		| Fcase (e1,b2,b3) => Fcase (rep e1, forBranch b2, forBranch b3)
		| Fif (e1,e2,e3) => Fif (rep e1, rep e2, rep e3)
		| Flet (e1, b) => Flet (rep e1, forBranch b)
		| Ferror (t) => Ferror (t)
		| Fbinop (bo,e1,e2) => Fbinop(bo, rep e1, rep e2)
		| Froll (t, e) => Froll (t, rep e)
		| Funroll e => Funroll (rep e)
	end

fun forPattern (f,unpack,_) g (Pvar x) t = f g x t
  | forPattern (fu as (f,unpack,_)) g (Ptuple xs) t = forPattList fu g xs (unpack t)
and forPattList fu g [] [] = g
  | forPattList fu g (x::xs) (t::ts) = forPattern fu (forPattList fu g xs ts) x t
  | forPattList (_,_,ex) _ _ _ = raise ex
	
fun typeCheck gamma checkrec (extendC,lookupC) Twrap Tunwrap teq subst primTypes exp = 
	let
		val check = checkrec gamma
		fun checkbranch t (patt,e) = checkrec (forPattern (extendC, unprod o Tunwrap,TypeError) gamma patt t) e
		fun checkFun eq ((a,b),c) = if eq a c then b else raise TypeError
		fun binSame eq (a,b) (c,d,e) = if (eq a c) andalso (eq b d) then e else raise TypeError
		fun selfSubst t = subst 0 (Twrap (TFrec t)) t
	in
		case exp of 
		  Fvar v => lookupC gamma v
		| Flam (t,b) => Twrap (TFarr (t, checkbranch t b))
		| Fapp (e1,e2) => checkFun teq (unarr (Tunwrap (check e1)), check e2)
		| Funit => Twrap TFunit
		| Fint _ => Twrap TFint
		| Fbool _ => Twrap TFbool
		| Ftuple es => Twrap (TFprod (map check es))
		| Fpi (i, e) => List.nth(unprod (Tunwrap (check e)), i)
		| Finj (lr, t, e) => Twrap (TFsum (injLR lr (check e) t))
		| Fcase (e1,b1,b2) => assertSame teq (zip2 checkbranch (unsum (Tunwrap (check e1))) (b1,b2)) 
		| Fif (e1,e2,e3) => (TypesBase.unbool (Tunwrap (check e1)); assertSame teq (check e2, check e3))
		| Flet (e,b) => checkbranch (check e) b
		| Ferror t => t
		| Froll (t, e) => if teq (selfSubst t) (check e) then Twrap (TFrec t) else raise TypeError
		| Funroll e => selfSubst (unrec (Tunwrap (check e)))
		| Fbinop (bo, e1, e2) => binSame teq (check e1, check e2) (primTypes bo)
	end
  
	
fun convertPrim (VFint i) = Prims.PrimEval.Vint i
  | convertPrim (VFbool b) = Prims.PrimEval.Vbool b
  | convertPrim _ = raise Stuck
fun unconvertPrim (Prims.PrimEval.Vint i) = VFint i
  | unconvertPrim (Prims.PrimEval.Vbool b) = VFbool b

fun evalF env evalRec (extendC,lookupC) Vwrap Vunwrap exp = 
	let
		val eval = evalRec env
		fun evalBranchE value (env,(patt,e)) = evalRec (forPattern (extendC, untuple o Vunwrap, Stuck) env patt value) e
		fun evalBranch v b = evalBranchE v (env,b)
		val convertP = convertPrim o Vunwrap
	in
		case exp of 
		  Fvar v => lookupC env v
		| Flam (t, b) => Vwrap (VFlam (env,b))
		| Fapp (e1, e2) => evalBranchE (eval e2) (unlam (Vunwrap (eval e1)))
		| Funit => Vwrap VFunit
		| Fint i => Vwrap (VFint i)
		| Fbool b => Vwrap (VFbool b)
		| Ftuple es => Vwrap (VFtuple (map eval es))
		| Fpi (i, e) => List.nth (untuple (Vunwrap (eval e)), i)
		| Finj (side, _, e) => Vwrap (VFinj (side, eval e))
		| Fcase (e1, b1, b2) => (case uninj (Vunwrap (eval e1)) of (Left, v) => evalBranch v b1 | (Right, v) => evalBranch v b2)
		| Fif (e1, e2, e3) => eval (if unbool (Vunwrap (eval e1)) then e2 else e3)
		| Flet (e, b) => evalBranch (eval e) b
		| Fbinop (bo,e1,e2) => Vwrap (unconvertPrim (Prims.PrimEval.evalPrim (bo, convertP (eval e1), convertP (eval e2))))
		| Froll (_, e) => Vwrap (VFroll (eval e))
		| Funroll e => unroll (Vunwrap (eval e))
		| Ferror t => raise Stuck
	end

end
