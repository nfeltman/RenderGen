

signature SourceTypes = 
sig
	datatype 't typeF	= TFint
						| TFbool
						| TFunit
						| TFprod of 't * 't
						| TFsum of 't * 't
						| TFarr of 't * 't

	val unint  : 't typeF -> unit
	val unbool : 't typeF -> unit
	val ununit : 't typeF -> unit
	val unprod : 't typeF -> 't * 't
	val unarr  : 't typeF -> 't * 't
	val unsum  : 't typeF -> 't * 't
end

structure TypesBase : SourceTypes = 
struct
	open LangCommon
	datatype 't typeF	= TFint
						| TFbool
						| TFunit
						| TFprod of 't * 't
						| TFsum of 't * 't
						| TFarr of 't * 't

	fun unint TFint = ()
	  | unint _ = raise TypeError
	fun unbool TFbool = ()
	  | unbool _ = raise TypeError
	fun ununit TFint = ()
	  | ununit _ = raise TypeError
	fun unprod (TFprod ab) = ab
	  | unprod _ = raise TypeError
	fun unarr (TFarr v) = v
	  | unarr _ = raise TypeError
	fun unsum (TFsum v) = v
	  | unsum _ = raise TypeError
end

structure ValuesBase = 
struct
	open LangCommon
	datatype ('v,'r,'e) valueF	= VFint of int
								| VFbool of bool
								| VFunit
								| VFtuple of 'v * 'v
								| VFinj of LR * 'v
								| VFlam of 'r * 'e

	fun untuple (VFtuple v) = v
	  | untuple _ = raise Stuck
	fun uninj (VFinj v) = v
	  | uninj _ = raise Stuck
	fun unbool (VFbool b) = b
	  | unbool _ = raise Stuck
	fun unint (VFint i) = i
	  | unint _ = raise Stuck
	fun unlam (VFlam e) = e
	  | unlam _ = raise Stuck
end

structure SourceLang = 
struct

open LangCommon
open TypesBase
open ValuesBase

datatype ('e,'r,'t) exprF	= Fvar of 'r
							| Funit
							| Fint of int
							| Fbool of bool
							| Flam of 't * ('r * 'e)
							| Fapp of 'e * 'e
							| Ftuple of 'e * 'e
							| Fpi of LR * 'e
							| Finj of LR * 't * 'e
							| Fcase of 'e * ('r * 'e) * ('r * 'e)
							| Fif of 'e * 'e * 'e
							| Flet of 'e * ('r * 'e)
							| Ferror of 't
							| Fbinop of Prims.binops * 'e * 'e


fun mapType _ TFint = TFint
  | mapType _ TFbool = TFbool
  | mapType _ TFunit = TFunit
  | mapType f (TFsum (t1,t2)) = TFsum (f t1, f t2)
  | mapType f (TFprod (t1,t2)) = TFprod (f t1, f t2)
  | mapType f (TFarr (t1,t2)) = TFarr (f t1, f t2)
						
fun mapExpr fe ft exp =
	case exp of
	  Fvar v => Fvar v
	| Funit => Funit
	| Fint i => Fint i
	| Fbool b => Fbool b
	| Flam (t, (x,e)) => Flam (ft t, (x, fe e))
	| Fapp (e1,e2) => Fapp (fe e1, fe e2)
	| Ftuple (e1,e2) => Ftuple (fe e1, fe e2)
	| Fpi (lr, e) => Fpi (lr, fe e)
	| Finj (lr, t, e) => Finj (lr, ft t, fe e)
	| Fcase (e1,(x2,e2),(x3,e3)) => Fcase (fe e1, (x2, fe e2), (x3, fe e3))
	| Fif (e1,e2,e3) => Fif (fe e1, fe e2, fe e3)
	| Flet (e1, (x,e2)) => Flet (fe e1, (x, fe e2))
	| Ferror (t) => Ferror (ft t)
	| Fbinop (bo,e1,e2) => Fbinop(bo, fe e1, fe e2)
	
fun replaceVars recRep G f exp =
	let
		val rep = recRep G
		fun forBranch (x,e) = let val x2 = f x in (x2,recRep (extendContext G x x2) e) end
	in
		case exp of
		  Fvar v => Fvar (lookup G v)
		| Funit => Funit
		| Fint i => Fint i
		| Fbool b => Fbool b
		| Flam (t, b) => Flam (t, forBranch b)
		| Fapp (e1,e2) => Fapp (rep e1, rep e2)
		| Ftuple (e1,e2) => Ftuple (rep e1, rep e2)
		| Fpi (lr, e) => Fpi (lr, rep e)
		| Finj (lr, t, e) => Finj (lr, t, rep e)
		| Fcase (e1,b2,b3) => Fcase (rep e1, forBranch b2, forBranch b3)
		| Fif (e1,e2,e3) => Fif (rep e1, rep e2, rep e3)
		| Flet (e1, b) => Flet (rep e1, forBranch b)
		| Ferror (t) => Ferror (t)
		| Fbinop (bo,e1,e2) => Fbinop(bo, rep e1, rep e2)
	end
  
	
fun convertPrim (VFint i) = Prims.PrimEval.Vint i
  | convertPrim (VFbool b) = Prims.PrimEval.Vbool b
  | convertPrim _ = raise Stuck
fun unconvertPrim (Prims.PrimEval.Vint i) = VFint i
  | unconvertPrim (Prims.PrimEval.Vbool b) = VFbool b
  
fun teq _ TFint TFint = true
  | teq _ TFbool TFbool = true
  | teq _ TFunit TFunit = true
  | teq eq (TFprod (t1,t2)) (TFprod (u1,u2)) = (eq t1 u1) andalso (eq t2 u2)
  | teq _ _ _ = false


fun typeCheck gamma checkrec (extendC,lookupC) Twrap Tunwrap teq primTypes exp = 
	let
		val check = checkrec gamma
		fun checkbranch t (v,e) = checkrec (extendC gamma v t) e
		fun checkFun eq ((a,b),c) = if eq a c then b else raise TypeError
		fun binSame eq (a,b) (c,d,e) = if (eq a c) andalso (eq b d) then e else raise TypeError
	in
		case exp of 
		  Fvar v => lookupC gamma v
		| Flam (t,b) => Twrap (TFarr (t, checkbranch t b))
		| Fapp (e1,e2) => checkFun teq (unarr (Tunwrap (check e1)), check e2)
		| Funit => Twrap TFunit
		| Fint _ => Twrap TFint
		| Fbool _ => Twrap TFbool
		| Ftuple (e1,e2) => Twrap (TFprod (check e1, check e2))
		| Fpi (lr, e) => projLR lr (unprod (Tunwrap (check e)))
		| Finj (lr, t, e) => Twrap (TFsum (injLR lr (check e) t))
		| Fcase (e1,b1,b2) => assertSame teq (zip2 checkbranch (unsum (Tunwrap (check e1))) (b1,b2)) 
		| Fif (e1,e2,e3) => (assertSame teq (Twrap TFbool, check e1); assertSame teq (check e2, check e3))
		| Flet (e,b) => checkbranch (check e) b
		| Ferror t => t
		| Fbinop (bo, e1, e2) => binSame teq (check e1, check e2) (primTypes bo)
	end
  
fun evalF env evalRec (extendC,lookupC) Vwrap Vunwrap exp = 
	let
		val eval = evalRec env
		fun evalBranch value (var,e) = evalRec (extendC env var value) e
		val convertP = convertPrim o Vunwrap
	in
		case exp of 
		  Fvar v => lookupC env v
		| Flam (t, (x,e)) => Vwrap (VFlam (x, e))
		| Fapp (e1, e2) => evalBranch (eval e2) (unlam (Vunwrap (eval e1)))
		| Funit => Vwrap VFunit
		| Fint i => Vwrap (VFint i)
		| Fbool b => Vwrap (VFbool b)
		| Ftuple (e1, e2) => Vwrap (VFtuple (eval e1, eval e2))
		| Fpi (side, e) => (case side of Left => #1 | Right => #2) (untuple (Vunwrap (eval e)))
		| Finj (side, _, e) => Vwrap (VFinj (side, eval e))
		| Fcase (e1, b1, b2) => (case uninj (Vunwrap (eval e1)) of (Left, v) => evalBranch v b1 | (Right, v) => evalBranch v b2)
		| Fif (e1, e2, e3) => eval (if unbool (Vunwrap (eval e1)) then e2 else e3)
		| Flet (e, b) => evalBranch (eval e) b
		| Fbinop (bo,e1,e2) => Vwrap (unconvertPrim (Prims.PrimEval.evalPrim (bo, convertP (eval e1), convertP (eval e2))))
		| Ferror t => raise Stuck
	end

end
