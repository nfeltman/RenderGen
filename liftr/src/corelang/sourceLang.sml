
structure SourceLang = 
struct

local
open LangCommon
open Contexts
open ValuesBase

in
datatype 'r pattern 		= Pvar of 'r
							| Ptuple of ('r pattern) list
							
datatype ('e,'r,'t) exprF	= Fvar of 'r
							| FprimVal of Prims.primValue
							| Flam of 't * ('r pattern * 'e)
							| Fapp of 'e * 'e
							| Ftuple of 'e list
							| Fpi of int * 'e
							| Finj of 't list * 't list * 'e
							| Fcase of 'e * ('r pattern * 'e) list
							| Fif of 'e * 'e * 'e
							| Flet of 'e * ('r pattern * 'e)
							| Froll of 't * 'e
							| Funroll of 'e
							| Ferror of 't
							| Fbinop of Prims.binops * 'e * 'e
						
fun mapExpr fe ft exp =
	case exp of
	  Fvar v => Fvar v
	| FprimVal pv => FprimVal pv
	| Flam (t, (x,e)) => Flam (ft t, (x, fe e))
	| Fapp (e1,e2) => Fapp (fe e1, fe e2)
	| Ftuple es => Ftuple (map fe es)
	| Fpi (lr, e) => Fpi (lr, fe e)
	| Finj (ts, us, e) => Finj (map ft ts, map ft us, fe e)
	| Fcase (e1,xes) => Fcase (fe e1, map (fn (x,e) => (x, fe e)) xes)
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
		| FprimVal pv => FprimVal pv
		| Flam (t, b) => Flam (t, forBranch b)
		| Fapp (e1,e2) => Fapp (rep e1, rep e2)
		| Ftuple es => Ftuple (map rep es)
		| Fpi (lr, e) => Fpi (lr, rep e)
		| Finj (ts, us, e) => Finj (ts, us, rep e)
		| Fcase (e1,bs) => Fcase (rep e1, map forBranch bs)
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


functor TypeChecker 
	(T : TypeSystem)
	(F : SourceTypes where type t = T.ty)
	(C : Context where type t = T.ty)
= struct
  
fun typeCheck (gamma : C.cont) (checkrec : C.cont -> 'e -> T.ty) (exp : ('e,C.var,T.ty) exprF) = 
	let
		val check = checkrec gamma
		val checkbranch = typeCheckBranch gamma checkrec
		fun checkFun ((a,b),c) = if T.teq a c then b else raise (TypeError "function domain")
		fun binSame (a,b) (c,d,e) = if (a = c) andalso (b = d) then e else raise (TypeError "binop")
		fun selfSubst t = F.subst (F.makerec t) t
		fun assertSame (a,b) = if T.teq a b then a else raise (TypeError "branches not same")
		fun assertAllSame [] = raise (TypeError "no branches in case")
		  | assertAllSame (t::[]) = t
		  | assertAllSame (t::ts) = assertSame (t,assertAllSame ts)
		val checkOpArg = F.unprim o check
	in
		case exp of 
		  Fvar v => C.lookup gamma v
		| Flam (t,b) => F.makearr (t, checkbranch (t,b))
		| Fapp (e1,e2) => checkFun (F.unarr (check e1), check e2)
		| FprimVal pv => F.makeprim (Prims.getValType pv)
		| Ftuple es => F.makeprod (map check es)
		| Fpi (i, e) => List.nth(F.unprod (check e), i)
		| Finj (ts, us, e) => F.makesum (ts @ (check e :: us))
		| Fcase (e1,bs) => assertAllSame 
				(zip checkbranch (F.unsum (check e1)) bs (TypeError "wrong number of branches")) 
		| Fif (e1,e2,e3) => (Prims.assertBool (F.unprim (check e1)); assertSame (check e2, check e3))
		| Flet (e,b) => checkbranch (check e, b)
		| Ferror t => t
		| Froll (t, e) => if T.teq (selfSubst t) (check e) then F.makerec t else raise (TypeError "")
		| Funroll e => selfSubst (F.unrec (check e))
		| Fbinop (bo, e1, e2) => F.makeprim (binSame (checkOpArg e1, checkOpArg e2) (Prims.getBinopType bo))
	end

and typeCheckBranch gamma checkrec (t,(patt,e)) = 
		checkrec (forPattern (C.extend, F.unprod, TypeError "pattern") gamma patt t) e
end

functor Evaluator (F : SourceValues where type r = var pattern) = 
struct
fun evalF env evalRec (extendPatt,lookupC) exp = 
	let
		val eval = evalRec env
		fun evalBranchE value (env,(patt,e)) = evalRec (extendPatt env patt value) e
		fun evalBranch v b = evalBranchE v (env,b)
		val evalOpArg = F.unprim o eval
	in
		case exp of 
		  Fvar v => lookupC env v
		| Flam (t, b) => F.makelam (env,b)
		| Fapp (e1, e2) => evalBranchE (eval e2) (F.unlam (eval e1))
		| FprimVal pv => F.makeprim pv
		| Ftuple es => F.maketuple (map eval es)
		| Fpi (i, e) => List.nth (F.untuple (eval e), i)
		| Finj (ts, _, e) => F.makeinj (length ts, eval e)
		| Fcase (e, bs) => (case F.uninj (eval e) of (i, v) => evalBranch v (List.nth (bs,i)))
		| Fif (e1, e2, e3) => eval (if Prims.unbool (F.unprim (eval e1)) then e2 else e3)
		| Flet (e, b) => evalBranch (eval e) b
		| Fbinop (bo,e1,e2) => F.makeprim (Prims.evalPrim (bo, evalOpArg e1, evalOpArg e2))
		| Froll (_, e) => F.makeroll (eval e)
		| Funroll e => F.unroll (eval e)
		| Ferror t => raise Stuck
	end
end

end
end
