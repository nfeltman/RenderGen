
structure SourceLang = 
struct

local
open LangCommon
open ValuesBase

infixr 9 `
fun a ` b = a b

in
datatype ('r,'p) pattern 	= Pvar of 'r
							| Ptuple of 'p list
							| Punused
							
datatype ('e,'r,'b,'t) exprF	= Fvar of 'r
								| Flam of 't * 'b
								| Fapp of 'e * 'e
								| Ffix of 't * 't * 'b
								| SEprod of 'e BranchlessFrag.exprF
								| SEdata of ('e, 'b, 't) DataFrag.exprF
								| Flet of 'e * 'b
								| Froll of 't * 'e
								| Funroll of 'e

fun mapPattern _ (Pvar v) = Pvar v
  | mapPattern f (Ptuple ps) = Ptuple (map f ps)
  | mapPattern _ Punused = Punused
								
fun mapExpr fe ft fp exp =
	case exp of
	  Fvar v => Fvar v
	| Flam (t, (x,e)) => Flam (ft t, (fp x, fe e))
	| Fapp (e1,e2) => Fapp (fe e1, fe e2)
	| SEprod e => SEprod ` BranchlessFrag.mapExpr fe e
	| SEdata e => SEdata ` DataFrag.mapExpr (fe,fn (x,e) => (fp x, fe e),ft) e
	| Flet (e1, (x,e2)) => Flet (fe e1, (fp x, fe e2))
	| Froll (t, e) => Froll (ft t, fe e)
	| Funroll e => Funroll (fe e)
	| Ffix (t1, t2, (x,e)) => Ffix (ft t1, ft t2, (fp x, fe e))

fun collectExpr exp =
	case exp of
	  Fvar v => []
	| Flam (t, (x,e)) => [e]
	| Fapp (e1,e2) => [e1,e2]
	| SEprod e => BranchlessFrag.collectExpr e
	| SEdata e => DataFrag.collectExpr ` DataFrag.mapExpr (id,#2,id) e
	| Flet (e1, (x,e2)) => [e1,e2]
	| Froll (t, e) => [e]
	| Funroll e => [e]
	| Ffix (t1,t2,(x,e)) => [e]

fun recastPattern (_,ext,f) g (Pvar x) = let val y = f x in (Pvar y,ext g x y) end
  | recastPattern (rpRec,ext,_) g (Ptuple xs) =
		let 
			fun f (x,(ys,g2)) = 
				let val (y,g3) = rpRec g2 x in (y::ys,g3) end
			val (ys, g2) = foldr f ([],g) xs
		in 
			(Ptuple ys, g2)
		end
  | recastPattern _ g Punused = (Punused, g)
	
fun replaceVars recRep G (recastPattern,lookup) exp =
	let
		val rep = recRep G
		fun forBranch (x,e) = let val (y,g) = recastPattern G x in (y, recRep g e) end
	in
		case exp of
		  Fvar v => Fvar (lookup G v)
		| Flam (t, b) => Flam (t, forBranch b)
		| Fapp (e1,e2) => Fapp (rep e1, rep e2)
		| SEprod e => SEprod ` BranchlessFrag.mapExpr rep e
		| SEdata e => SEdata ` DataFrag.mapExpr (rep,forBranch,id) e
		| Flet (e1, b) => Flet (rep e1, forBranch b)
		| Froll (t, e) => Froll (t, rep e)
		| Funroll e => Funroll (rep e)
		| Ffix (t1,t2,b) => Ffix (t1,t2, forBranch b)
	end

fun foldPattern (f,foldRec,unpack,ex) g p t =
	let
		fun fold g (Pvar x) t = f g x t
		  | fold g (Ptuple xs) t = foldList g xs (unpack t)
		  | fold g Punused _ = g
		  
		and foldList g [] [] = g
		  | foldList g (x::xs) (t::ts) = foldRec (foldList g xs ts) x t
		  | foldList _ _ _ = raise ex
	in
		fold g p t
	end


functor TypeChecker 
	(T : TypeSystem)
	(F : SourceTypes where type t = T.ty)
	(C : Context where type t = T.ty)
	(P : sig type p type c  type t val fold : c -> p -> t -> c end where type c = C.cont and type t = T.ty)
= struct
structure ProdTC = BranchlessFrag.TC (F)
structure DataTC = DataFrag.TC (F)(T)
  
fun typeCheckSpecial (gamma : C.cont) (checkrec : C.cont -> 'e -> T.ty * 'o) (exp : ('e,C.var,P.p * 'e,T.ty) exprF) = 
	let
		val check = checkrec gamma
		val checkbranch = typeCheckBranch gamma checkrec
		fun checkFun ((a,b),c) = if T.teq a c then b else raise (TypeError "function domain")
		fun binSame (a : Prims.primType, b : Prims.primType) (c,d,e) = if (a = c) andalso (b = d) then e else raise (TypeError "binop")
		fun selfSubst t = F.subst (F.makerec t) t
		fun assertSame (a,b) = if T.teq a b then a else raise (TypeError "branches not same")
		fun assertAllSame [] = raise (TypeError "no branches in case")
		  | assertAllSame (t::[]) = t
		  | assertAllSame (t::ts) = assertSame (t,assertAllSame ts)
	in
		case exp of 
		  Fvar v => (C.lookup gamma v, Fvar v)
		| Flam (t,b as (x,_)) => 
			let
				val o2 as (t2,_) = checkbranch (t,b)
			in
				(F.makearr (t, t2), Flam (t,(x,o2)))
			end
		| Fapp (e1,e2) => 
			let
				val (o1 as (t1,_),o2 as (t2,_)) = (check e1, check e2)
			in
				(checkFun (F.unarr t1, t2), Fapp (o1,o2))
			end
		| SEprod e => 
			let
				val tos = BranchlessFrag.mapExpr check e
			in
				(ProdTC.tc (BranchlessFrag.mapExpr #1 tos), SEprod tos)
			end
		| SEdata e => 
			let
				val e1 = DataFrag.mapExpr (check,id,id) e
				val e2 = DataTC.propDown e1
				val e3 = DataFrag.mapExpr (id,fn (t,(x,e)) => (x,checkbranch (t,(x,e))),id) e2
			in
				(DataTC.checkUp (DataFrag.mapExpr (#1,#1 o #2,id) e3), SEdata e3)
			end
		| Flet (e,b) =>
			let
				val o1 as (t1,_) = check e
				val o2 as (t2,_) = checkbranch (t1,b)
			in
				(t2, Flet (o1, (#1 b, o2)))
			end
		| Froll (t, e) => 
			let
				val o1 as (t1,_) = check e
			in
				(if T.teq (selfSubst t) t1 then F.makerec t else raise (TypeError ""), Froll (t, o1))
			end
		| Funroll e =>
			let
				val o1 as (t,_) = check e
			in
				(selfSubst (F.unrec t), Funroll o1)
			end
		| Ffix (t1,t2,b as (x,_)) =>
			let
				val o1 as (t,_) = checkbranch (F.makeprod [F.makearr (t1,t2),t1],b)
			in
				assertSame (t,t2);
				(F.makearr (t1,t2), Ffix (t1,t2,(x,o1)))
			end
	end

and typeCheckBranch gamma checkrec (t,(patt,e)) = 
		checkrec (P.fold gamma patt t) e
		
fun typeCheck gamma checkrec exp = #1 (typeCheckSpecial gamma (fn c => fn e => (checkrec c e, ())) exp)
end

functor Evaluator (type t) (F : SourceValues where type v = t where type f = t) = 
struct
structure ProdEval = BranchlessFrag.Eval (F)
structure DataEval = DataFrag.Eval (F)
fun evalF env evalRec (extendPatt,lookupC) exp = 
	let
		val eval = evalRec env
		fun evalBranch v (patt,e) = evalRec (extendPatt env patt v) e
	in
		case exp of 
		  Fvar v => lookupC env v
		| Flam (_, b) => F.makelam (fn v => evalBranch v b)
		| Fapp (e1, e2) => (F.unlam (eval e1)) (eval e2)
		| SEprod e => ProdEval.eval eval e
		| SEdata e => DataEval.eval (eval,evalBranch) e
		| Flet (e, b) => evalBranch (eval e) b
		| Froll (_, e) => F.makeroll (eval e)
		| Funroll e => F.unroll (eval e)
		| Ffix (_,_,b) => 
			let 
				fun f v = evalBranch (F.maketuple [F.makelam f, v]) b 
			in F.makelam f end
	end
end

end
end
