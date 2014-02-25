
structure PropStage = 
struct

open LangCommon
open Lambda12c
open Lambda12

exception StagePropException

fun propTy1 Tint = T1int
  | propTy1 Tbool = T1bool
  | propTy1 Tunit = T1unit
  | propTy1 (Tprod (t1,t2)) = T1prod (propTy1 t1, propTy1 t2)
  | propTy1 (Tsum (t1,t2)) = T1sum (propTy1 t1, propTy1 t2)
  | propTy1 (Tfut t) = T1fut (propTy2 t)
  
and propTy2 Tint = T2int
  | propTy2 Tbool = T2bool
  | propTy2 Tunit = T2unit
  | propTy2 (Tprod (t1,t2)) = T2prod (propTy2 t1, propTy2 t2)
  | propTy2 (Tsum (t1,t2)) = T2sum (propTy2 t1, propTy2 t2)
  | propTy2 (Tfut t) = raise StagePropException

fun propBr1 (v,e) = (v, prop1 e)
and prop1 exp = 
	case exp of 
	  Evar v => E1var v
(*	| Ecall (v,e) => E1call (v, prop1 e) *)
	| Eunit => E1unit
	| Etuple (e1,e2) => E1tuple (prop1 e1, prop1 e2)
	| Epi (lr, e) => E1pi (lr, prop1 e)
(*	| Einj (lr, t, e) => E1inj (lr, propTy1 t, prop1 e)
	| Ecase (e,b1,b2) => E1case (prop1 e, propBr1 b1, propBr1 b2) *)
	| Ebinop (bo,e1,e2) => E1binop(bo, prop1 e1, prop1 e2)
	| Eprev e => raise StagePropException
	| Eerror t => E1error (propTy1 t)
	| Enext e => E1next (prop2 e)

and propBr2 (v,e) = (v, prop2 e)
and prop2 exp = 
	case exp of 
	  Evar v => E2var v
(*	| Ecall (v,e) => raise StagePropException (* E2call (v, prop2 e) *) *)
	| Eunit => E2unit
	| Etuple (e1,e2) => E2tuple (prop2 e1, prop2 e2)
	| Epi (lr, e) => E2pi (lr, prop2 e)
(*	| Einj (lr, t, e) => E2inj (lr, propTy2 t, prop2 e)
	| Ecase (e,b1,b2) => E2case (prop2 e, propBr2 b1, propBr2 b2) *)
	| Ebinop (bo,e1,e2) => E2binop(bo, prop2 e1, prop2 e2)
	| Eprev e => E2prev (prop1 e)
	| Eerror t => E2error (propTy2 t)
	| Enext e => raise StagePropException
(*
fun propProgram p = 
	let		
		fun propFunc (Lambda12c.FuncDec1 (f,t1,t2,v,e)) = 
				Lambda12.FuncDec1 (f, propTy1 t1, propTy1 t2, v, prop1 e)
	(*	  | propFunc (Lambda12c.FuncDec2 (f,t1,t2,v,e)) = 
				Lambda12.FuncDec2 (f, propTy2 t1, propTy2 t2, v, prop2 e) *)
	in
		map propFunc p
	end *)
	
end
