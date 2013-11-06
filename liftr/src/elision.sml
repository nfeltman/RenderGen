
structure Elision = 
struct

open LangCommon
structure S = Lambda12
open LambdaPSF

fun elideTy1 S.T1unit = Tunit
  | elideTy1 (S.T1prod (t1,t2)) = Tprod (elideTy1 t1, elideTy1 t2)
  | elideTy1 (S.T1sum (t1,t2)) = Tsum (elideTy1 t1, elideTy1 t2)
  | elideTy1 (S.T1func (t1,t2)) = Tfunc (elideTy1 t1, elideTy1 t2)
  | elideTy1 (S.T1fut t) = elideTy2 t
  
and elideTy2 S.T2unit = Tunit
  | elideTy2 (S.T2prod (t1,t2)) = Tprod (elideTy2 t1, elideTy2 t2)
  | elideTy2 (S.T2sum (t1,t2)) = Tsum (elideTy2 t1, elideTy2 t2)
  | elideTy2 (S.T2func (t1,t2)) = Tfunc (elideTy2 t1, elideTy2 t2)

fun elideBr1 (v,e) = (v, elide1 e)
and elide1 exp = 
	case exp of 
	  S.E1var (v) => Evar v
	| S.E1lam (t,b) => Elam (elideTy1 t, elideBr1 b)
	| S.E1app (e1,e2) => Eapp (elide1 e1, elide1 e2)
	| S.E1unit => Eunit
	| S.E1tuple (e1,e2) => Etuple [elide1 e1, elide1 e2]
	| S.E1pi (Left, e) => Epi (0, elide1 e)
	| S.E1pi (Right, e) => Epi (1, elide1 e)
	| S.E1inj (lr, t, e) => Einj (lr, elideTy1 t, elide1 e)
	| S.E1case (e,b1,b2) => Ecase (elide1 e, elideBr1 b1, elideBr1 b2)
	| S.E1next e => elide2 e

and elideBr2 (v,e) = (v, elide2 e)
and elide2 exp = 
	case exp of 
	  S.E2var (v) => Evar v
	| S.E2lam (t,b) => Elam (elideTy2 t, elideBr2 b)
	| S.E2app (e1,e2) => Eapp (elide2 e1, elide2 e2)
	| S.E2unit => Eunit
	| S.E2tuple (e1,e2) => Etuple [elide2 e1, elide2 e2]
	| S.E2pi (Left, e) => Epi (0, elide2 e)
	| S.E2pi (Right, e) => Epi (1, elide2 e)
	| S.E2inj (lr, t, e) => Einj (lr, elideTy2 t, elide2 e)
	| S.E2case (e,b1,b2) => Ecase (elide2 e, elideBr2 b1, elideBr2 b2)
	| S.E2prev e => elide1 e
	
end
