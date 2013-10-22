
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

fun elide1 exp = 
	case exp of 
	  S.E1var (v) => Evar v
	| S.E1lam (v,t,e) => Elam (v, elideTy1 t, elide1 e)
	| S.E1app (e1,e2) => Eapp (elide1 e1, elide1 e2)
	| S.E1unit => Eunit
	| S.E1tuple (e1,e2) => Etuple (elide1 e1, elide1 e2)
	| S.E1pi (lr, e) => Epi (lr, elide1 e)
	| S.E1inj (lr, t, e) => Einj (lr, elideTy1 t, elide1 e)
	| S.E1case (e1,(v2,e2),(v3,e3)) => Ecase (elide1 e1, (v2, elide1 e2), (v3, elide1 e3))
	| S.E1next e => elide2 e
	
and elide2 exp = 
	case exp of 
	  S.E2var (v) => Evar v
	| S.E2lam (v,t,e) => Elam (v, elideTy2 t, elide2 e)
	| S.E2app (e1,e2) => Eapp (elide2 e1, elide2 e2)
	| S.E2unit => Eunit
	| S.E2tuple (e1,e2) => Etuple (elide2 e1, elide2 e2)
	| S.E2pi (lr, e) => Epi (lr, elide2 e)
	| S.E2inj (lr, t, e) => Einj (lr, elideTy2 t, elide2 e)
	| S.E2case (e1,(v2,e2),(v3,e3)) => Ecase (elide2 e1, (v2, elide2 e2), (v3, elide2 e3))
	| S.E2prev e => elide1 e
	
end
