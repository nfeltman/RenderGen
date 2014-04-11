signature SourceTypes = 
sig
	datatype 't typeF	= TFint
						| TFbool
						| TFunit
						| TFvar of int 
						| TFrec of 't
						| TFprod of 't * 't
						| TFsum of 't * 't
						| TFarr of 't * 't

	val unint  : 't typeF -> unit
	val unbool : 't typeF -> unit
	val ununit : 't typeF -> unit
	val unprod : 't typeF -> 't * 't
	val unarr  : 't typeF -> 't * 't
	val unsum  : 't typeF -> 't * 't
	val mapType : ('t->'u) -> 't typeF -> 'u typeF
	val teq : ('t -> 't -> bool) -> 't typeF -> 't typeF -> bool
end

structure TypesBase :> SourceTypes = 
struct
	open LangCommon
	datatype 't typeF	= TFint
						| TFbool
						| TFunit
						| TFvar of int 
						| TFrec of 't
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
	
	fun mapType _ TFint = TFint
	  | mapType _ TFbool = TFbool
	  | mapType _ TFunit = TFunit
	  | mapType _ (TFvar i) = TFvar i
	  | mapType f (TFrec t) = TFrec (f t)
	  | mapType f (TFsum (t1,t2)) = TFsum (f t1, f t2)
	  | mapType f (TFprod (t1,t2)) = TFprod (f t1, f t2)
	  | mapType f (TFarr (t1,t2)) = TFarr (f t1, f t2)  
	
	fun teq _ TFint TFint = true
	  | teq _ TFbool TFbool = true
	  | teq _ TFunit TFunit = true
	  | teq eq (TFvar j) (TFvar k) = (j = k)
	  | teq eq (TFrec t) (TFrec u) = eq t u
	  | teq eq (TFprod (t1,t2)) (TFprod (u1,u2)) = (eq t1 u1) andalso (eq t2 u2)
	  | teq eq (TFsum (t1,t2)) (TFsum (u1,u2)) = (eq t1 u1) andalso (eq t2 u2)
	  | teq eq (TFarr (t1,t2)) (TFarr (u1,u2)) = (eq t1 u1) andalso (eq t2 u2)
	  | teq _ _ _ = false
end

structure TypeSubst = 
struct
	open TypesBase
	fun substTy sub n v ty =
			case ty of
			  TFvar i => if i = n then v else TFvar (if i > n then i-1 else i)
			| TFrec t => TFrec (sub (n+1) v t)  (* NEED TO ADD LIFTING *)
			| TFsum (t1,t2) => TFsum (sub n v t1, sub n v t2)
			| TFprod (t1,t2) => TFprod (sub n v t1, sub n v t2)
			| TFarr (t1,t2) => TFarr (sub n v t1, sub n v t2)  
			| prim => prim
end
