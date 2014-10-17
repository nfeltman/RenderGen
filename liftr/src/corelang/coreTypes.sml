signature SourceTypes = 
sig
	datatype 't typeF	= TFprim of Prims.primType
						| TFvar of int 
						| TFrec of 't
						| TFprod of 't list
						| TFsum of 't list
						| TFarr of 't * 't

	val unprim : 't typeF -> Prims.primType
	val unrec  : 't typeF -> 't
	val unprod : 't typeF -> 't list
	val unarr  : 't typeF -> 't * 't
	val unsum  : 't typeF -> 't list
	val mapType : ('t->'u) -> 't typeF -> 'u typeF
	val teq : ('t -> 't -> bool) -> 't typeF -> 't typeF -> bool
end

structure TypesBase  = 
struct
	open LangCommon
	datatype 't typeF	= TFprim of Prims.primType
						| TFvar of int 
						| TFrec of 't			(* binds *)
						| TFprod of 't list
						| TFsum of 't list
						| TFarr of 't * 't

	fun unprim (TFprim pt) = pt
	  | unprim _ = raise (TypeError "expected prim")
	fun unrec (TFrec a) = a
	  | unrec _ = raise (TypeError "expected rec")
	fun unprod (TFprod ab) = ab
	  | unprod _ = raise (TypeError "expected prod")
	fun unarr (TFarr v) = v
	  | unarr _ = raise (TypeError "expected function")
	fun unsum (TFsum v) = v
	  | unsum _ = raise (TypeError "expected sum")
	
	fun mapType _ (TFprim pt) = TFprim pt
	  | mapType _ (TFvar i) = TFvar i
	  | mapType f (TFrec t) = TFrec (f t)
	  | mapType f (TFsum ts) = TFsum (map f ts)
	  | mapType f (TFprod ts) = TFprod (map f ts)
	  | mapType f (TFarr (t1,t2)) = TFarr (f t1, f t2)  
	
	fun teq _ (TFprim t1) (TFprim t2) = (t1 = t2)
	  | teq eq (TFvar j) (TFvar k) = (j = k)
	  | teq eq (TFrec t) (TFrec u) = eq t u
	  | teq eq (TFprod ts) (TFprod us) = listeq eq ts us
	  | teq eq (TFsum ts) (TFsum us) = listeq eq ts us
	  | teq eq (TFarr (t1,t2)) (TFarr (u1,u2)) = (eq t1 u1) andalso (eq t2 u2)
	  | teq _ _ _ = false
end

structure TypeSubst = 
struct
	open TypesBase
	
	fun liftTy lift n ty =
			case ty of
			  TFvar i => TFvar (if i > n then i+1 else i)
			| TFrec t => TFrec (lift (n+1) t)
			| other => mapType (lift n) other
	
	fun substTy sub lift Twrap n v ty =
			case ty of
			  TFvar i => if i = n then v else Twrap (TFvar (if i > n then i-1 else i))
			| TFrec t => Twrap (TFrec (sub (n+1) (lift n v) t))
			| other => Twrap (mapType (sub n v) other)
end
