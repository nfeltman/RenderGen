signature TypeSystem = 
sig
	type ty
	val teq : ty -> ty -> bool
	val toString : ty -> string
end

signature SourceTypes = 
sig
	type t
	val makeprim	: Prims.primType -> t
	val makevar		: int -> t
	val makerec		: t -> t
	val makeprod	: t list -> t
	val makesum		: t list -> t
	val makearr		: t * t -> t
	val unprim		: t -> Prims.primType
	val unrec		: t -> t
	val unprod		: t -> t list
	val unsum		: t -> t list
	val unarr		: t -> t * t
	val subst		: t -> t -> t
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

functor EmbedTypes (T : sig 
	type u 
	val outof : u -> u TypesBase.typeF
	val into : u TypesBase.typeF -> u 
	val subst : u -> u -> u 
end) : SourceTypes = 
struct
	type t = T.u
	val unprim = TypesBase.unprim o T.outof
	val unrec = TypesBase.unrec o T.outof
	val unprod = TypesBase.unprod o T.outof
	val unsum = TypesBase.unsum o T.outof
	val unarr = TypesBase.unarr o T.outof
	val makeprim = T.into o TypesBase.TFprim
	val makevar = T.into o TypesBase.TFvar
	val makerec = T.into o TypesBase.TFrec
	val makeprod = T.into o TypesBase.TFprod
	val makesum = T.into o TypesBase.TFsum
	val makearr = T.into o TypesBase.TFarr
	val subst = T.subst
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
