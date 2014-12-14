signature Dict = 
sig
	type 't cont
	type var

	exception UnboundVar of var

	val empty : 't cont 
	val extend : 't cont -> var -> 't -> 't cont
	val lookup : 't cont -> var -> 't
end

functor ListDict (eqtype var) :> Dict 
	where type var = var = 
struct
	exception UnboundVar of var
	
	type 't cont = (var * 't) list
	type var = var

	val empty = []
	fun extend g v t = (v,t) :: g
	fun lookup [] v = raise (UnboundVar v)
	  | lookup ((v2,t)::g) v = if v = v2 then t else lookup g v
end

signature Context = 
sig
	type cont
	type var
	type t

	exception UnboundVar of var

	val empty : cont 
	val extend : cont -> var -> t -> cont
	val lookup : cont -> var -> t
end

functor BasicContext 
	(D : Dict) 
	(type t) 
	: Context = 
struct
	exception UnboundVar = D.UnboundVar

	type cont = t D.cont
	type var = D.var
	type t = t

	val empty = D.empty
	val extend = D.extend
	val lookup = D.lookup
end

functor DoubleContext 
	(D : Dict) 
	(type t1) (type t2) = 
struct
	datatype entry = Bind1 of t1 | Bind2 of t2
	structure Base = BasicContext (D) (type t = entry)
end

functor TripleContext 
	(D : Dict) 
	(type t1) (type t2) (type t3) = 
struct
	type t1 = t1 type t2 = t2 type t3 = t3
	datatype entry = Bind1 of t1 | Bind2 of t2 | Bind3 of t3
	structure Base = BasicContext (D) (type t = entry)
end

functor EmbedContext (M : sig 
	structure C : Context
	type t
	val outof : C.t -> t
	val into : t -> C.t 
end) : Context = 
struct
	type cont = M.C.cont
	type var = M.C.var
	type t = M.t
	exception UnboundVar = M.C.UnboundVar

	val empty = M.C.empty
	fun extend g x v = M.C.extend g x (M.into v)
	fun lookup g x = M.outof (M.C.lookup g x)
end

functor ProjectDoubleContext
	(M : sig 
		type t1 type t2
		datatype entry = Bind1 of t1 | Bind2 of t2
		structure Base : Context where type t = entry
	end) = 
struct
	exception WrongStage
	
	structure C1 = EmbedContext (struct 
		structure C = M.Base 
		type t = M.t1
		val into = M.Bind1
		fun outof (M.Bind1 v) = v | outof (M.Bind2 v) = raise WrongStage
	end)
	structure C2 = EmbedContext (struct 
		structure C = M.Base 
		type t = M.t2
		val into = M.Bind2
		fun outof (M.Bind2 v) = v | outof (M.Bind1 v) = raise WrongStage
	end)
end

functor ProjectTripleContext 
	(M : sig 
		type t1 type t2 type t3
		datatype entry = Bind1 of t1 | Bind2 of t2 | Bind3 of t3
		structure Base : Context where type t = entry
	end ) = 
struct
	exception WrongStage
	
	structure C1 = EmbedContext (struct 
		structure C = M.Base 
		type t = M.t1
		val into = M.Bind1
		fun outof (M.Bind1 v) = v | outof _ = raise WrongStage
	end)
	structure C2 = EmbedContext (struct 
		structure C = M.Base 
		type t = M.t2
		val into = M.Bind2
		fun outof (M.Bind2 v) = v | outof _ = raise WrongStage
	end)
	structure C3 = EmbedContext (struct 
		structure C = M.Base 
		type t = M.t3
		val into = M.Bind3
		fun outof (M.Bind3 v) = v | outof _ = raise WrongStage
	end)
end
