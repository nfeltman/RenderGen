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
	exception UnboundVar of D.var

	type cont = t D.cont
	type var = D.var
	type t = t

	val empty = D.empty
	val extend = D.extend
	val lookup = D.lookup
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
	exception UnboundVar of var

	val empty = M.C.empty
	fun extend g x v = M.C.extend g x (M.into v)
	fun lookup g x = M.outof (M.C.lookup g x)
end

functor DoubleContext 
	(D : Dict) 
	(type t1) (type t2) = 
struct
	exception UnboundVar of D.var
	exception WrongStage
	datatype entry = Bind1 of t1 | Bind2 of t2
	
	structure Base = BasicContext (D) (type t = entry)
	structure C1 = EmbedContext (struct 
		structure C = Base 
		type t = t1
		val into = Bind1
		fun outof (Bind1 v) = v | outof (Bind2 v) = raise WrongStage
	end)
	structure C2 = EmbedContext (struct 
		structure C = Base 
		type t = t2
		val into = Bind2
		fun outof (Bind2 v) = v | outof (Bind1 v) = raise WrongStage
	end)
end

functor TripleContext 
	(D : Dict) 
	(type t1) (type t2) (type t3) = 
struct
	exception UnboundVar of D.var
	exception WrongStage
	datatype entry = Bind1 of t1 | Bind2 of t2 | Bind3 of t3
	
	structure Base = BasicContext (D) (type t = entry)
	structure C1 = EmbedContext (struct 
		structure C = Base 
		type t = t1
		val into = Bind1
		fun outof (Bind1 v) = v | outof _ = raise WrongStage
	end)
	structure C2 = EmbedContext (struct 
		structure C = Base 
		type t = t2
		val into = Bind2
		fun outof (Bind2 v) = v | outof _ = raise WrongStage
	end)
	structure C3 = EmbedContext (struct 
		structure C = Base 
		type t = t3
		val into = Bind3
		fun outof (Bind3 v) = v | outof _ = raise WrongStage
	end)
end

(* would be great to get rid of this below *)
structure Contexts = 
struct

exception UnboundVar of string
exception WrongStage

type ('a,'b) context = ('a * 'b) list

val empty = []

fun extendContext g v t = (v,t) :: g
fun lookup [] v = raise (UnboundVar "??")
  | lookup ((v2,t)::g) v = if v = v2 then t else lookup g v
  

structure DoubleContext = struct 
datatype ('a,'b) doubleEntry = Bind1 of 'a | Bind2 of 'b
fun extendContext1 g v t = (v, Bind1 t) :: g
fun extendContext2 g v t = (v, Bind2 t) :: g
fun lookup1 g v = case lookup g v of Bind1 t => t | Bind2 _ => raise (WrongStage)
fun lookup2 g v = case lookup g v of Bind2 t => t | Bind1 _ => raise (WrongStage)
val extendLookup1 = (extendContext1,lookup1)
val extendLookup2 = (extendContext2,lookup2)
end

structure TripleContext = struct  
datatype ('a,'b,'c) tripleEntry = Bind1 of 'a | Bind2 of 'b | Bind3 of 'c
fun extendContext1 g v t = (v, Bind1 t) :: g
fun extendContext2 g v t = (v, Bind2 t) :: g
fun extendContext3 g v t = (v, Bind3 t) :: g
fun lookup1 g v = case lookup g v of Bind1 t => t | _ => raise (WrongStage)
fun lookup2 g v = case lookup g v of Bind2 t => t | _ => raise (WrongStage)
fun lookup3 g v = case lookup g v of Bind3 t => t | _ => raise (WrongStage)
val extendLookup1 = (extendContext1,lookup1)
val extendLookup2 = (extendContext2,lookup2)
val extendLookup3 = (extendContext3,lookup3)
end

end
