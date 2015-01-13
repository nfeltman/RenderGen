signature MONAD = 
sig
	type 'a monad
	val ret : 'a -> 'a monad
	val bind : 'a monad -> ('a-> 'b monad) -> 'b monad
end

structure IdentityMonad : MONAD = 
struct
	type 'a monad = 'a
	fun ret x = x
	fun bind a f = f a
end

functor ContextMonad (type s) : MONAD = 
struct
	type 'a monad = 'a * (s -> s)
	fun ret x = (x, LangCommon.id)
	fun bind (a,t) f = 
		let 
			val (a2,t2) = f a
		in
			(a2,t o t2)
		end
end