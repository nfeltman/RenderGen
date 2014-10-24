signature SourceValues = 
sig
	type v (* value *)
	type c (* context *)
	type r (* variable *)
	type e (* expr *)
	
	val makeprim   : Prims.primValue -> v
	val makeroll  : v -> v
	val maketuple : v list -> v
	val makeinj   : int * v -> v
	val makelam   : c * (r * e) -> v
	
	val unprim  : v -> Prims.primValue
	val unroll  : v -> v
	val untuple : v -> v list
	val uninj   : v -> int * v
	val unlam   : v -> c * (r * e)
end

structure ValuesBase = 
struct
	datatype ('v,'c,'r,'e) valueF	= VFprim of Prims.primValue
									| VFroll of 'v
									| VFtuple of 'v list
									| VFinj of int * 'v
									| VFlam of 'c * ('r * 'e)
	
	fun unroll (VFroll v) = v
	  | unroll _ = raise LangCommon.Stuck
	fun untuple (VFtuple v) = v
	  | untuple _ = raise LangCommon.Stuck
	fun uninj (VFinj v) = v
	  | uninj _ = raise LangCommon.Stuck
	fun unprimV (VFprim p) = p
	  | unprimV _ = raise LangCommon.Stuck
	fun unlam (VFlam e) = e
	  | unlam _ = raise LangCommon.Stuck
	  
	fun mapValue f fe (VFprim p) = VFprim p 
	  | mapValue f fe (VFroll v) = VFroll (f v)
	  | mapValue f fe (VFtuple vs) = VFtuple (map f vs)
	  | mapValue f fe (VFinj (i,v)) = VFinj (i, f v)
	  | mapValue f fe (VFlam (c,(r,e))) = VFlam (c,(r,fe e))
end


functor EmbedValues (V : sig 
	type v 
	type c
	type r
	type e
	val outof : v -> (v,c,r,e) ValuesBase.valueF
	val into : (v,c,r,e) ValuesBase.valueF -> v 
end) : SourceValues = 
struct 
	type v = V.v
	type c = V.c 
	type r = V.r 
	type e = V.e
	
	val makeprim	= V.into o ValuesBase.VFprim
	val makeroll	= V.into o ValuesBase.VFroll
	val maketuple	= V.into o ValuesBase.VFtuple
	val makeinj		= V.into o ValuesBase.VFinj
	val makelam		= V.into o ValuesBase.VFlam
	
	val unprim  = ValuesBase.unprimV o V.outof
	val unroll  = ValuesBase.unroll o V.outof
	val untuple = ValuesBase.untuple o V.outof
	val uninj   = ValuesBase.uninj o V.outof
	val unlam   = ValuesBase.unlam o V.outof
end