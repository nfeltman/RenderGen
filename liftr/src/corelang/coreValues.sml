structure ValuesBase = 
struct
	datatype ('v,'c,'r,'e) valueF	= VFint of int
									| VFbool of bool
									| VFunit
									| VFroll of 'v
									| VFtuple of 'v list
									| VFinj of LangCommon.LR * 'v
									| VFlam of 'c * ('r * 'e)
	
	fun unroll (VFroll v) = v
	  | unroll _ = raise LangCommon.Stuck
	fun untuple (VFtuple v) = v
	  | untuple _ = raise LangCommon.Stuck
	fun uninj (VFinj v) = v
	  | uninj _ = raise LangCommon.Stuck
	fun unbool (VFbool b) = b
	  | unbool _ = raise LangCommon.Stuck
	fun unint (VFint i) = i
	  | unint _ = raise LangCommon.Stuck
	fun unlam (VFlam e) = e
	  | unlam _ = raise LangCommon.Stuck
	fun ununit VFunit = ()
	  | ununit _ = raise LangCommon.Stuck
end

signature ValueProvider = 
sig
	type v
	type lamBody
	type valueF
	
	val VFunit  : valueF
	val VFint   : int -> valueF
	val VFbool  : bool -> valueF
	val VFroll  : v -> valueF
	val VFtuple : v * v -> valueF
	val VFinj   : LangCommon.LR * v -> valueF
	val VFlam   : lamBody -> valueF
	
	val unint   : valueF -> int
	val unbool  : valueF -> bool
	val ununit  : valueF -> unit
	val unroll  : valueF -> v
	val untuple : valueF -> v * v
	val uninj   : valueF -> LangCommon.LR * v
	val unlam   : valueF -> lamBody
end
(*
structure BaseValueProvider : ValueProvider = struct open ValuesBase end 
*)
functor ConjugateValues (structure B : ValueProvider; 
	structure M :
	sig
		type newVF
		val f : B.valueF -> newVF
		val g : newVF -> B.valueF 
	end) : ValueProvider = 
struct
	open M
	type v = B.v
	type lamBody = B.lamBody
	type valueF = newVF
	
	val VFunit	= f B.VFunit
	val VFint	= f o B.VFint
	val VFbool	= f o B.VFbool
	val VFroll	= f o B.VFroll
	val VFtuple	= f o B.VFtuple
	val VFinj	= f o B.VFinj
	val VFlam	= f o B.VFlam
	
	val unint   = B.unint o g
	val unbool  = B.unbool o g
	val ununit  = B.ununit o g
	val unroll  = B.unroll o g
	val untuple = B.untuple o g
	val uninj   = B.uninj o g
	val unlam   = B.unlam o g
end
