
signature TypeHelper =
sig
	structure E : LATTICE
	
	val getBasicDecompType : BasicSyntax.primDecomp -> (E.t*E.t) BasicSyntax.entityType
	val justOne : E.t
	
end
