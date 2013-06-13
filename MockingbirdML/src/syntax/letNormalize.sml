

structure LetNormalize = 
struct
	
	open MbSML
	fun id x = x

	fun	norm ex f = 
		case ex of
		  Eif (e1, e2, e3) => norm e1 (fn c1 => f (Eif (c1, norm e2 id, norm e3 id)))
		| Etuple (es) => foldl (fn (e,g) => fn cs => norm e (fn c => g (c::cs))) (f o Etuple) es []
		| Eproj (i, e) => norm e (fn c => f (Eproj(i,c)))
		| EgetElement (e1, e2) => norm e1 (fn c1 => norm e2 (fn c2 => f (EgetElement (c1,c2))))
		| EsetElement (e1, e2, e3) => norm e1 (fn c1 => norm e2 (fn c2 => norm e3 (fn c3 => f (EsetElement (c1,c2,c3)))))
		| Ecall (name, e) => norm e (fn c => f (Ecall(name,c)))
		| EbinOp (bop, e1, e2) => norm e1 (fn c1 => norm e2 (fn c2 => f (EbinOp (bop, c1,c2))))
		| Elet (x, e, rest) => norm e (fn c => Elet (x, c, norm rest f))
		| EfuncDefs (f0, fs, rest) => EfuncDefs (normFunc f0, map normFunc fs, norm rest f)
		| Evar v => f ex
		| Eint i => f ex
		| EbotHit => f ex
	and normFunc (lab,arg,ty,e) = (lab, arg, ty, norm e id)
	
	fun normalize ex = norm ex id
end
