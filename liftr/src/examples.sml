
structure Examples = 
struct

open Lambda12c

fun EinjL (t,e) = Einj(Left, t, e)
fun EinjR (t,e) = Einj(Right, t, e)

val two = Tsum (Tunit, Tunit)
val three = Tsum (Tunit, two)
val hold1 = FuncDec1 ("hold1", Tunit, Tfut Tunit, "_", Enext Eunit)
val hold2 = FuncDec1 ("hold2", 
			two, Tfut two, 
			"x", 
			Ecase(Evar "x",
				("x1", Enext(EinjL(Tunit,Eprev(Ecall("hold1",Evar "x1"))))),
				("x2", Enext(EinjR(Tunit,Eprev(Ecall("hold1",Evar "x2")))))
			))
val hold3 = FuncDec1 ("hold3", 
			three, Tfut three, 
			"x", 
			Ecase(Evar "x",
				("x1", Enext(EinjL(two  ,Eprev(Ecall("hold1",Evar "x1"))))),
				("x2", Enext(EinjR(Tunit,Eprev(Ecall("hold2",Evar "x2")))))
			))

val prog1 = [hold1]
val prog2 = [hold1, hold2]
val prog3 = [hold1, hold2, hold3]

fun runtests () = 
	let
		fun testProgram p = 
			let
				val propegated = PropStage.propProgram p
				val _ = Typecheck12.checkProgram propegated
				val (split1, split2) = StageSplit.splitProg propegated
			in
				()
			end
		val _ = testProgram prog1
		val _ = testProgram prog2
		val _ = testProgram prog3
	in
		"All pass."
	end
end
