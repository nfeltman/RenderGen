
structure Examples = 
struct

open Lambda12c

(*
fun runtests () = 

	let
		val _ = Variable.reset ()
		fun v s = Variable.newvar s

		fun EinjL (t,e) = Einj(Left, t, e)
		fun EinjR (t,e) = Einj(Right, t, e)

		val vhold1 = v "hold1"
		val vhold2 = v "hold2"
		fun nv s f = let val vari = v s in f (vari,Evar vari) end

		val two = Tsum (Tunit, Tunit)
		val three = Tsum (Tunit, two)
		val hold1 = FuncDec1 (vhold1, Tunit, Tfut Tunit, v "unused", Enext Eunit)
		val hold2 = nv "x" (fn (x,vx) => 
						FuncDec1 (vhold2, 
						two, Tfut two, 
						x, 
						Ecase(vx,
							nv "x1" (fn (x1, vx1) => (x1, Enext(EinjL(Tunit,Eprev(Ecall(vhold1,vx1)))))),
							nv "x2" (fn (x2, vx2) => (x2, Enext(EinjR(Tunit,Eprev(Ecall(vhold1,vx2))))))
						))
					)
		val hold3 = nv "x" (fn (x,vx) => 
						FuncDec1 (v "hold3", 
						three, Tfut three, 
						x, 
						Ecase(vx,
							nv "x1" (fn (x1, vx1) => (x1, Enext(EinjL(two  ,Eprev(Ecall(vhold1,vx1)))))),
							nv "x2" (fn (x2, vx2) => (x2, Enext(EinjR(Tunit,Eprev(Ecall(vhold2,vx2))))))
						))
					)

		val prog1 = [hold1]
		val prog2 = [hold1, hold2]
		val prog3 = [hold1, hold2, hold3]

		fun testProgram p = 
			let
				val propegated = PropStage.propProgram p
				val _ = Typecheck12.checkProgram propegated
				val (split1, split2) = StageSplit.splitProg propegated
				val _ = PrintPSF.printTerm print split1
				val _ = print "\n========\n"
				val _ = TypecheckPSF.typeCheck split1
				val _ = PrintPSF.printTerm print split2
				val _ = print "\n========\n"
				val _ = TypecheckPSF.typeCheck split2 
			in
				()
			end
		val _ = testProgram prog1
		val _ = testProgram prog2
		val _ = testProgram prog3
	in
		"All pass."
	end*)
end
