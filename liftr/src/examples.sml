
structure Examples = 
struct

open Lambda12

val two1 = T1sum (T1unit, T1unit)
val two2 = T2sum (T2unit, T2unit)
val three1 = T1sum (T1unit, two1)
val three2 = T2sum (T2unit, two2)
val hold1 = FuncDec1 ("hold1", T1unit, T1fut T2unit, "_", E1next E2unit)
val hold2 = FuncDec1 ("hold2", 
			two1, T1fut two2, 
			"x", 
			E1case(E1var "x",
				("x1", E1next(E2inj(Left, T2unit,E2prev(E1call("hold1",E1var "x1"))))),
				("x2", E1next(E2inj(Right,T2unit,E2prev(E1call("hold1",E1var "x2")))))
			))
val hold3 = FuncDec1 ("hold3", 
			three1, T1fut three2, 
			"x", 
			E1case(E1var "x",
				("x1", E1next(E2inj(Left, two2  ,E2prev(E1call("hold1",E1var "x1"))))),
				("x2", E1next(E2inj(Right,T2unit,E2prev(E1call("hold2",E1var "x2")))))
			))

val prog1 = [hold1]
val prog2 = [hold1, hold2]
val prog3 = [hold1, hold2, hold3]

fun runtests () = 
	let
		open Typecheck12
		val _ = checkProgram prog1
		val _ = checkProgram prog2
		val _ = checkProgram prog3
	in
		"All pass."
	end
end
