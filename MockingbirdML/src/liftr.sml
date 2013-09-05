
structure Liftr = 
struct

type var = string
type func = string

datatype stageType = ST1 | ST2

datatype Op1 = O1pi1 | O1pi2 | O1inj1 | O1inj2

datatype expr	= Efunc of func * expr
				| Elet of var * expr * expr
				| Evar of var
				| Etuple of expr * expr
				| Eunit of stageType
				| Ecase of expr * (var * expr) * (var * expr)
				| Eop1 of Op1 * expr
				| Epause of expr
	
datatype sexpr	= Sfunc of func * sexpr
				| Slet of var * sexpr * sexpr
				| Svar of var
				| Stuple of sexpr * sexpr
				| Sunit 
				| Scase of sexpr * (var * sexpr) * (var * sexpr)
				| Sop1 of Op1 * sexpr

type context = (var * stageType) list

exception BadLabel of string
fun findLabel ((l:string,t)::gs) label = if label = l then t else findLabel gs label
  | findLabel [] label = (raise (BadLabel label))

exception StageMismatch

datatype splitResult	= Stage1 of sexpr 
						| Stage2 of sexpr * var * sexpr 
						| Stage2t of sexpr
						
datatype reducedSplitResult	= Rstage1 of sexpr 
							| Rstage2 of sexpr * var * sexpr

fun castReduced (Stage1 e) = Rstage1 e
  | castReduced (Stage2 (x,y,z)) = Rstage2 (x, y, z)
  | castReduced (Stage2t e) = Rstage2 (Sunit,"_",e)
							
fun mapSR f (Stage1 e) = Stage1 (f e)
  | mapSR f (Stage2 (x,y,z)) = Stage2 (x, y, f z)
  | mapSR f (Stage2t e) = Stage2t (f e)

fun ps gamma ex = 
	case ex of
	  Efunc (name, e) => mapSR (fn x => Sfunc (name, x)) (ps gamma e)
	| Eunit ST1 => Stage1 Sunit
	| Eunit ST2 => Stage2t Sunit
	| Etuple (e1, e2) => (
		case (ps gamma e1, ps gamma e2) of
		  (Stage1 e1, Stage1 e2) => Stage1 (Stuple (e1,e2))
		| (Stage2t (r1), Stage2t (r2)) => Stage2t (Stuple(r1, r2))
		| (Stage2 (p1,x1,r1), Stage2t (r2)) => Stage2 (p1, x1, Stuple(r1, r2))
		| (Stage2t (r1), Stage2 (p2,x2,r2)) => Stage2 (p2, x2, Stuple(r1, r2))
		| (Stage2 (p1,x1,r1), Stage2 (p2,x2,r2)) => 
			Stage2 (Stuple(p1,p2), "t", Stuple(Slet (x1, Sop1 (O1pi1, Svar "t"), r1), Slet (x2, Sop1 (O1pi2, Svar "t"), r2)))
		| _ => raise StageMismatch
		)
	| Eop1 (op1,e) => mapSR (fn x => Sop1 (op1, x)) (ps gamma e)
	| Elet (y, e1, e2) => (
		case (ps gamma e1) of
		  (Stage1 e1) => (
			case (ps ((y,ST1)::gamma) e2) of
			  (Stage1 e2) => Stage1 (Slet (y,e1,e2))
			| (Stage2 (p2,x2,r2)) => Stage2 (Slet (y,e1,p2), x2, r2)
			| (Stage2t _) => raise StageMismatch
			)
		| (Stage2 (p1,x1,r1)) => 
			let
				val li = "lifted"
				val v = Svar li
			in
				case (ps ((y,ST2)::gamma) e2) of
				  (Stage1 e2) => raise StageMismatch
				| (Stage2 (p2,x2,r2)) => Stage2 (Stuple (p1,p2), li, Slet( y, Slet (x1, Sop1 (O1pi1, v), r1), Slet (x2, Sop1 (O1pi2, v), r2)))
				| (Stage2t (r2)) => Stage2 (p1, li, Slet( y, Slet (x1, v, r1), r2))
			end
		| (Stage2t (r1)) => 
			let
				val li = "lifted"
				val v = Svar li
			in
				case (ps ((y,ST2)::gamma) e2) of
				  (Stage1 e2) => raise StageMismatch
				| (Stage2 (p2,x2,r2)) => Stage2 (p2, li, Slet( y, r1, Slet (x2, v, r2)))
				| (Stage2t (r2)) => Stage2t (Slet(y, r1, r2))
			end
		)
	| Evar x => (
		case findLabel gamma x of
		  ST1 => Stage1 (Svar x)
		| ST2 => Stage2t (Svar x)
		)
	| Ecase (e1,(x2,e2),(x3,e3)) => 
		let
			val li = "lifted"
			val v = Svar li
		in
		case (ps gamma e1) of
			  (Stage1 e1) => 
				(
				case (castReduced (ps ((x2,ST1)::gamma) e2), castReduced (ps ((x3,ST1)::gamma) e3)) of
				  (Rstage1 e2, Rstage1 e3) => 
						Stage1 (Scase (e1, (x2, e2), (x3, e3)))
				| (Rstage2 (p2,y2,r2), Rstage2 (p3,y3,r3)) => 
						Stage2 (Scase (e1,(x2,p2),(x3,p3)), li, Scase (v,(y2,r2),(y3,r3)))
				| (Rstage1 _, Rstage2 _) => raise StageMismatch
				| (Rstage2 _, Rstage1 _) => raise StageMismatch
				)
			| (Stage2 (p1,y1,r1)) => 
				(
				case (ps ((x2,ST2)::gamma) e2, ps ((x3,ST2)::gamma) e3) of
				  (Stage2 (p2,y2,r2), Stage2 (p3,y3,r3)) => 
						Stage2 (Stuple(p1,Stuple(p2,p3)), li, 
							Scase (
								Slet (y1, Sop1 (O1pi1, v), r1), 
								(x2, Slet (y2, Sop1 (O1pi1, Sop1 (O1pi2, v)), r2)),
								(x3, Slet (y3, Sop1 (O1pi2, Sop1 (O1pi2, v)), r3))
							)
						)
				| (Stage2t (r2), Stage2 (p3,y3,r3)) => 
						Stage2 (Stuple(p1,p3), li, 
							Scase (
								Slet (y1, Sop1 (O1pi1, v), r1), 
								(x2, r2),
								(x3, Slet (y3, Sop1 (O1pi2, v), r3))
							)
						)
				| (Stage2 (p2,y2,r2), Stage2t (r3)) => 
						Stage2 (Stuple(p1,p2), li, 
							Scase (
								Slet (y1, Sop1 (O1pi1, v), r1), 
								(x2, Slet (y2, Sop1 (O1pi2, v), r2)),
								(x3, r3)
							)
						)
				| (Stage2t (r2), Stage2t (r3)) => Stage2 (p1, y1, Scase (r1,(x2, r2),(x3, r3)))
				| _ => raise StageMismatch
				)
			| (Stage2t r1) => 
				case (ps ((x2,ST2)::gamma) e2, ps ((x3,ST2)::gamma) e3) of
				  (Stage2 (p2,y2,r2), Stage2 (p3,y3,r3)) => 
						Stage2 (Stuple(p2,p3), li, 
							Scase ( r1, 
								(x2, Slet (y2, Sop1 (O1pi1, v), r2)),
								(x3, Slet (y3, Sop1 (O1pi2, v), r3))
							)
						)
				| (Stage2t (r2), Stage2 (p3,y3,r3)) => 
						Stage2 (p3, y3, Scase (r1, (x2, r2), (x3, r3)))
				| (Stage2 (p2,y2,r2), Stage2t (r3)) => 
						Stage2 (p2, y2, Scase (r1, (x2, r2), (x3, r3)))
				| (Stage2t (r2), Stage2t (r3)) => 
						Stage2t (Scase (r1,(x2, r2),(x3, r3)))
				| _ => raise StageMismatch
		end
	| Epause ex => (
		case ps gamma ex of
		  Stage1 e => Stage2 (e, "p", Svar "p")
		| Stage2 _ => raise StageMismatch
		| Stage2t _ => raise StageMismatch
		)

fun printExpr p ex = 
	let
		val pe = printExpr p
	in
		case ex of
		  Sfunc (name, e) => (p name; p "("; pe e; p ")")
		| Sunit => p "()"
		| Stuple (e1, e2) => (p "("; pe e1; p ", "; pe e2; p ")")
		| Sop1 (op1,e) => (p (
				case op1 of 
				  O1pi1 => "pi1"
				| O1pi2 => "pi2"
				| O1inj1 => "inj1"
				| O1inj2 => "inj2")
			; p "("; pe e; p ")")
		| Slet (y, e1, e2) => (p "let "; p y; p " = ("; pe e1; p ") in "; pe e2)
		| Svar x => p x
		| Scase (e1,(x2,e2),(x3,e3)) => (p "case "; pe e1; p " of "; p x2; p " => "; pe e2; p " | "; p x3; p " => "; pe e3)
	end
		
fun runExample () = 
	let
		val g = "g"
		val s = "s"
		val box = "box"
		val exp = 
			Ecase (Efunc ("isect", Etuple (Epause (Efunc ("buildBox", Evar g)), Evar s)), 
				("_", 
					Ecase ((Efunc ("split", Evar g)),
						("g2", Efunc ("closer", Etuple (
							Efunc ("rec", Etuple (Epause (Eop1( O1pi1, Evar "g2")), Evar s)),
							Efunc ("rec", Etuple (Epause (Eop1( O1pi2, Evar "g2")), Evar s))
						))),
						("g1", Efunc ("hit", Etuple (Epause (Evar "g1"), Evar s)))
					)),
				("_", Efunc ("inf", Evar s))
			)
		val context = [(g,ST1),(s,ST2)]
		val Stage2 (p,x,r) = ps context exp
	in
		print "\n---------\n";
		printExpr print p;
		print "\n---------\n";
		print x;
		print "\n---------\n";
		printExpr print r;
		print "\n---------\n"
	end
end
