
structure PrettyPrinter = 
struct

datatype patt	= Pvar of string
				| Ptuple of patt list
datatype expr	= Eatom of string
				| Elam of expr * (patt * expr)
				| Eapp of expr * expr
				| Etuple of expr list
				| Ecase of expr * (patt * expr) * (patt * expr)
				| Eif of expr * expr * expr
				| Elet of expr * (patt * expr)
				| Ebinop of string * expr * expr
				| EprimApp of string * expr
				| EbraceApp of string * expr

local
fun rp slot ex = 
	let
		infixr 9 `
		fun a ` b = a b
		fun need i e = if i > slot then Etuple [e] else e
		fun rpb (v,e) = (v,rp 2 e)
	in
		case ex of
		  Eatom s => Eatom s
		| Elam (t, b) => need 2 ` Elam (rp 0 t, rpb b)
		| Eapp (e1, e2) => need 1 ` Eapp (rp 1 e1, rp 0 e2)
		| Etuple es => Etuple (map top es)
		| Ecase (e1,b1,b2) => need 2 ` Ecase (top e1, rpb b1, rpb b2)
		| Eif (e1,e2,e3) => need 2 ` Eif (top e1, top e2, top e3)
		| Elet (e,b) => need 2 ` Elet (top e, rpb b)
		| Ebinop (bo, e1, e2) => need 2 ` Ebinop (bo, rp 1 e1, rp 1 e2)
		| EprimApp (f, e) => need 1 ` EprimApp (f, rp 0 e)
		| EbraceApp (f, e) => need 1 ` EbraceApp (f, top e)
	end
and top e = rp 2 e
in
val resolvePresedence = top
end
				
fun pat2string (Pvar x) = x
  | pat2string (Ptuple xs) = "("^(String.concatWith "," (map pat2string xs))^")"
fun convertToLayout ex = 
	let
		val c = convertToLayout
		val $ = Layout.str
		val % = Layout.mayAlign
		val & = Layout.seq
		val >> = Layout.indent
		fun convertBranch (x,e) = %[&[$ (pat2string x), $ " =>"], >> 4 (c e)]
	in
		case ex of
		  Eatom s => $ s
		| Elam (t, (v,e)) => % [& [$ "fn ", $ (pat2string v), $ " : ", c t, $ " =>"], c e]
		| Eapp (e1, e2) => % [c e1, c e2]
		| Etuple [] => $ "()"
		| Etuple (e0::es) => & ($ "(" :: c e0 :: foldr (fn (e,prev) => $ "," :: c e :: prev) [$ ")"] es)
		| Ecase (e1,b1,b2) => % [&[$"case ", c e1, $ " of"], >> 2 (convertBranch b1), &[$"| ", convertBranch b2]]
		| Eif (e1,e2,e3) => % [&[$ "if ", c e1, $ " then"], >> 4 (c e2), $ "else", >> 4 (c e3)]
		| Elet (e1,(v,e2)) => % [ %[ &[ $ "let ", $ (pat2string v), $ " ="], >> 4 (c e1), $ "in"], c e2]
		| Ebinop (bo, e1, e2) => & [c e1, $" ", $ bo, $" ", c e2]
		| EprimApp (f, e) => & [$ f, $" ", c e]
		| EbraceApp (f, e) => % [& [$ f, $ " {"], >> 4 (c e), $ "}"]
	end
fun printExp p e = p (Layout.tostringex 80 (convertToLayout (resolvePresedence e)))
				
				

datatype prioSlot = Pliteral of string | Psubterm of int * prioTerm
withtype prioTerm = int * prioSlot list

datatype hierSlot = Hliteral of string | Hsubterm of hierTerm
withtype hierTerm = hierSlot list

fun resolvePrioTerm slotLevel (termLevel,subs) = 
	if termLevel > slotLevel 
	then [Hliteral "(", Hsubterm (map resolvePrioSlot subs), Hliteral ")"]
	else map resolvePrioSlot subs

and resolvePrioSlot (Pliteral s) = Hliteral s
  | resolvePrioSlot (Psubterm (slotLevel,e)) = Hsubterm(resolvePrioTerm slotLevel e)

fun printTerm p es = 
	app (fn e => case e of Hliteral l => p l | Hsubterm s => printTerm p s) es


fun convertTerm ex = 
	let
		val L = Pliteral
		fun S n e = Psubterm (n, convertTerm e)
		val toString = Variable.toString
	in
		case ex of
		  Eatom s => (0, [L s])
		| Elam (t, (v,e)) => (2, [L ("fn "^pat2string v^" : "), S 2 t, L(" => "),  S 2 e])
		| Eapp (e1, e2) => (1, [S 1 e1, L " ", S 0 e2])
		| Etuple [] => (0, [L "()"])
		| Etuple (e0::es) => (0, L "(" :: S 2 e0 :: foldr (fn (e,prev) => L ", " :: S 2 e :: prev) [L ")"] es)
		| Ecase (e1,(v2,e2),(v3,e3)) => (2, [L "case ", S 2 e1, L (" of "^pat2string v2^" => "), S 2 e2, L (" | "^pat2string v3^" => "), S 2 e3])
		| Eif (e1,e2,e3) => (2, [L "if ", S 2 e1, L " then ", S 2 e2, L " else ", S 2 e3])
		| Elet (e1,(v,e2)) => (2, [L ("let "^pat2string v^" = "), S 2 e1, L " in ", S 2 e2])
		| Ebinop (bo, e1, e2) => (1, [S 2 e1, L bo, S 2 e2])
		| EprimApp (f, e) => (1, [L (f^" "), S 0 e])
		| EbraceApp (f, e) => (1, [L (f^" "), L "{", S 2 e, L "}"])
	end
  
end
