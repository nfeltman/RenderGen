
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
				| Einfix of string * expr list
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
		| Einfix (bo, es) => need 2 ` Einfix (bo, map (rp 1) es)
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
		| Einfix (bo, []) => $ ("("^bo^")")
		| Einfix (bo, e0::es) => & (c e0 :: foldr (fn (e,prev) => $ bo :: c e :: prev) [] es)
		| EprimApp (f, e) => & [$ f, $" ", c e]
		| EbraceApp (f, e) => % [& [$ f, $ " {"], >> 4 (c e), $ "}"]
	end
fun printExp p e = p (Layout.tostringex 80 (convertToLayout (resolvePresedence e)))
				
end
