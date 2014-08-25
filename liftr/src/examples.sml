
structure Examples = 
struct

open SourceLang
open Lambda12c
open LangCommon
open ErasureSemantics
structure Comp = ValueComparison

datatype testLevel = NONE | SAME | EXACT of value1
val ansI = EXACT o V1 o VFint
val ansNI = EXACT o V1next o V2 o VFint
val ansB = EXACT o V1 o VFbool

infixr 9 `
fun a ` b = a b

val _ = Variable.reset ()

fun j test = [test]
fun k (name,prog,t) = [(name^"-first",prog,t), (name^"-second","next{"^prog^"}", case t of EXACT v => EXACT (V1next (holdGeneral v)) | _ => t)]
val programs = [
k("fourPlusSix", 		"4 + 6",ansI 10),
k("twoTimesThree", 		"2 * 3",ansI 6),
k("twoGtThree", 		"2 > 3",ansB false),
k("proj1", 				"#1 (3,true)",ansI 3),
k("proj2", 				"#2 (3,true)",ansB true),
k("proj3", 				"3 + #1 (3,true)",ansI 6),
k("proj4", 				"#1 (3,true) + 3",ansI 6),
k("let1", 				"let x = 4 + 6 in x * x",ansI 100),
k("let2", 				"let (x,b) = (4+6,false) in if b then x else x * x",ansI 100),
j("letnext",			"let x = next{4 + 6} in next {prev{x} * prev{x}}",SAME),
j("doubleBind", 		"let x = next{4 + 6} in let x = next{prev{x} * prev{x}} in next{prev{x} * prev{x}}",SAME),
k("if1", 				"if 3 > 2 then 4 + 6 else 2 * 3",ansI 10),
j("ifFirstThenSecnod",	"if 3 > 2 then next{4 + 6} else next{2 * 3}",SAME),
j("if2", 				"next{ if 2 > 3 then prev{hold (4 + 6)} else 2 * 3}",SAME),
j("ifBothSides",		"next{ if 2 > 3 then prev{hold (4 + 6)} else prev{hold (2 * 3)}}",SAME),
j("ifPred",				"next{ if 2 > prev{hold (2 + 4)} then prev{hold (4 + 6)} else prev{hold (2 * 3)}}",SAME),
j("if7",				"if 3 > 2 then next{prev{hold (8 * 9)} + 6} else next{prev{hold (8 + 9)} * prev{hold (6 - 2)}}",SAME),
j("holdif",				"hold (if 2 > 3 then 1 else 0)",SAME),
j("holdif2",			"next {prev {hold (if 2 > 3 then 1 else 0)}}",SAME),
k("funcApp1", 			"(fn x : int => x + x) 45",ansI 90),
k("funcApp2", 			"let f = fn x : int => x + x in f 45",ansI 90),
k("funcApp3", 			"letfun g (x:int) = x + x in 12 + g 45",ansI 102),
k("funcApp4", 			"letfun g (x:int) = x + x in g 45 + 12",ansI 102),
j("multiStageFunc", 	"(fn x : int => next{prev{hold (x * x)} + prev{hold x}}) 45",SAME),
k("caseLeft", 			"case inl int 34 of x => x * x | y => y + y",ansI 1156),
k("caseRight", 			"case inr int 34 of x => x * x | y => y + y",ansI 68),
k("datatype3", 			"datatype t = A of int | B of int * int | C of int * int * int in " ^
						"case unroll (B (3,4)) of x => x | (x,y) => x+y | (x,y,z) => x+y+z",ansI 7),
k("highOrder1", 		"(fn f:int->int => f 5) (fn x:int=>x+x)",ansI 10),
k("closure", 			"(let x = 3 in fn y:int=> x*y) 5",ansI 15),
k("higherOrder1", 		"(fn f:int->int => fn x:int=> f (f x)) (fn y:int=>y+y) 5",ansI 20),
j("datastruct2", 		"letfun f (x:int) = next{prev{hold (x*x)}+4} in ((f 1,f 2),(f 3,f 4))",SAME),
j("datastruct3", 		"letfun map (f : int -> $int) = " ^ 
						"fn M:((int*int)*(int*int)) => ((f (#1 (#1 M)), f (#2 (#1 M))), (f (#1 (#2 M)), f (#2 (#2 M)))) in " ^ 
						"map (fn x:int => next{prev{hold (x*x)}+4}) ((1, 2), (3, 4))",SAME),
j("datastruct4", 		"letfun map (f : int -> $int) = " ^ 
						"fn ((M1,M2),(M3,M4)):((int*int)*(int*int)) => ((f M1, f M2), (f M3, f M4)) in " ^ 
						"map (fn x:int => next{prev{hold (x*x)}+4}) ((1, 2), (3, 4))",SAME),
j("datastruct5", 		"letfun map (f : int -> $int) = " ^ 
						"fn (M1,M2,M3,M4):(int*int*int*int) => (f M1, f M2, f M3, f M4) in " ^ 
						"map (fn x:int => next{prev{hold (x*x)}+4}) (1, 2, 3, 4)",SAME),
k("roll1",				"roll (int) 5",SAME),
k("roll2",				"roll (int * bool) (234, true)",SAME),
k("unroll1",			"#1 (unroll (roll (int * bool) (234, true)))",ansI 234),
k("emptyList",			"let empty = roll (unit + (int * 0)) (inl (int * (mu unit + int * 0)) ()) in empty",SAME),
k("makeList",			"let empty = roll (unit + (int * 0)) (inl (int * (mu unit + int * 0)) ()) in " ^
						"letfun cons (ht : int * (mu unit + int * 0)) = roll (unit + (int * 0)) (inr unit ht) in "^
						"cons (5, cons (3, empty))",SAME),
k("fact",				"letrec fact (n : int) : int = if n <= 0 then 1 else n * fact (n-1) in fact 5",ansI 120),
j("sumlist",			"datatype list = Empty | Cons of int * list in " ^
						"letrec sum (l : list) : int = case unroll l of empty => 0 | (h,t) => h + sum t in "^
						"sum (Cons (5, Cons (3, Empty)))",ansI 8),
(*j("renderer",			"fn ((tile,light),(pixel,tex)) : ((int*int)*((int*int)->int))*(($((int*int)->int))*($(int*int))) => "^
						"next{prev{hold(light tile)} * (prev{pixel} prev{tex})}",NONE), *)
j("fastexp",			"letrec exp ((b,e) : $int*int) : $int = if e == 0 then next{1} else if (e mod 2) == 0 then "^
						"next{let x = prev{exp (b,e/2)} in x*x} else next{prev{b} * prev{exp (b,e-1)}} in exp (next{3},5)",ansNI 243) ,
j("quickselect",		"datatype list = Empty | Cons of int * list in " ^
						"letrec partition ((p,l) : int*list) : (int*list*list) = "^
							"case unroll l of em => (0,Empty, Empty) | (h,t) => "^
								"let (s,left,right) = partition (p,t) in "^
								"if h<p then (s+1,Cons(h,left),right) else (s,left,Cons(h,right)) in " ^
						"letrec qs ((l,i) : list * $int) : $int = "^
							"case unroll l of "^
							  "em => next {0} "^
							"| (h,t) => let (n,left,right) = partition (h,t) in "^
								"next { let n = prev {hold n} in "^
									"if prev{i} < n then prev{qs (left,i)} " ^
									"else if prev{i} == n then prev{hold h} " ^
									"else prev{qs (right,next{(prev{i}-n)-1})}} " ^
						"in let c = Cons in qs (c(8,c(2,c(3,c(7,c(4,c(5,Empty)))))), next{2})", ansNI 4),
j("prefixtree",
	"   datatype string = EmptyS | ConsS of bool * string in " ^
	"   lettype2 string2 = mu unit + bool * 0 in " ^
	"   datatype list = EmptyL | ConsL of string * list in " ^
	"   letrec partition (l : list) : (bool * list * list) = " ^
		"   case unroll l of " ^ 
		"   em => (false,EmptyL,EmptyL) " ^
		"   | (s,ss) => " ^
			"   let (anyEmpty,ts,fs) = partition ss in " ^
			"   case unroll s of " ^
			"     em => (true,ts,fs) " ^
			"   | (c,cs) => if c then (anyEmpty, ConsL(cs,ts), fs) else (anyEmpty, ts, ConsL(cs,fs)) " ^ 
	"   in " ^
	"   letrec exists ((l,s) : list * $string2) : $bool = " ^
		"   let (anyEmpty, ts, fs) = partition l in " ^
		"   next { " ^
			"   case unroll prev{s} of " ^
			"     em => prev{if anyEmpty then next{true} else next{false}} " ^
			"   | (c,cs) => " ^
				"   if c  " ^
				"   then prev{exists (ts,next{cs})}  " ^
				"   else prev{exists (fs,next{cs})} " ^ 
		"   } " ^
	"   in 4", SAME)
]

fun pad s n = concat (s :: List.tabulate (n-(String.size s), fn _ => " "))
				
fun testProgram verbose name p t = 
	let
		val _ = Variable.reset ()
		val emit = if verbose >= 1 then print else (fn _ => ())
		val debug = if verbose >= 2 then print else (fn _ => ())
	(*	fun printTerm e = (((PrettyPrinter.printTerm debug) o (PrettyPrinter.resolvePrioTerm 3) o PrintPSF.convertTerm) e; debug "\n") *)
		fun printTerm e = (PrettyPrinter.printExp debug e; debug "\n")
		
		(* Prologue *)
		val _ = (emit "Starting test: "; emit (pad name 24); emit " ...")
		
		(* Parsing *)
		val parsed = L12Parser.parseString p
		
		(* Stage Propegating *)
		val propegated = PropStage.prop1 parsed
	
		(* Printing Input *)
		val _ = debug "\n\n";
		val _ = printTerm (PrintPSF.convertStage1 propegated);
		val _ = debug "~~~~~~~~~~~\n";
		
		(* Typechecking *)
		val _ = Typecheck12.typeCheck1 Contexts.empty propegated
		
		(* Splitting *)
		val (split1, (l,split2)) = StageSplit.coerce1 (StageSplit.stageSplit1 propegated)
		
		(* Printing Split Results *)
		val _ = printTerm (PrintPSF.convertPSF split1);
		val _ = debug "---\n";
		val _ = debug (PrintPSF.pat2string (PrintPSF.convertSourcePattern l));
		val _ = debug ".\n";
		val _ = printTerm (PrintPSF.convertPSF split2);
		val _ = debug "~~~~~~~~~~~\n";
		
		(* Erasure Semantics *)
		val valErasure = ErasureSemantics.eval1 Contexts.empty propegated
				
		(* Diagonal Semantics *)
		val (xiDiag, v1Diag) = DiagonalSemantics.eval1 empty propegated
		val (v1DiagC, diagBody) = Comp.splitDiagValue1 v1Diag
		val diagResidual = xiDiag diagBody
		val v2Diag = DiagonalSemantics.eval2 empty diagResidual
		val v2DiagC = Comp.convertDiagValue2 v2Diag
		
		(* Printing Diagonal Semantics *)
		val _ = printTerm ` PrintPSF.convertDiag diagResidual
		val _ = debug "~~~~~~~~~~~\n";
		
		(* Evaluating Split Part *)
		val (PSFSemantics.V (VFtuple [v1Split,pSplit])) = PSFSemantics.evaluate Contexts.empty split1
		val v2Split = PSFSemantics.evaluate (PSFSemantics.extendPattern Contexts.empty l pSplit) split2
		
		
		(* Comparing *)
		fun triComp a b c = (Comp.valueEq a b, Comp.valueEq a c, Comp.valueEq b c)
		fun triCompAgainst v a b c = (Comp.valueEq v a, Comp.valueEq v b, Comp.valueEq v c)
		val results = 
		case t of
		  NONE => []
		| SAME => let
			val (v1Eras,v2Eras) = Comp.splitErasureValue1 valErasure
			val (ed1, es1, ds1) = triComp v1Eras v1DiagC v1Split
			val (ed2, es2, ds2) = triComp v2Eras v2DiagC v2Split
			in [ed1, es1, ds1, ed2, es2, ds2] end
		| EXACT v => let
			val (v1Eras,v2Eras) = Comp.splitErasureValue1 valErasure
			val (v1Suplied,v2Suplied) = Comp.splitErasureValue1 v
			val (ae1, ad1, as1) = triCompAgainst v1Suplied v1Eras v1DiagC v1Split
			val (ae2, ad2, as2) = triCompAgainst v2Suplied v2Eras v2DiagC v2Split
			in [ae1, ad1, as1, ae2, ad2, as2] end
		
		(* Epilogue *)
		fun printTestResult b = emit (if b then "P" else "F")
		val _ = if List.all (fn b=>b) results 
				then (debug "\n"; emit (case results of [] => "no tests!\n" | _ => "all pass!\n"); debug "\n") 
				else (emit "SOME FAILED: "; List.app printTestResult results; emit "\n")
	in
	(*	printTerm (PrintPSF.convertDiag rDiag);
		print "\n~~~~~~~~~~~\n";*)
		debug "====================\n\n";
		()
	end


fun runtests v = List.app (fn (name,prog,t) => testProgram v name prog t) (List.concat programs)

(* 
val compiled = CM.make "sources.cm"; if compiled then Examples.runtests 1 else ();
*)
end
