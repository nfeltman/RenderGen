
structure Examples = 
struct

open Lambda12c
open LangCommon
structure Comp = ValueComparison

val fourPlusSix = Ebinop(Prims.Iplus, Eint 4, Eint 6)
val twoTimesThree = Ebinop(Prims.Itimes, Eint 2, Eint 3)
val if2 = Enext( Eif(
				Ebool true,
				Eprev (Ehold fourPlusSix), 
				twoTimesThree
				))

fun pad s n = concat (s :: List.tabulate (n-(String.size s), fn _ => " "))
				
fun testProgram verbose name p = 
	let
		val printTerm = (PrettyPrinter.printTerm print) o (PrettyPrinter.resolvePrioTerm 3) o PrintPSF.convertTerm
		val _ = Variable.reset ()
		val emit = if verbose >= 1 then print else (fn _ => ())
		val debug = if verbose >= 2 then print else (fn _ => ())
		
		(* Prologue *)
		val _ = (emit "Starting test: "; emit (pad name 24); emit " ...")
		
		(* Checking Input *)
		val propegated = PropStage.prop1 p
		val _ = Typecheck12.typeCheck1 empty propegated
		
		(* Erasure Semantics *)
		val valErasure = ErasureSemantics.eval1 empty propegated
		val (v1Eras,v2Eras) = Comp.splitErasureValue1 valErasure
		
		(* Diagonal Semantics *)
		val (v1Diag, rDiag) = DiagonalSemantics.eval1 empty propegated
		val v2Diag = DiagonalSemantics.eval2 empty rDiag
		val (v1DiagC, v2DiagC) = (Comp.convertDiagValue v1Diag, Comp.convertDiagValue v2Diag)
		
		(* Splitting *)
		val (split1, _, (l,split2), _) = StageSplit.stageSplit1 empty propegated
		val (PSFSemantics.Vtuple [v1Split,pSplit]) = PSFSemantics.evaluate empty split1
		val v2Split = PSFSemantics.evaluate (extendContext empty l pSplit) split2
		
		(* Comparing *)
		fun triComp a b c = (Comp.valueEq a b, Comp.valueEq a c, Comp.valueEq b c)
		val (ed1, es1, ds1) = triComp v1Eras v1DiagC v1Split
		val (ed2, es2, ds2) = triComp v2Eras v2DiagC v2Split
		val results = [ed1, es1, ds1, ed2, es2, ds2]
		
		(* Epilogue *)
		fun printTestResult b = emit (if b then "P" else "F")
		val _ = if List.all (fn b=>b) results 
				then (emit "all pass!\n") 
				else (emit "SOME FAILED: "; List.app printTestResult results; emit "\n")
	in
		() (* printTerm (PrintPSF.convertPSF split2) *)
	end


fun runtests () = 

	let
		val testProgram = testProgram 1
		val _ = testProgram "fourPlusSix" fourPlusSix
		val _ = testProgram "twoTimesThree" twoTimesThree
		val _ = testProgram "if2" if2
	in
		()
	end
end
