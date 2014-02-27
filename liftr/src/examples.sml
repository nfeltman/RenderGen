
structure Examples = 
struct

open Lambda12c
open LangCommon

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
		val emit = if verbose >= 1 then print else (fn _ => ())
		val debug = if verbose >= 2 then print else (fn _ => ())
		val _ = (emit "Starting test: "; emit (pad name 24); emit " ...")
		val propegated = PropStage.prop1 p
		val valErasure = ErasureSemantics.eval1 empty propegated
		val (v1Diag, rDiag) = DiagonalSemantics.eval1 empty propegated
		val v2Diag = DiagonalSemantics.eval2 empty rDiag
		val _ = Typecheck12.typeCheck1 empty propegated
		val (split1, _, (l,split2), _) = StageSplit.stageSplit1 empty propegated
		val (PSFSemantics.Vtuple [v1Split,pSplit]) = PSFSemantics.evaluate empty split1
		val v2Split = PSFSemantics.evaluate (extendContext empty l pSplit) split2
		val _ = (emit "all pass!\n")
	in
		()
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
