
structure PrintPSF = 
struct

open LangCommon
open Prims
open PrettyPrinter

fun opToString bo = (
		case bo of 
		  O2plus => "+"
		| O2minus => "-"
		| O2times => "*"
		| O2div => "/"
		| O2mod => "mod"
		| O2less => "<"
		| O2greater => ">"
		| O2equal => "=="
		| O2lesseq => "<="
		| O2greatereq => ">="
		| O2and => "and"
		| O2or => "or"
		| O2cat => "^")

structure V = ValuesBase
structure S = TypesBase
fun convertSourceTypes convert ty = 
		case ty of
		  S.TFprim Prims.Tint => Eatom "int"
		| S.TFprim Prims.Tbool => Eatom "bool"
		| S.TFprim Prims.Tstr => Eatom "string"
		| S.TFvar i => Eatom (Int.toString i) 
		| S.TFrec t => EprimApp ("mu", convert t)
		| S.TFprod [] => Eatom ("unit")
		| S.TFprod ts => Einfix ("*", map convert ts)
		| S.TFsum ts => Einfix ("+", map convert ts)
		| S.TFarr (t1, t2) => Einfix ("->", [convert t1, convert t2])
		
structure S = SourceLang
fun convertSourcePatternv (Gnum,Gname) (S.Pvar x) = 
		let
			val (_,desiredName) = x
			val (n, s) =
				let	
					val n = Contexts.lookup Gnum desiredName
				in
					(n+1, desiredName^"_"^(Int.toString n))
				end
				handle Contexts.UnboundVar _ => (0,desiredName)
		in
			(Pvar s, (Contexts.extendContext Gnum desiredName n, Contexts.extendContext Gname x s))
		end
  | convertSourcePatternv G (S.Ptuple xs) = 
		let
			fun f (x,(Gin,ps)) = let val (p,Gout) = convertSourcePatternv Gin x in (Gout,p::ps) end
			val (Gfinal,ps) = foldr f (G,[]) xs
		in
			(Ptuple ps, Gfinal)
		end
fun convertSource (Gnum,Gname) convertRec convertTy ex = 
	let
		val convert = convertRec (Gnum,Gname)
		fun convertBranch (x,e) = 
			let val (p, G2) = convertSourcePatternv (Gnum,Gname) x 
			in (p,convertRec G2 e) end
	in
		case ex of 
		  S.Fvar v => Eatom (Contexts.lookup Gname v)
		| S.FprimVal (Prims.Vint i) => Eatom (Int.toString i)
		| S.FprimVal (Prims.Vbool b) => Eatom (if b then "true" else "false")
		| S.FprimVal (Prims.Vstr s) => Eatom ("\""^s^"\"")
		| S.Flam (t,b) => Elam (convertTy t, convertBranch b)
		| S.Fapp (e1,e2) => Eapp (convert e1, convert e2)
		| S.Ftuple es => Etuple (map convert es)
		| S.Fpi (i, e) => EprimApp ("#" ^ (Int.toString (i+1)), convert e)
		| S.Finj (ts, us, e) => Eapp (EprimApp("inj", Einfix ("+", (map convertTy ts) @ (Eatom "#" :: map convertTy us))), convert e)
		| S.Fcase (e,bs) => Ecase (convert e, map convertBranch bs)
		| S.Fif (e1,e2,e3) => Eif (convert e1, convert e2, convert e3)
		| S.Flet (e, b) => Elet (convert e, convertBranch b)
		| S.Fbinop (bo, e1, e2) => Einfix (opToString bo, [convert e1, convert e2])
		| S.Froll (t,e) => Eapp (EprimApp ("roll", convertTy t), convert e)
		| S.Funroll e => EprimApp ("unroll", convert e)
		| S.Ferror t => EprimApp ("error", convertTy t)
	end
	
structure S = Lambda12
fun convertTyStage1 (S.T1 t) = convertSourceTypes convertTyStage1 t
  | convertTyStage1 (S.T1fut t) = EprimApp("$", convertTyStage2 t)
and convertTyStage2 (S.T2 t) = convertSourceTypes convertTyStage2 t
fun convertStage1v G (S.E1 e) = convertSource G convertStage1v convertTyStage1 e
  | convertStage1v G (S.E1mono e) = EbraceApp("mono", convertStageMv G e)
  | convertStage1v G (S.E1next e) = EbraceApp("next", convertStage2v G e)
  | convertStage1v G (S.E1hold e) = EprimApp("holdInt", convertStage1v G e)
and convertStage2v G (S.E2 e) = convertSource G convertStage2v convertTyStage2 e
  | convertStage2v G (S.E2prev e) = EbraceApp("prev", convertStage1v G e)
and convertStageMv G (S.EM e) = convertSource G convertStageMv convertTyStage2 e
  
fun convertDiagv G (DiagonalSemantics.E e) = convertSource G convertDiagv (fn _ => Eatom "_") e
fun convertPSFv G (LambdaPSF.E e) = convertSource G convertPSFv (fn () => Eatom "_") e
  | convertPSFv G (LambdaPSF.Edummy) = Eatom "dummy"
  
fun convertPSFVal (PSFSemantics.V exp) = (
		case exp of
		  V.VFprim (Prims.Vint i) => Eatom (Int.toString i)
		| V.VFprim (Prims.Vbool b) => Eatom (if b then "true" else "false")
		| V.VFprim (Prims.Vstr s) => Eatom ("\""^s^"\"")
		| V.VFroll e => EprimApp ("roll", convertPSFVal e)
		| V.VFtuple es => Etuple (map convertPSFVal es)
		| V.VFinj (i,e) => Eapp (EprimApp ("inj", Eatom (Int.toString i)), convertPSFVal e)
		| V.VFlam _ => Eatom "[FUNC]"
		)
  | convertPSFVal (PSFSemantics.Vdummy) = Eatom "dummy"

fun convertPSFBranch (x,e) = 
	let 
		val (p, G) = convertSourcePatternv (Contexts.empty, Contexts.empty) x 
	in
		(p,convertPSFv G e) 
	end

val convertStage1 = convertStage1v (Contexts.empty, Contexts.empty)
val convertStage2 = convertStage2v (Contexts.empty, Contexts.empty)
val convertDiag = convertDiagv (Contexts.empty, Contexts.empty)
val convertPSF = convertPSFv (Contexts.empty, Contexts.empty)

end
