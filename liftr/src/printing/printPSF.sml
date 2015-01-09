
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
structure NumDict = ListDict (type var = string)
fun convertSourcePatternv _ (Gnum,Gname) (S.Pvar x) = 
		let
			val (_,desiredName) = x
			val (n, s) =
				let	
					val n = NumDict.lookup Gnum desiredName
				in
					(n+1, desiredName^"_"^(Int.toString n))
				end
				handle NumDict.UnboundVar _ => (0,desiredName)
		in
			(Pvar s, (NumDict.extend Gnum desiredName n, MainDict.extend Gname x s))
		end
  | convertSourcePatternv convRec G (S.Ptuple xs) = 
		let
			fun f (x,(Gin,ps)) = let val (p,Gout) = convRec Gin x in (Gout,p::ps) end
			val (Gfinal,ps) = foldr f (G,[]) xs
		in
			(Ptuple ps, Gfinal)
		end
  | convertSourcePatternv _ G S.Punused = (Pvar "_", G)
fun convertSource (Gnum,Gname) convertRec convertTy convertPatt ex = 
	let
		val convert = convertRec (Gnum,Gname)
		fun convertBranch (x,e) = 
			let val (p, G2) = convertPatt (Gnum,Gname) x 
			in (p,convertRec G2 e) end
	in
		case ex of 
		  S.Fvar v => Eatom (MainDict.lookup Gname v)
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
fun convertPatternM G (S.PM p) = convertSourcePatternv convertPatternM G p
fun convertPattern12 G (S.P p) = convertSourcePatternv convertPattern12 G p
  | convertPattern12 G (S.Pmono p) = let val (p,G) = convertPatternM G p in (Pbrace ("mono", p), G) end
  | convertPattern12 G (S.Pnext p) = let val (p,G) = convertPatternM G p in (Pbrace ("next", p), G) end
fun convertTyStage1 (S.T1 t) = convertSourceTypes convertTyStage1 t
  | convertTyStage1 (S.T1fut t) = EprimApp("$", convertTyStage2 t)
  | convertTyStage1 (S.T1now t) = EprimApp("^", convertTyStage2 t)
and convertTyStage2 (S.T2 t) = convertSourceTypes convertTyStage2 t
fun convertStage1v G (S.E1 e) = convertSource G convertStage1v convertTyStage1 convertPattern12 e
  | convertStage1v G (S.E1mono e) = EbraceApp("mono", convertStageMv G e)
  | convertStage1v G (S.E1next e) = EbraceApp("next", convertStage2v G e)
  | convertStage1v G (S.E1hold e) = EprimApp("holdInt", convertStage1v G e)
  | convertStage1v G (S.E1pushPrim e) = EprimApp("push", convertStage1v G e)
  | convertStage1v G (S.E1pushProd e) = EprimApp("pushP", convertStage1v G e)
  | convertStage1v G (S.E1pushSum e) = EprimApp("pushS", convertStage1v G e)
  | convertStage1v G (S.E1pushArr e) = EprimApp("pushA", convertStage1v G e)
and convertStage2v G (S.E2 e) = convertSource G convertStage2v convertTyStage2 convertPatternM e
  | convertStage2v G (S.E2prev e) = EbraceApp("prev", convertStage1v G e)
and convertStageMv G (S.EM e) = convertSource G convertStageMv convertTyStage2 convertPatternM e
  
fun convertDiagv G (DiagonalSemantics.E e) = convertSource G convertDiagv (fn _ => Eatom "_") convertPatternM e

fun convertPatternPSF G (LambdaPSF.P p) = convertSourcePatternv convertPatternPSF G p
fun convertPSFv G (LambdaPSF.E e) = convertSource G convertPSFv (fn () => Eatom "_") convertPatternPSF e
  
fun convertPSFVal (PSFSemantics.V exp) =
		case exp of
		  V.VFprim (Prims.Vint i) => Eatom (Int.toString i)
		| V.VFprim (Prims.Vbool b) => Eatom (if b then "true" else "false")
		| V.VFprim (Prims.Vstr s) => Eatom ("\""^s^"\"")
		| V.VFroll e => EprimApp ("roll", convertPSFVal e)
		| V.VFtuple es => Etuple (map convertPSFVal es)
		| V.VFinj (i,e) => Eapp (EprimApp ("inj", Eatom (Int.toString i)), convertPSFVal e)
		| V.VFlam _ => Eatom "[FUNC]"

fun convertPSFBranch (x,e) = 
	let 
		val (p, G) = convertPatternPSF (NumDict.empty, MainDict.empty) x 
	in
		(p,convertPSFv G e) 
	end

val convertStage1 = convertStage1v (NumDict.empty, MainDict.empty)
val convertStage2 = convertStage2v (NumDict.empty, MainDict.empty)
val convertDiag = convertDiagv (NumDict.empty, MainDict.empty)
val convertPSF = convertPSFv (NumDict.empty, MainDict.empty)

end
