
structure PropStage = 
struct

local
open LangCommon
open Lambda12c
open Lambda12
open SourceLang
in

exception StagePropException

fun id x = x

fun propTy1 (Tstandard t) = T1 (mapType propTy1 t)
  | propTy1 (Tfut t) = T1fut (propTy2 t)
  
and propTy2 (Tstandard t) = T2 (mapType propTy2 t)
  | propTy2 (Tfut t) = raise StagePropException

and prop1r G (Estandard exp) = E1 (replaceVars prop1r G Variable.newvar (mapExpr id propTy1 exp))
  | prop1r G (Eprev _) = raise StagePropException
  | prop1r G (Enext e) = E1next (prop2r G e)
  | prop1r G (Ehold e) = E1hold (prop1r G e)

and prop2r G (Estandard exp) = E2 (replaceVars prop2r G Variable.newvar (mapExpr id propTy2 exp))
  | prop2r G (Eprev e) = E2prev (prop1r G e)
  | prop2r G (Enext _) = raise StagePropException
  | prop2r G (Ehold _) = raise StagePropException
  
val prop1 = prop1r empty
val prop2 = prop2r empty
  
end
	
end
