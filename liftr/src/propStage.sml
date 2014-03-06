
structure PropStage = 
struct

open LangCommon
open Lambda12c
open Lambda12

exception StagePropException

fun propTy1 (Tstandard t) = T1 (mapType propTy1 t)
  | propTy1 (Tfut t) = T1fut (propTy2 t)
  
and propTy2 (Tstandard t) = T2 (mapType propTy2 t)
  | propTy2 (Tfut t) = raise StagePropException

and prop1 (Estandard exp) = E1 (mapExpr prop1 propTy1 exp)
  | prop1 (Eprev _) = raise StagePropException
  | prop1 (Enext e) = E1next (prop2 e)
  | prop1 (Ehold e) = E1hold (prop1 e)

and prop2 (Estandard exp) = E2 (mapExpr prop2 propTy2 exp)
  | prop2 (Eprev e) = E2prev (prop1 e)
  | prop2 (Enext _) = raise StagePropException
  | prop2 (Ehold _) = raise StagePropException
  
(*
fun propProgram p = 
	let		
		fun propFunc (Lambda12c.FuncDec1 (f,t1,t2,v,e)) = 
				Lambda12.FuncDec1 (f, propTy1 t1, propTy1 t2, v, prop1 e)
	(*	  | propFunc (Lambda12c.FuncDec2 (f,t1,t2,v,e)) = 
				Lambda12.FuncDec2 (f, propTy2 t1, propTy2 t2, v, prop2 e) *)
	in
		map propFunc p
	end *)
	
end
