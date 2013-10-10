
structure StageSplit = 
struct

open LangCommon
open LambdaPSF
				
fun split1 gamma exp = 
	case exp of 
	  E1var (v) =>
	| E1lam (v,t,e) =>
	| E1app (e1,e2) => 
	| E1unit => 
	| E1tuple (e1,e2) => 
	| E1case (e1,(v2,e2),(v3,e3)) => 
	| E1op1 (op, e) =>
	| E1next e => 
	
fun split2 gamma exp = 
	case exp of 
	  E2var (v) =>
	| E2lam (v,t,e) =>
	| E2app (e1,e2) => 
	| E2unit => 
	| E2tuple (e1,e2) => 
	| E2case (e1,(v2,e2),(v3,e3)) => 
	| E2op1 (op, e) =>
	| E2prev e => 
	| E2save e => 
	
end
