
structure OBJReader =
struct
	type fileinfo = {numVerts : int, numFaces : int}
	
	fun readObj loc = 
		let 
			val ins = TextIO.openIn loc
			val rF = Real.fromString
			val rI = Int.fromString
			fun collect vs fs = 
				case TextIO.inputLine ins of
				  NONE => (vs,fs)
				| SOME(s) => 
					case String.tokens (fn t => t = #" ") s of
					  ["v",x,y,z] => (
						case (rF x, rF y, rF z) of
						  (SOME(xf), SOME(yf), SOME(zf)) => collect ((xf,yf,zf)::vs) fs
						| _ => (print ("bad vertex: "^s^"\n"); collect vs fs) )
					| ["f",x,y,z] => (
						case (rI x, rI y, rI z) of
						  (SOME(i1), SOME(i2), SOME(i3)) => collect vs ((i1,i2,i3)::fs)
						| _ => (print ("bad face: "^s^"\n"); collect vs fs) )
					| [] => collect vs fs
					| _ => (print ("unrecognized: "^s^"\n"); collect vs fs)
			
			val (vs,fs) = collect [] []
			val verteces = Vector.fromList vs
			val n = Vector.length verteces
			fun lookup i = Vector.sub(verteces,n-i)
		in
			List.map (fn (v1,v2,v3) => (lookup v1, lookup v2, lookup v3)) fs
		end
end
