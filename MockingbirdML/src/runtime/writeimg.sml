
structure WriteIMG =
struct
	exception SizeMismatch
	
	fun toMatrixString m n vals = 
		let
			val len = m * n
			fun mapToString c (f::fs) = 
				if c = len then [(Real.toString f) ^ "];"]
				else if c mod m = 0 then 
					((Real.toString f) ^ ";")::(mapToString (c+1) fs)
				else 
					((Real.toString f) ^ ",")::(mapToString (c+1) fs)
			  | mapToString _ _ = []
		in
			if len = List.length vals
			then "["::mapToString 1 vals
			else raise SizeMismatch
		end
		
	fun writeString f s = 
		let
			val out = TextIO.openOut f;
		in
			(TextIO.output (out,s); TextIO.closeOut out)
		end
	
	fun writeStringList f xs = 
		let
			val out = TextIO.openOut f;
		in
			(map (fn x => TextIO.output (out,x)) xs; TextIO.closeOut out)
		end
			
end
