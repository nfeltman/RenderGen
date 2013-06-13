
structure Util  = 
struct
    fun leftSection a b = (a,b)
    fun rightSection a b = (b,a)
	fun mapReduce f r (h,t) = foldl (fn (n,p) => r (f n) p) (f h) t
	fun find [] _ = NONE
      | find ((key,value)::rest) lab = if lab = key then SOME value else find rest lab
end



