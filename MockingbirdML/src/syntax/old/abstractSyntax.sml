
functor FixedPointSyntax (B : SyntaxBlock) = 
struct
	datatype recSyntax  = Comp of recSyntax B.block
                        | Fix of string * recSyntax
                        | Label of string
    
	exception RuntimeLabelError
	datatype 'a context = Cons of ('a context -> 'a) * ('a context) | Empty	
    
    type 'z t_subbuild = (recSyntax -> 'z context -> 'z) -> recSyntax B.block -> 'z context -> 'z
    
    fun interpret (subBuild : 'z t_subbuild) program =
        let 
            fun build labels (Comp b) = subBuild (build labels) b
              | build labels (Fix (lab, e)) =
                    let
                        val f = build (lab::labels) e
                    in
                        fn c => f (Cons (f,c))
                    end
              | build labels (Label lab) = 
                    let
                        fun findIndex (l::tail) = 
                            if l = lab then 0
                            else 1 + findIndex tail
                          | findIndex [] = raise RuntimeLabelError
                        val index = findIndex labels				
                        fun dropN 0 (ctx as Cons (f,_)) = f ctx
                          | dropN n (Cons (_,tail)) = dropN (n-1) tail
                          | dropN _ Empty = raise RuntimeLabelError
                    in
                        fn c => dropN index c
                    end
        in
            build [] program Empty
        end
end
