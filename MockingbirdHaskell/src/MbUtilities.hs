 {-# LANGUAGE DeriveFunctor #-}

module MbUtilities where


infF :: Float
infF = 1.0/0.0

angryZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
angryZipWith f [] [] = []
angryZipWith f [] (_:_) = error "First list shorter than second."
angryZipWith f (_:_) [] = error "Second list shorter than first."
angryZipWith f (x:xs) (y:ys) = (f x y):(angryZipWith f xs ys)

-- X3 stuff

data X3 t = X3 !t !t !t deriving (Functor, Show)

cycleX3 :: (a -> a -> a-> b) -> X3 a -> X3 b
cycleX3 f (X3 x y z) =  X3 (f x y z) (f y z x) (f z x y)

xzip3 :: (a -> b -> c) -> X3 a -> X3 b -> X3 c
xzip3 f (X3 x y z) (X3 a b c) = X3 (f x a) (f y b) (f z c)

x3ToList :: X3 a -> [a]
x3ToList (X3 a b c) = [a,b,c]