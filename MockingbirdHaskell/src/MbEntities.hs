{-# LANGUAGE DeriveFunctor #-}

module MbEntities where
import MbUtilities
import MbVectors
import MbKeys
import Geometry
import Data.List
import Data.Monoid

type Geom m = (Tri3,m)
type GS m = ([Geom m], KeyedList Seg3)
data Depthed t = Depthed (Maybe (Float, t)) deriving Functor
type Hit m = Depthed (Geom m)
type HitList m = KeyedList (Hit m)
type FragList t = KeyedList (Depthed t)

data Shaders m t = Shaders (Geom m -> t)


instance Monoid (Depthed a) where
    mempty = Depthed Nothing
    mappend a (Depthed Nothing) = a
    mappend (Depthed Nothing) b = b
    mappend a@(Depthed (Just(da,_))) b@(Depthed (Just(db,_))) = if da < db then a else b


hitDyn :: GS m -> (Int, Hit m)
hitDyn (geom@(tri,_):[], KeyedList ((k,samp):[])) = (k, Depthed $ fmap (\d -> (d,geom)) $ intersectSegTri samp tri )
hitDyn (_,_) = error "Hit node requires single sample and single geometry."

shadeDyn :: Shaders m t -> HitList m -> (Int, Depthed t)
shadeDyn (Shaders shadeFunc) (KeyedList ((k,Depthed h):[])) = (k, Depthed $ fmap (\(dep,g) -> (dep, shadeFunc g)) h)
shadeDyn _ _ = error "Shade node requires a single hit."

buildMatrix :: Int -> Int -> [(Int, Float, Float)]
buildMatrix w h = 
    do  y <- [0..(h-1)]
        x <- [0..(w-1)] 
        let fi = fromIntegral in [(x+y*w, (fi x)/(fi w-1)*2-1, (fi (h-1-y))/(fi h-1)*2-1)]

generateSamples :: R -> R -> Int -> Int -> KeyedList Seg3
generateSamples fovX aspectRatio pw ph = 
    let w = tan(fovX/2.0) in
    let h = w / aspectRatio in
    let m = buildMatrix pw ph in
    KeyedList [(k, (vec3 0.0 0.0 0.0, normalize (vec3 (x*w) (y*h) 1), positives)) | (k, x, y) <- m]

                
