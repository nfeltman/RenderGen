
module Geometry where
import MbVectors
import MbUtilities

type Tri3 = X3 Vec3
type Seg3 = (Vec3, Vec3, Interval)
type Plane3 = (Vec3, Vec3)
type BBox3 = (Vec3, Vec3)

triToPlane :: Tri3 -> Plane3
triToPlane (X3 p1 p2 p3) = (p1, cross (vsub p2 p1) (vsub p3 p1))

intersectSegPlane :: Seg3 -> Plane3 -> Maybe R
intersectSegPlane (o,d,i) (p,n) = 
    let denom = dot d n in
    if denom == 0.0 then Nothing else
        let t = (dot n (vsub p o)) / denom in
        if contains i t then Just t else Nothing

sideTest :: Vec3 -> Vec3 -> Vec3 -> Bool
sideTest d v1 v2 = (>0.0).(dot d).(cross v1) $ (vsub v2 v1)

lineTest :: Vec3 -> Tri3 -> Bool
lineTest d tri = let X3 a b c = (cycleX3 (\ v1 v2 _ -> sideTest d v1 v2)) $ tri in a==b && b == c 
        
intersectSegTri :: Seg3 -> Tri3 -> Maybe R
intersectSegTri seg@(o,d,_) tri = 
    do  t <- intersectSegPlane seg (triToPlane tri)
        if lineTest d (xzip3 vsub tri (X3 o o o)) then Just t else Nothing

