
module MbVectors where
import MbUtilities

type R = Float

-- VECTORS

type Vec3 = X3 R

vec3 :: Float -> Float -> Float -> Vec3
vec3 x y z = X3 x y z

vneg :: Vec3 -> Vec3
vneg = fmap (0-)

vadd :: Vec3 -> Vec3 -> Vec3
vadd = xzip3 (+)

vsub :: Vec3 -> Vec3 -> Vec3
vsub = xzip3 (-)

vmul :: Vec3 -> R -> Vec3
vmul v c = fmap (*c) v

vdiv :: Vec3 -> R -> Vec3
vdiv v c = fmap (/c) v

dot :: Vec3 -> Vec3 -> R
dot (X3 a b c) (X3 x y z) = a*x + b*y + c*z

normSq :: Vec3 -> R
normSq v = dot v v

norm :: Vec3 -> R
norm = sqrt.normSq

normalize :: Vec3 -> Vec3
normalize v = vdiv v (norm v)

cross :: Vec3 -> Vec3 -> Vec3
cross (X3 a b c) (X3 x y z) = X3 (b*z-c*y) (c*x-z*a) (a*y-b*x)

-- INTERVALS

type Interval = (R,R)
data EInterval = Empty | NonEmpty Interval

contains :: Interval -> R -> Bool
contains (s,e) r = s <= r && r <= e

positives :: Interval
positives = (0.0, infF)