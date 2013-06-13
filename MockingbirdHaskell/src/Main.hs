module Main where
import MbSyntax
import MbEntities
import MbVectors
import MbKeys
import MbUtilities
import Geometry
import Data.List
import Data.Vector.Storable
import Codec.Picture
import Codec.Picture.Png
import qualified Data.ByteString as B


fragsmentsToImage :: Int -> Int -> FragList t -> Image PixelRGB8
fragsmentsToImage x y (KeyedList frags) = (Image x y).fromList $ frags >>= \(_,Depthed v) -> maybe [0, 0, 0] (\_->[255, 255, 255]) v

--fragmentsToASCII :: Int -> Int -> [Frag] -> String
--fragmentsToASCII x y = (concat $ (map $ (++"\n").(map $ maybe '.' (\_->'X')) )).(arrangeGrid x y)

pixX = 300
pixY = 250
oneTri = X3 (vec3 0 1 1) (vec3 1 0 1) (vec3 0 0 1)
rays = generateSamples (pi*0.66) 1.2 pixX pixY 
tree = (MergeF2 General OneG (MergeF2 Union OneS (ChainGHF HitNode Shade)))
frags = evalGS2F (Shaders (\_ -> ())) tree ([(oneTri,())],rays)

main::IO()
main = B.writeFile "output.png" $ encodePng $ fragsmentsToImage pixX pixY frags