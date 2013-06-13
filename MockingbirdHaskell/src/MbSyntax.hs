
module MbSyntax where
import MbEntities
import MbUtilities
import MbKeys
import Data.Monoid

data DecompGS = OneG | TwoGP | OneS | SixteenSqSP
data DecompH = OneH

data GeomBound = SingleG | BBboxG
data SampleBound = SingleS | BBboxS
data MergeImp = General | Union

data GS2GS = Filt | ChainGGG GS2GS GS2GS
-- data H2H
-- data F2F

data GS2H = MergeH MergeImp DecompGS GS2H | ChainGGH GS2GS GS2H | HitNode
data H2F = MergeF1 MergeImp DecompH H2F | Shade
data GS2F = MergeF2 MergeImp DecompGS GS2F | ChainGGF GS2GS GS2F | ChainGHF GS2H H2F


-- DECOMPOSITION

evalDecompGS :: DecompGS -> GS m -> [GS m]
evalDecompGS OneG (g,s) = map (\e -> ([e],s)) g
evalDecompGS TwoGP (g,s) = undefined
evalDecompGS OneS (g, KeyedList s) = map (\e -> (g, KeyedList [e])) s
evalDecompGS SixteenSqSP (g,s) = undefined

evalDecompH :: DecompH -> HitList m -> [HitList m]
evalDecompH OneH (KeyedList l) = map ((KeyedList).(:[])) l

-- MERGING

evalMerge :: MergeImp -> [KeyedList (Depthed t)] -> KeyedList (Depthed t)
evalMerge General = mergeByKey
evalMerge Union = mergeDisjoint


-- HELPERS

chainHelp :: (a -> Maybe b) -> (a -> c) -> (b -> c) -> a -> c
chainHelp f l r a = maybe (l a) r (f a)

allMiss :: KeyedList t -> KeyedList (Depthed u)
allMiss = fmap (\_ -> mempty)

-- ALL EVALUATORS

evalGS2GS :: GS2GS -> GS m -> Maybe (GS m)
evalGS2GS Filt gst = undefined -- if f gst then Just gst else Nothing
evalGS2GS (ChainGGG lhs rhs) gst = evalGS2GS lhs gst >>= (evalGS2GS rhs) 

evalGS2H :: GS2H -> GS m -> HitList m
evalGS2H (MergeH m decomp rhs) = (evalMerge m).(map $ evalGS2H rhs).(evalDecompGS decomp)
evalGS2H (ChainGGH lhs rhs) = chainHelp (evalGS2GS lhs) (allMiss.snd) (evalGS2H rhs) 
evalGS2H HitNode = KeyedList.(:[]).hitDyn

evalH2F :: Shaders m t -> H2F -> HitList m -> FragList t
evalH2F s (MergeF1 mode decomp rhs) = (evalMerge mode).(map $ evalH2F s rhs).(evalDecompH decomp)
evalH2F s Shade = KeyedList.(:[]).(shadeDyn s)

evalGS2F :: Shaders m t -> GS2F -> GS m -> FragList t
evalGS2F s (MergeF2 m decomp rhs) = (evalMerge m).(map $ evalGS2F s rhs).(evalDecompGS decomp)
evalGS2F s (ChainGGF lhs rhs) = chainHelp (evalGS2GS lhs) (allMiss.snd) (evalGS2F s rhs)
evalGS2F s (ChainGHF lhs rhs) = (evalH2F s rhs).(evalGS2H lhs)