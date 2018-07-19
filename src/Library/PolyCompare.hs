{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude #-}

module Library.PolyCompare 
(
    (<<=),
    (<<),
    (>>)
)
where

import Algebra.Prelude                  hiding ((>>),(>>=))
import qualified Data.Map.Strict        as M

(<<=) :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Ord k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n -> Bool
pol1 <<= pol2 = foldl1 (\acc bol -> acc && bol) bools
        where
        termsP1 = reverse $ M.toList $ _terms pol1
        termsP2 = reverse $ M.toList $ _terms pol2
        bools = zipWith (\x y -> x <= y) termsP1 termsP2

compareLists :: (IsOrder n ord, Ord k)
              =>[(OrderedMonomial ord n, k)] -> [(OrderedMonomial ord n, k)] -> Bool
compareLists [] [] = False
compareLists (x:xs) (y:ys)
            |  x /= y = x < y
            | otherwise = compareLists xs ys



(<<) :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Ord k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n -> Bool
pol1 << pol2 = compareLists termsP1 termsP2
                where
                termsP1 = reverse $ M.toList $ _terms pol1
                termsP2 = reverse $ M.toList $ _terms pol2
-------------------


(>>) :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Ord k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n -> Bool
pol1 >>  pol2 = not (pol1 <<= pol2)

(>>=) :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Ord k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n -> Bool
pol1 >>=  pol2 = not (pol1 << pol2)
