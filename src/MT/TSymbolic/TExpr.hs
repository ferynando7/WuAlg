--Some code taken from http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, TypeOperators #-}
{-# LANGUAGE UndecidableInstances, OverlappingInstances, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleContexts #-}

module MT.TSymbolic.TExpr
(
    Expr(..),
    simplify,
    showTermSym,
    evaluate,
    fromString,
    fromExpr,
    toExpr
) where

import qualified Algebra.Prelude  as AP hiding ((++), (+), (-), (*), (^))
import Algebra.Ring.Polynomial.Class
import Prelude hiding (null, filter)
import qualified Numeric.Algebra.Class as N hiding ((+), (-), (^))
import qualified Numeric.Additive.Class as A
import qualified Numeric.Additive.Group as G
import Numeric.Algebra.Unital
import qualified Numeric.Ring.Class as NR
import Numeric.Rig.Class
import Numeric.Decidable.Zero
import Numeric.Algebra.Commutative
import Data.Type.Natural hiding (one)
import qualified Data.Map.Strict as M
import qualified Data.List as L
import GHC.TypeLits
import GHC.Natural
import qualified GHC.Real as GR
import Algebra.Scalar

infixl 5 :/:, :%:

data Expr a =   Expr (M.Map [String] a)
                | (Expr a) :/: (Expr a)
                | (Expr a) :%: (Expr a)
                deriving (Eq)

instance Show (Expr Integer) where
    show (Expr a) = "("++ showSym (M.toList a)++")"
    show (Expr a :/: Expr b) = showSym (M.toList a) ++ "/" ++ showSym (M.toList b)


showSym :: [([String],Integer)] -> String
showSym [] = ""
showSym [(x,c)] = show c ++ showTermSym x
showSym (x:xs) = showSym [x] ++ "+" ++ showSym xs

showTermSym :: [String] -> String
showTermSym [] = ""
showTermSym [x] = x
showTermSym (x:xs) = x ++ "*" ++  showTermSym xs

instance (Ord a) => Ord (Expr a) where
    Expr a `compare` Expr b
        | M.size a < M.size b = LT
        | otherwise = GT


instance (Integral a) => A.Additive (Expr a) where
    Expr a + Expr b = simplify $ suma (Expr a) (Expr b)


instance N.RightModule Natural (Expr Integer) where
    Expr a *. n = Expr $ M.map (* (toInteger n)) a 

instance N.LeftModule Natural (Expr Integer) where
    n .* Expr a = Expr $ M.map (* (toInteger n)) a
         
instance N.RightModule Integer (Expr Integer) where
    Expr a *. n = Expr $ M.map (*n) a        

instance N.LeftModule Integer (Expr Integer) where
    n .* Expr a = Expr $ M.map (*n) a
    
instance N.Monoidal (Expr Integer) where
    zero = Expr M.empty

instance DecidableZero (Expr Integer) where
    isZero (Expr a) | M.null a = True
                    | otherwise = False

instance AP.Ring (Expr Integer) where
    fromInteger a = MT.TSymbolic.TExpr.fromInteger a

instance Rig (Expr Integer) where
    fromNatural a = MT.TSymbolic.TExpr.fromInteger $ toInteger a

instance G.Group (Expr Integer) where
    (Expr a) - (Expr b) = let newB = M.map negate b
                            in suma (Expr a) (Expr newB)

instance N.Semiring (Expr Integer)

instance Unital (Expr Integer) where
    one = MT.TSymbolic.TExpr.fromInteger 1

instance A.Abelian (Expr Integer)

instance N.Multiplicative (Expr Integer) where
    a * b = prodSym a b

instance Commutative (Expr Integer)

instance PrettyCoeff (Expr Integer)

instance Num (Expr Integer) where
    (+) = suma
    (*) = prodSym

    signum a = one

    abs a = a
    a - b = suma a (negate b)

    fromInteger c = MT.TSymbolic.TExpr.fromInteger c


instance AP.UFD (Expr Integer)
instance AP.PID (Expr Integer)
instance AP.GCDDomain (Expr Integer)
instance AP.IntegralDomain (Expr Integer)
instance AP.ZeroProductSemiring (Expr Integer)

instance Real (Expr Integer) where
    toRational a = 1

instance AP.DecidableUnits (Expr Integer) where
    recipUnit = recipUnitExprIntegral

recipUnitExprIntegral :: Expr Integer -> Maybe (Expr Integer)
recipUnitExprIntegral (Expr a) = Just $ Expr a
recipUnitExprIntegral _ = Nothing


instance AP.DecidableAssociates (Expr Integer) where
    isAssociate = isAssociateExpr

isAssociateExpr :: Expr Integer -> Expr Integer -> Bool
isAssociateExpr = (==)


instance AP.UnitNormalForm (Expr Integer) where
    splitUnit zero = (one, zero)
    splitUnit n = (signum n, abs n)

instance AP.Euclidean (Expr Integer) where
    degree a = if isZero a then Nothing else Just 1
    divide = divMod

instance Integral (Expr Integer) where
    a `quot` b
        | isZero b                   = GR.divZeroError
        | otherwise                  = a :/: b

    a `rem` b
        | isZero b                   = GR.divZeroError
        | b == negate one            = Expr M.empty
        | otherwise                  = a :%: b

    div = divSym

    a `quotRem` b
        | isZero b                   = GR.divZeroError
        | otherwise                  = ((quot a b), (rem a b))

    toInteger a = 1

instance Enum (Expr Integer) where
    toEnum c = MT.TSymbolic.TExpr.fromInteger $ toInteger c
    fromEnum a = 1



suma :: (Eq a, Num a, Integral a) => Expr a -> Expr a -> Expr a
suma (Expr a) (Expr b) = simplify $ Expr $ M.unionWith (+) a b 

prodSym :: (Integral a, Num a) => Expr a -> Expr a -> Expr a
-- prodSym _ zero = zero
-- prodSym zero _ = zero
-- prodSym a one = a
-- prodSym one a = a
prodSym (Expr a) (Expr b) = Expr (insertFull prod)
    where
        listA = M.toList a
        listB = M.toList b
        prod = prodList listA listB


insertFull :: (Num a) => [([String], a)] -> M.Map [String] a
insertFull [] = M.empty
insertFull [(k,v)] = M.singleton k v
insertFull (x:xs) = M.unionWith (+) (insertFull [x]) (insertFull xs)

-- Multiply two list of terms
prodList :: (Num a) => [([String], a)] -> [([String], a)] -> [([String], a)]
prodList [] _ = []
prodList  (x:xs) y = (map (*** x) y) ++ prodList xs y  


--Multiply two terms
(***) :: (Num a) => ([String], a) -> ([String], a) -> ([String], a)
(l1, c1) *** (l2, c2)
        | l1 == [""] = (l2, c1*c2)
        | l2 == [""] = (l1, c1*c2)
        | otherwise = (L.sort $ l1 ++ l2, c1*c2)


divSym :: (Num a) => Expr a -> Expr a -> Expr a
divSym zero _ = zero
divSym _ zero = GR.divZeroError
divSym a one = a
divSym a b = a :/: b

simplify :: (Num a, Eq a, Integral a) => Expr a -> Expr a
simplify (Expr a) = Expr $ M.filter (/=0) a

toExpr :: M.Map [String] a -> Expr a
toExpr  a = Expr a

fromExpr :: Expr a -> M.Map [String] a
fromExpr (Expr a) = a

fromString :: String -> Expr Integer
fromString str = Expr $ M.fromList [([str],1)]

fromInteger :: Integer -> Expr Integer
fromInteger int = Expr $ M.fromList [([""],int)]


evaluate :: Expr Integer -> String -> Integer -> Expr Integer
evaluate (Expr a) str val = Expr $ newMap
      where
            newMap = M.fromListWith (+) $ map (evalTerm str val) $ M.toList a
            evalTerm _ _ ([""], n) = ([""], n)
            evalTerm str val (lst, n) = (L.filter (/= str) lst, n*val^value)
                  where 
                        lengthList = length lst
                        lengthFiltered = length $ L.filter (/= str) lst
                        value =  fromIntegral (lengthList - lengthFiltered)