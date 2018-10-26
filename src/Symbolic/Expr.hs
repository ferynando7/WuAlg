--Some code taken from http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, TypeOperators #-}
{-# LANGUAGE UndecidableInstances, OverlappingInstances, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleContexts #-}

module Symbolic.Expr
(
    Expr(..),
    simplify,
    showTermSym
) where

import qualified Algebra.Prelude  as AP hiding ((++), (+), (-), (*), (^))
import Algebra.Ring.Polynomial.Class
import Prelude hiding (map, null, filter)
import qualified Numeric.Algebra.Class as N hiding ((+), (-), (^))
import qualified Numeric.Additive.Class as A
import qualified Numeric.Additive.Group as G
import Numeric.Algebra.Unital
import qualified Numeric.Ring.Class as NR
import Numeric.Rig.Class
import Numeric.Decidable.Zero 
import Numeric.Algebra.Commutative
import Data.Type.Natural hiding (one)
import Data.Map.Strict
import GHC.TypeLits
import GHC.Natural
import qualified GHC.Real as GR
import Algebra.Scalar

infixl 5 :/:, :%:

data Expr a =   Expr (Map [String] a) 
                | (Expr a) :/: (Expr a)
                | (Expr a) :%: (Expr a)
                deriving (Eq)

instance Show (Expr Integer) where
    show (Expr a) = "("++ showSym (toList a)++")"
    show (Expr a :/: Expr b) = showSym (toList a) ++ "/" ++ showSym (toList b)


showSym :: [([String],Integer)] -> String
showSym [] = ""
showSym [(x,c)] = show c ++ showTermSym x
showSym (x:xs) = showSym [x] ++ "+" ++ showSym xs

showTermSym :: [String] -> String
showTermSym [] = ""
showTermSym [x] = x
showTermSym (x:xs) = x ++ showTermSym xs

instance (Ord a) => Ord (Expr a) where
    Expr a `compare` Expr b
        | size a < size b = LT
        | otherwise = GT
    

instance (Integral a) => A.Additive (Expr a) where 
    Expr a + Expr b = simplify $ suma (Expr a) (Expr b)


instance N.RightModule Natural (Expr Integer) where
    Expr a *. n = Expr $ map (* (toInteger n)) a 

instance N.LeftModule Natural (Expr Integer) where
    n .* Expr a = Expr $ map (* (toInteger n)) a
         
instance N.RightModule Integer (Expr Integer) where
    Expr a *. n = Expr $ map (*n) a        

instance N.LeftModule Integer (Expr Integer) where
    n .* Expr a = Expr $ map (*n) a
    
instance N.Monoidal (Expr Integer) where
    zero = Expr empty

instance DecidableZero (Expr Integer) where
    isZero (Expr a) | null a = True
                    | otherwise = False

instance AP.Ring (Expr Integer) where
    fromInteger a = Expr $ fromList [([""], a)]

instance Rig (Expr Integer) where
    fromNatural a = Expr $ fromList [([""], toInteger a)]

instance G.Group (Expr Integer) where
    (Expr a) - (Expr b) = let newB = map negate b
                            in suma (Expr a) (Expr newB)
              
instance N.Semiring (Expr Integer)

instance Unital (Expr Integer) where
    one = Expr $ fromList [([""],1)]

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

    fromInteger c = Expr $ fromList [([""], c)]


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
        | b == negate one            = Expr empty
        | otherwise                  = a :%: b
    
    div = divSym

    a `quotRem` b
        | isZero b                   = GR.divZeroError
        | otherwise                  = ((quot a b), (rem a b))     

    toInteger a = 1

instance Enum (Expr Integer) where
    toEnum c = Expr $ fromList [([""], toInteger c)]
    fromEnum a = 1
  


suma :: (Eq a, Num a, Integral a) => Expr a -> Expr a -> Expr a
suma (Expr a) (Expr b) = simplify $ Expr $ unionWith (+) a b 

prodSym :: (Integral a, Num a) => Expr a -> Expr a -> Expr a
-- prodSym _ zero = zero
-- prodSym zero _ = zero
-- prodSym a one = a
-- prodSym one a = a
prodSym (Expr a) (Expr b) = Expr (insertFull prod)
    where
        listA = toList a
        listB = toList b
        prod = prodList listA listB


insertFull :: (Num a) => [([String], a)] -> Map [String] a
insertFull [] = empty
insertFull [(k,v)] = singleton k v
insertFull (x:xs) = unionWith (+) (insertFull [x]) (insertFull xs)

-- Multiply two list of terms
prodList :: (Num a) => [([String], a)] -> [([String], a)] -> [([String], a)]
prodList [] _ = []
prodList  (x:xs) y = (AP.map (*** x) y) ++ prodList xs y  


--Multiply two terms
(***) :: (Num a) => ([String], a) -> ([String], a) -> ([String], a)
(l1, c1) *** (l2, c2)
        | l1 == [""] = (l2, c1*c2) 
        | l2 == [""] = (l1, c1*c2)
        | otherwise = (AP.sort $ l1 ++ l2, c1*c2)


divSym :: (Num a) => Expr a -> Expr a -> Expr a
divSym zero _ = zero
divSym _ zero = GR.divZeroError
divSym a one = a
divSym a b = a :/: b

simplify :: (Num a, Eq a, Integral a) => Expr a -> Expr a
simplify (Expr a) = Expr $ filter (/=0) a

