--Some code taken from http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, TypeOperators #-}
{-# LANGUAGE UndecidableInstances, OverlappingInstances, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies, DataKinds, FlexibleContexts #-}

module Symbolic.Expr
(
    Expr(..),
    sample,
    simplify,
    fullSimplify,
) where

import qualified Algebra.Prelude  as AP hiding ((++), (+), (-), (*), (^))
import Algebra.Ring.Polynomial.Class
import Prelude hiding (map)
import qualified Numeric.Algebra.Class as N hiding ((+), (-), (^))
import qualified Numeric.Additive.Class as A
import qualified Numeric.Additive.Group as G
import Numeric.Algebra.Unital
import qualified Numeric.Ring.Class as NR
import Numeric.Rig.Class
import Numeric.Decidable.Zero 
import Numeric.Algebra.Commutative
import Data.Type.Natural
import GHC.TypeLits
import GHC.Natural
import qualified GHC.Real as GR
import Algebra.Scalar

infixl 4 :+:, :-:
infixl 5 :*:, :/:, :%:
infixr 6 :^:



data Expr a = Var String
                | Const a 
                | (Expr a) :+: (Expr a)
                | (Expr a) :-: (Expr a)
                | (Expr a) :*: (Expr a)
                | (Expr a) :^: (Expr a)
                | (Expr a) :/: (Expr a)
                | (Expr a) :%: (Expr a)
                deriving (Eq,Show)

-- instance (Show a, Integral a) => Show (Expr a) where
--     show (Var a) = id a
--     show (Const a) = show a
--     show (left :+: right) = show (simplify left) ++ "+" ++ show (simplify right)
--     show (left :-: right) = show (simplify left) ++ "-" ++ show (simplify right)
--     show (expr :*: (Const a)) = show a ++ "*" ++ show (simplify expr)
--     show (left :*: right) = show (simplify left) ++ "*" ++ show (simplify right)
    
--     show (left :^: right) = show (simplify left) ++ "^" ++ show (simplify right)
--     show (left :%: right) = show (simplify left) ++ "%" ++ show (simplify right)
--     show (left :/: right) = show (simplify left) ++ "/" ++ show (simplify right)



instance (Ord a) => Ord (Expr a) where
    (Const a) `compare` (Const b) = a `compare` b
    (Const a) `compare` (Var x) = LT
    (Var x) `compare` (Const a) = GT  
    (Var x) `compare` (Var y) = x `compare` y


instance (Integral a) => A.Additive (Expr a) where 
    a + b = simplify (simplify a :+:simplify b)


instance N.RightModule Natural (Expr Integer) where
    b *. a =  simplify $ simplify b :*: Const (toInteger a)

instance N.LeftModule Natural (Expr Integer) where
    a .* b = simplify $ Const (toInteger a) :*: simplify b
     
instance N.LeftModule Integer (Expr Integer) where
    a .* b = simplify $ Const a :*: simplify b
    
instance N.RightModule Integer (Expr Integer) where
    b *. a = simplify $ simplify b :*: Const a        

instance N.LeftModule (Expr Integer) (Expr Integer) where 
    a .* b = simplify (simplify a :*: simplify b)

instance N.RightModule (Expr Integer) (Expr Integer) where 
    a *. b = simplify (simplify a :*: simplify b)

instance N.Monoidal (Expr Integer) where
    zero = Const 0

instance DecidableZero (Expr Integer) where
    isZero (Const 0) = True
    isZero expr = False

instance AP.Ring (Expr Integer) where
    fromInteger a = Const a

instance Rig (Expr Integer) where
    fromNatural a = Const (toInteger a)

instance G.Group (Expr Integer) where
    a - (Const (b) :*: c) | b < 0 = simplify (simplify a :+: Const (-b) :*: simplify c)
    a - b = simplify (simplify a :-: simplify b)
    


    negate = simplify . negate' . simplify
    subtract exp1 exp2 = simplify $ simplify exp2 :-: simplify exp1
    times a expr = simplify $ Const (GR.fromIntegral a) :*:simplify expr

instance N.Semiring (Expr Integer)

instance Unital (Expr Integer) where
    one = Const 1

instance A.Abelian (Expr Integer)

instance N.Multiplicative (Expr Integer) where
   a * b = simplify (simplify a :*: simplify b)

instance Commutative (Expr Integer)

instance PrettyCoeff (Expr Integer)

instance Num (Expr Integer) where
    expr1 + expr2 = simplify $ simplify expr1 :+: simplify expr2
    expr1 * expr2 = simplify $ simplify expr1 :*: simplify expr2
    
    signum (Const a)    | a <= 0 = negate' $ Const 1
                        | a == 0 = Const 0
                        | a >= 0 = Const 1
    
    abs (Const a)   | a < 0 = Const (-a)
                    | a >= 0 = Const a
    abs (Var a) = Var a
    abs (expr1 :+: expr2) = abs (simplify expr1) :+: abs(simplify expr2)
    abs (expr1 :-: expr2) = abs (simplify expr1) :-: abs(simplify expr2)
    abs (expr1 :*: expr2) = abs (simplify expr1) :*: abs(simplify expr2)
    abs (expr1 :/: expr2) | expr2 /= 0 = abs (simplify expr1) :/: abs(simplify expr2)
    abs (expr1 :^: expr2) = abs (simplify expr1) :^: simplify expr2
    abs (expr1 :%: expr2) = abs (simplify expr1) :%: abs(simplify expr2)





    fromInteger a = Const a
    expr1 - expr2 = simplify $ simplify expr1 :-: simplify expr2


instance AP.UFD (Expr Integer)
instance AP.PID (Expr Integer)
instance AP.GCDDomain (Expr Integer)
instance AP.IntegralDomain (Expr Integer)
instance AP.ZeroProductSemiring (Expr Integer)

instance Real (Expr Integer) where
    toRational (Const a) = a GR.:% 1

-- instance  (Expr Integer) where
--     a <= b = True
--     -- (Const a) <= (Const b) = a <= b
--     -- ((Const a) :*: expr1)  <= ((Const b) :*: expr2) | expr1 == expr2 =  a <= b



instance AP.DecidableUnits (Expr Integer) where
    recipUnit = recipUnitExprIntegral

recipUnitExprIntegral :: Expr Integer -> Maybe (Expr Integer)
recipUnitExprIntegral (Const a) = Just (Const a)
recipUnitExprIntegral _ = Nothing


instance AP.DecidableAssociates (Expr Integer) where
    isAssociate = isAssociateExpr

isAssociateExpr :: Expr Integer -> Expr Integer -> Bool
isAssociateExpr = (==)


instance AP.UnitNormalForm (Expr Integer) where
    splitUnit (Const 0) = (Const 1, Const 0)
    splitUnit n = (signum n, abs n)

instance AP.Euclidean (Expr Integer) where
    degree (Const a) = Just (fromInteger $ abs a) 
    divide = divMod

instance Integral (Expr Integer) where
    (Const a) `quot` (Const b)
        | b == 0                     = GR.divZeroError
        | otherwise                  = Const (GR.quot a b)
    
    (Const a) `rem` (Const b)
        | b == 0                     = GR.divZeroError
        | b == (-1)                  = Const 0
        | otherwise                  = Const (GR.rem a b)
    
    (Const a) `div` (Const b)
        | b == 0                     = GR.divZeroError
        | otherwise                  = Const (div a b)
    
    expr1 `div` expr2 
        | isZero expr2 = GR.divZeroError
        | otherwise = simplify $ simplify expr1 :/: simplify expr2

    expr1 `mod` expr2 = simplify (simplify expr1 :%: simplify expr2)
    -- (Const a) `mod` (Const b)
    --     | b == 0                     = GR.divZeroError
    --     | b == (-1)                  = Const 0
    --     | otherwise                  = Const (mod a b)
    


    (Const a) `quotRem` (Const b)
        | b == 0                     = GR.divZeroError
        | otherwise                  = (Const (a `GR.quot` b), Const (GR.rem a b))
    
    (Const a) `divMod` (Const b)
        | b == 0                     = GR.divZeroError
        | otherwise                  = (Const (toInteger (a `div` b)), Const (toInteger(mod a b)))
    
    toInteger (Const a) = toInteger a

instance Enum (Expr Integer) where
    succ (Const a) = Const (succ a)
    pred (Const a) = Const (pred a)
    toEnum a = Const (toInteger a)
    fromEnum (Const a) = fromInteger a
    enumFrom (Const a) = AP.map toEnum [fromEnum (Const a) ..]
    enumFromThen (Const a) (Const b) = AP.map toEnum [fromEnum (Const a), fromEnum (Const b) ..]
    enumFromTo (Const a) (Const b) = AP.map toEnum [fromEnum (Const a) .. fromEnum (Const b)]
    enumFromThenTo (Const a) (Const b) (Const c) = AP.map toEnum [fromEnum (Const a), fromEnum (Const b) .. fromEnum (Const c)]
  
instance N.RightModule (Scalar (Expr Integer)) (Expr Integer) where
    b *. Scalar a = simplify $ simplify b :*: a
    
instance N.LeftModule (Scalar (Expr Integer)) (Expr Integer) where
    Scalar a .* b = simplify $ a :*: simplify b

-- instance N.LeftModule (Scalar Integer) (Expr Integer) where
--     Scalar a .* b = Const a :*: b

-- instance N.RightModule (Scalar Integer) (Expr Integer) where
--     b *. Scalar a = b :*: Const a



-- instance IsPolynomial (Expr Integer) where
--     type Coefficient (Expr Integer) = (Expr Integer)
--     type Arity (Expr Integer) = 0

--    terms' expr = terms $ toPolynomial (expr, 1) 


-- instance (KnownNat n) => IsOrderedPolynomial (OrderedPolynomial (Expr Integer) Lex n) where
--     type MOrder (OrderedPolynomial (Expr Integer) Lex n) = Lex
-- coeff d = M.findWithDefault zero d . terms
-- {-# INLINE coeff #-}

-- terms = C.coerce
-- {-# INLINE terms #-}

-- orderedMonomials = M.keysSet . terms
-- {-# INLINE orderedMonomials #-}

-- toPolynomial (c, deg) =
--   if isZero c
--   then Polynomial M.empty
--   else Polynomial $ M.singleton deg c
-- {-# INLINE toPolynomial #-}

-- polynomial = normalize . C.coerce
-- {-# INLINE polynomial #-}

-- leadingTerm (Polynomial d) =
--   case M.maxViewWithKey d of
--     Just ((deg, c), _) -> (c, deg)
--     Nothing -> (zero, one)
-- {-# INLINE leadingTerm #-}

-- leadingMonomial = snd . leadingTerm
-- {-# INLINE leadingMonomial #-}

-- leadingCoeff = fst . leadingTerm
-- {-# INLINE leadingCoeff #-}



    

    -- injectCoeff r | isZero r  = Polynomial M.empty
    --                 | otherwise = Polynomial $ M.singleton one r
    -- {-# INLINE injectCoeff #-}
    
    -- sArity' = sizedLength . getMonomial . leadingMonomial
    -- {-# INLINE sArity' #-}
    
    -- mapCoeff' = mapCoeff
    -- {-# INLINE mapCoeff' #-}
    
    -- monomials = HS.fromList . map getMonomial . Set.toList . orderedMonomials
    -- {-# INLINE monomials #-}
    
    -- fromMonomial m = Polynomial $ M.singleton (OrderedMonomial m) one
    -- {-# INLINE fromMonomial #-}
    
    -- toPolynomial' (r, m) = Polynomial $ M.singleton (OrderedMonomial m) r
    -- {-# INLINE toPolynomial' #-}
    
    -- polynomial' dic = normalize $ Polynomial $ M.mapKeys OrderedMonomial dic
    -- {-# INLINE polynomial' #-}
    
    -- terms'    = M.mapKeys getMonomial . terms
    -- {-# INLINE terms' #-}
    



sample :: Expr Integer
sample = Const (-3) :*: Var "x" :+: Var "x" --3x^2



simplify :: (Num a, Eq a, Integral a) => Expr a -> Expr a
simplify (Const a :+: Const b) = Const (a + b)

simplify (Const a :*: expr1 :+: Const b :*: expr2) | expr1 == expr2 =  simplify $ Const (a + b) :*: simplify expr1 


simplify (a       :+: Const 0) = simplify a
simplify (Const 0 :+: a      ) = simplify a


simplify (Const a :-: Const b) = Const (a - b)
simplify (a       :-: Const 0) = simplify a
simplify (Const 0 :-: a      ) = simplify a


simplify (a :*: Const 0)         = Const 0
simplify (Const 0 :*: a)         = Const 0

simplify (Const a :*: Const b) = Const (a * b)
simplify (Const a :*: Const b :*: expr) = Const (a * b) :*: simplify expr

simplify (Const a :*: (Const b :*: c)) = Const(a*b) :*: simplify c 

simplify ((Const a :*: b) :*: c) = Const a :*: simplify (simplify b :*: simplify c) 
simplify (c :*: (Const a :*: b)) = Const a :*: simplify (simplify b :*: simplify c) 


simplify (Const b :*: a)         = Const b :*: simplify a
simplify (a :*: Const b)         = Const b :*: simplify a
simplify (a :*: Const b :*: c) = Const b :*: simplify (a :*: c)

simplify (Var a :*: Var b) | a > b = simplify (Var b :*: Var a) 



simplify (Const a :^: Const b)       = Const (a^b)
simplify (a :^: Const 1)             = simplify a
simplify (a :^: Const 0)             = Const 1
simplify ((c :^: Const b) :^: Const a) = c :^: (Const (a*b))

simplify (Const 0 :/: a        ) = Const 0
simplify (Const a :/: Const 0)   = error "Division by zero!"
simplify (Const a :/: Const b)   | a == b = Const 1 -- only when a == b
simplify (a       :/: Const 1)   = simplify a

simplify (a :/: b)  = (simplify a) :/: (simplify b)
simplify (a :^: b)  = (simplify a) :^: (simplify b)
simplify (a :*: b)  = (simplify a) :*: (simplify b)
simplify (a :+: b)  = (simplify a) :+: (simplify b)
simplify x          = id x

fullSimplify expr = fullSimplify' expr (Const 0) -- placeholder
  where fullSimplify' cur last | cur == last = cur
                               | otherwise = let cur' = simplify cur
                                                in fullSimplify' cur' cur

negate' :: (Num a) => Expr a -> Expr a
negate' (Var c)    = (Const (-1)) :*: (Var c)
negate' (Const a)  = Const (-a)
negate' (a :+: b)  = (negate' a) :+: (negate' b)
negate' (a :*: b)  = (negate' a) :*: b
negate' (a :^: b) = Const (-1) :*: a :^: b