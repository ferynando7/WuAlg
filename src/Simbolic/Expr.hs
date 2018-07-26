--Some code taken from http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, TypeOperators #-}
{-# LANGUAGE UndecidableInstances, OverlappingInstances, FlexibleInstances, TypeSynonymInstances #-}
module Simbolic.Expr
(
    Expr(Var, Const),
    sample,
    simplify,
    fullSimplify
) where

import Algebra.Ring.Polynomial.Class
import qualified Numeric.Algebra.Class as N
import qualified Numeric.Additive.Class as A
import qualified Numeric.Additive.Group as G
import Numeric.Algebra.Unital
import Numeric.Ring.Class
import Numeric.Rig.Class
import Numeric.Decidable.Zero 
import GHC.Natural
import Numeric.Algebra.Commutative
import Control.DeepSeq (NFData (..))

infixl 4 :+:
infixl 5 :*:, :/:
infixr 6 :^:



data Expr a = Var Char
                | Const a 
                | (Expr a) :+: (Expr a)
                | (Expr a) :-: (Expr a)
                | (Expr a) :*: (Expr a)
                | (Expr a) :^: (Expr a)
                | (Expr a) :/: (Expr a)
                deriving (Eq)

instance (Show a) => Show (Expr a) where
    show (Var a) = show a
    show (Const a) = show a
    show (left :+: right) = show left ++ "+" ++ show right
    show (left :-: right) = show left ++ "-" ++ show right 
    show (left :*: right) = show left ++ "*" ++ show right 
    show (left :^: right) = show left ++ "^" ++ show right 

instance A.Additive (Expr Integer) where 
    a + b = a :+: b



instance N.LeftModule Natural (Expr Integer) where
   a .* b = Const (toInteger a) :*: b

instance N.RightModule Natural (Expr Integer) where
    b *. a = b :*: Const (toInteger a)

instance N.LeftModule Integer (Expr Integer) where
    a .* b = Const a :*: b
    
instance N.RightModule Integer (Expr Integer) where
    b *. a = b :*: Const a        

instance N.LeftModule (Expr Integer) (Expr Integer) where 
    a .* b = a :*: b

instance N.RightModule (Expr Integer) (Expr Integer) where 
    a *. b = a :*: b


instance N.Monoidal (Expr Integer) where
    zero = Const 0

instance DecidableZero (Expr Integer) where
    isZero (Const 0) = True
    isZero expr = False

instance Ring (Expr Integer) where
    fromInteger a = Const a

instance Rig (Expr Integer) where
    fromNatural a = Const (toInteger a)

instance G.Group (Expr Integer) where
    (-) = (:-:)
    negate = negate'
    subtract exp1 exp2 = exp2 :-: exp1
    times a expr = Const (Prelude.fromIntegral a) :*: expr

instance N.Semiring (Expr Integer)

instance Unital (Expr Integer) where
    one = Const 1

instance A.Abelian (Expr Integer)

instance N.Multiplicative (Expr Integer) where
    (*) = (:*:)

instance Commutative (Expr Integer)

instance PrettyCoeff (Expr Integer)

instance Num (Expr Integer) where
    expr1 + expr2 = expr1 :+: expr2
    expr1 * expr2 = expr1 :*: expr2
    
    signum (Const a)    | a <= 0 = negate' $ Const 1
                        | a == 0 = Const 0
                        | a >= 0 = Const 1

    fromInteger a = Const a
    expr1 - expr2 = expr1 :-: expr2

sample :: Expr Integer
sample = Const (-3) :*: Var 'x' :+: Var 'x' --3x^2



simplify :: (Num a, Eq a, Integral a) => Expr a -> Expr a
simplify (Const a :+: Const b) = Const (a + b)
simplify (a       :+: Const 0) = simplify a
simplify (Const 0 :+: a      ) = simplify a

simplify (Const a :-: Const b) = Const (a - b)
simplify (a       :-: Const 0) = simplify a
simplify (Const 0 :-: a      ) = simplify a

simplify (Const a :*: b :-: c) | b == c = simplify (Const (a-1) :*: (simplify b)) 
simplify (Const a :*: b :-: Const d :*: c) | b == c = simplify (Const (a-d) :*: (simplify b)) 


simplify (Const a :*: Const b) = Const (a*b)
simplify (a :*: Const 1)         = simplify a
simplify (Const 1 :*: a)         = simplify a
simplify (a :*: Const 0)         = Const 0
simplify (Const 0 :*: a)         = Const 0

simplify (Const a :^: Const b)       = Const (a^b)
simplify (a :^: Const 1)             = simplify a
simplify (a :^: Const 0)             = Const 1
simplify ((c :^: Const b) :^: Const a) = c :^: (Const (a*b))

simplify (Const a :*: (Const b :*: expr)) = (Const $ a*b) :*: (simplify expr)
simplify (Const a :*: expr :*: Const b) = (Const $ a*b) :*: (simplify expr)
simplify (expr :*: Const a :*: Const b) = (Const $ a*b) :*: (simplify expr)
simplify (Const a :*: (b :+: c))        = (Const a :*: (simplify b)) :+: (Const a :*: (simplify c))

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