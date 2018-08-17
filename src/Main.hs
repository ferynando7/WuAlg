{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE NoMonomorphismRestriction, QuasiQuotes, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels, TypeOperators #-}


module Main where
--import Prelude hiding ((+),(-),(*),(^))
import Algebra.Prelude                   hiding ((>>),(>>=))
import Symbolic.Wu
import Symbolic.PolyClass
import Symbolic.PolyCompare
import Symbolic.Expr
import Symbolic.Mon
import qualified Symbolic.Sym as S
import qualified Data.Map.Strict        as M

sTwo :: SNat 2
sTwo = sing

-- x = var 0
-- y = var 1

-- q1,q2:: PolynomialSym 2
-- q1 = (Var "a") !* y^2 -  (Var "b") !* x^2 - (Var "c") !* x^3
-- q2 = (Var "d") !* y^2 + (Var "e") !* x - Var "f" !* 1

-- sp = sPolynomial' q1 q2 sTwo 0
-- ter = M.toList $ terms sp
-- selected = snd $ last ter

-- -- ww = Const 1 * (Const 1 * (e * (Const (-1) * c)))
-- -- ww3 = e * Const (-1) * c


a,b,x,y,z,c,e :: Expr Integer
a = Const 0
b = Const 1
--c = Const 2

x = Var "x"
y = Var "y"
z = Var "z"
c = Var "c"
e = Var "e"

res1, res2 :: Expr Integer
res1 = Var "e" * (-1) * Var "c"
res2 = (-1)* Var "c" * Var "e"

---

main :: IO()
main = do
      putStrLn "\n Characteristic SET"
