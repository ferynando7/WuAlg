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
import Algebra.Prelude                   hiding ((>>),(>>=), fromList)
import qualified Symbolic.Wu as S
import Symbolic.PolyClass
import Symbolic.PolyCompare
import Symbolic.Expr
import Symbolic.Mon
import Data.Map.Strict   
import Library.Wu as L

sTwo :: SNat 2
sTwo = sing

x = var 0
y = var 1

a,b,c,d,e,f :: Expr Integer
a = Expr $ fromList [(["a"],1)]
b = Expr $ fromList [(["b"],1)]
c = Expr $ fromList [(["c"],1)]
d = Expr $ fromList [(["d"],1)]
e = Expr $ fromList [(["e"],1)]
f = Expr $ fromList [(["f"],1)]

aa = 2
bb = 1
cc = 3
dd = 1
ee = 4
ff = -2



q1,q2:: PolynomialSym 2
q1 = a !* y^2 +  b !* x^2 + c !* x^3
q2 = d !* y^2 + e !* x^2 + f !* 1

qq1, qq2 :: Polynomial' 2
qq1 = aa*y^2 +  bb*x^2 + cc* x^3
qq2 = dd* y^2 + ee* x^2 + ff* 1


asc = L.ascendentChain [qq1, qq2] [] [] sTwo 0
---

main :: IO()
main = do
      putStrLn "\n Characteristic SET"
      print asc
