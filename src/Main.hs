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

-- sTwo :: SNat 2
-- sTwo = sing

-- sTwelve :: SNat 12
-- sTwelve = sing


-- x = var 0
-- y = var 1
-- x1 = var 2
-- y1 = var 3
-- l1 = var 4
-- x2 = var 5
-- y2 = var 6
-- l2 = var 7
-- x3 = var 8
-- y3 = var 9
-- l3 = var 10
-- v = var 11


-- a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r :: Expr Integer
-- a = Expr $ fromList [(["a"],1)]
-- b = Expr $ fromList [(["b"],1)]
-- c = Expr $ fromList [(["c"],1)]
-- d = Expr $ fromList [(["d"],1)]
-- e = Expr $ fromList [(["e"],1)]
-- f = Expr $ fromList [(["f"],1)]
-- g = Expr $ fromList [(["g"],1)]
-- h = Expr $ fromList [(["h"],1)]
-- i = Expr $ fromList [(["i"],1)]
-- j = Expr $ fromList [(["j"],1)]
-- k = Expr $ fromList [(["k"],1)]
-- l = Expr $ fromList [(["l"],1)]
-- m = Expr $ fromList [(["m"],1)]
-- n = Expr $ fromList [(["n"],1)]
-- o = Expr $ fromList [(["o"],1)]
-- p = Expr $ fromList [(["p"],1)]
-- q = Expr $ fromList [(["q"],1)]
-- r = Expr $ fromList [(["r"],1)]

-- q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12:: PolynomialSym 12
-- q1 = a !* x1^2 + b !* x1*y1 + c !* y1^2 + d !* x1 + e !* y1 + f!*1
-- q2 = x - x1 - l1 * ( (2 * a) !* x1 + b !* y1 + d!*1 )
-- q3 = y - y1 - l1 * ( (2 * c) !* y1 + b !* x1 + e!*1)
-- q4 = (x - x1)^2 + (y - y1)^2 - v^2

-- q5 = g !* x2^2 + h !* x2*y2 + i !* y2^2 + j !* x2 + k !* y2 + l!*1
-- q6 = x - x2 - l2 * ( (2 * g) !* x2 + h !* y2 + j!*1 )
-- q7 = y - y2 - l2 * ( (2 * i) !* y2 + h !* x2 + k!*1 )
-- q8 = (x - x2)^2 + (y - y2)^2 - v^2

-- q9 = m !* x3^2 + n !* x3*y3 + o !* y3^2 + p !* x3 + q !* y3 + r!*1
-- q10 = x - x3 - l3 * ( (2 * m) !* x3 + n !* y3 + p!*1 )
-- q11 = y - y3 - l3 * ( (2 * o) !* y3 + n !* x3 + q !*1)
-- q12 = (x - x3)^2 + (y - y3)^2 - v^2

-- asc  = S.ascendentChain [q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12] [] [] sTwelve 0

-- sTwo :: SNat 2
-- sTwo = sing

-- x = var 0
-- y = var 1

-- a,b,c,d,e,f :: Expr Integer
-- a = Expr $ fromList [(["a"],1)]
-- b = Expr $ fromList [(["b"],1)]
-- c = Expr $ fromList [(["c"],1)]
-- d = Expr $ fromList [(["d"],1)]
-- e = Expr $ fromList [(["e"],1)]
-- f = Expr $ fromList [(["f"],1)]

-- aa = 2
-- bb = 1
-- cc = 3
-- dd = 1
-- ee = 4
-- ff = -2



-- q1,q2:: PolynomialSym 2
-- q1 = a !* y^2 +  b !* x^2 + c !* x^3
-- q2 = d !* y^2 + e !* x^2 + f !* 1

-- qq1, qq2 :: Polynomial' 2
-- qq1 = aa*y^2 +  bb*x^2 + cc* x^3
-- qq2 = dd* y^2 + ee* x^2 + ff* 1


sEight :: SNat 8
sEight = sing

x1 = var 0
y1 = var 1
z1 = var 2
l1 = var 3
x = var 4
y = var 5
z = var 6
v = var 7

a,b,c,d,e,f,g,h,i,j :: Expr Integer
a = Expr $ fromList [(["a"],1)]
b = Expr $ fromList [(["b"],1)]
c = Expr $ fromList [(["c"],1)]
d = Expr $ fromList [(["d"],1)]
e = Expr $ fromList [(["e"],1)]
f = Expr $ fromList [(["f"],1)]
g = Expr $ fromList [(["g"],1)]
h = Expr $ fromList [(["h"],1)]
i = Expr $ fromList [(["i"],1)]
j = Expr $ fromList [(["j"],1)]

aa = 2
bb = 1
cc = 3
dd = 1
ee = 4
ff = -2


q1,q2,q3,q4,q5:: PolynomialSym 8
q1 = a !* x1^2 + b !* y1^2 + c !* z1^2 + d!* (x1 * y1) + e !* (x1 * z1)  + f!* (y1 * z1) +  g !* x1 +  h !* y1 + i !* z1 + j !* 1
q2 = x - x1 - l1 * ( (2 * a) !* x1 + d !* y1 + e !* z1 + g !* 1 )
q3 = y - y1 - l1 * ( (2 * b) !* y1 + d !* x1 + f !* z1 + h !* 1 )
q4 = z - z1 - l1 * ( (2 * c) !* z1 + e !* x1 + f !* y1 + i !* 1 )
q5 = (x - x1)^2 + (y - y1)^2 + (z - z1)^2 - v^2

asc = S.ascendentChain [q1, q2, q3, q4, q5] [] [] sEight 0


main :: IO()
main = do
      putStrLn "\n Characteristic SET"
      print asc
