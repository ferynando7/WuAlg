{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE NoMonomorphismRestriction, QuasiQuotes, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ExplicitNamespaces, GeneralizedNewtypeDeriving, IncoherentInstances #-}

module Main where
import Algebra.Prelude                   hiding ((>>),(>>=), null)
import Symbolic.Wu
import Symbolic.Expr
import qualified Data.Map.Strict as V
import Library.Wu
import Library.Mon
import Library.PolyClass
import Util.Coeff
import Test.TestCases
import qualified Data.Sized.Builtin       as M

sTwo :: SNat 2
sTwo = sing

sSix :: SNat 6
sSix = sing
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
-- a = Expr $ V.fromList [(["a"],1)]
-- b = Expr $ V.fromList [(["b"],1)]
-- c = Expr $ V.fromList [(["c"],1)]
-- d = Expr $ V.fromList [(["d"],1)]
-- e = Expr $ V.fromList [(["e"],1)]
-- f = Expr $ V.fromList [(["f"],1)]
-- g = Expr $ V.fromList [(["g"],1)]
-- h = Expr $ V.fromList [(["h"],1)]
-- i = Expr $ V.fromList [(["i"],1)]
-- j = Expr $ V.fromList [(["j"],1)]
-- k = Expr $ V.fromList [(["k"],1)]
-- l = Expr $ V.fromList [(["l"],1)]
-- m = Expr $ V.fromList [(["m"],1)]
-- n = Expr $ V.fromList [(["n"],1)]
-- o = Expr $ V.fromList [(["o"],1)]
-- p = Expr $ V.fromList [(["p"],1)]
-- q = Expr $ V.fromList [(["q"],1)]
-- r = Expr $ V.fromList [(["r"],1)]

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
-- a = Expr $ V.fromList [(["a"],1)]
-- b = Expr $ V.fromList [(["b"],1)]
-- c = Expr $ V.fromList [(["c"],1)]
-- d = Expr $ V.fromList [(["d"],1)]
-- e = Expr $ V.fromList [(["e"],1)]
-- f = Expr $ V.fromList [(["f"],1)]

-- aa = 2
-- bb = 1
-- cc = 3
-- dd = 1
-- ee = 4
-- ff = -2




-- qq1, qq2 :: Polynomial' 2
-- qq1 = aa*y^2 +  bb*x^2 + cc* x^3
-- qq2 = dd* y^2 + ee* x^2 + ff* 1


snat :: SNat 7
snat = sing

x1 = var 0
y1 = var 1
z1 = var 2
l1 = var 3
x = var 4
y = var 5
z = var 6


a,b,c,d,e,f,g,h,i,j :: Expr Integer
a = Expr $ V.fromList [(["a"],1)]
b = Expr $ V.fromList [(["b"],1)]
c = Expr $ V.fromList [(["c"],1)]
d = Expr $ V.fromList [(["d"],1)]
e = Expr $ V.fromList [(["e"],1)]
f = Expr $ V.fromList [(["f"],1)]
g = Expr $ V.fromList [(["g"],1)]
h = Expr $ V.fromList [(["h"],1)]
i = Expr $ V.fromList [(["i"],1)]
j = Expr $ V.fromList [(["j"],1)]
v = Expr $ V.fromList [(["v"],1)]

aa = 2
bb = 1
cc = 3
dd = 1
ee = 4
ff = -2

sN :: (KnownNat n) => Nat -> SNat n
sN n = sing


s1,s2 :: Polynomial' 2
s1 = y1^2 - x1^2 - x1^3
s2 = x1^2 + y1^2 - 1


p1 :: Polynomial' 7
p1 = x+y

q1,q2,q3,q4,q5:: PolynomialSym 7
q1 = a !* x1^2 + b !* y1^2 + c !* z1^2 + d!* (x1 * y1) + e !* (x1 * z1)  + f!* (y1 * z1) +  g !* x1 +  h !* y1 + i !* z1 + j !* 1
q2 = x - x1 - l1 * ( (2 * a) !* x1 + d !* y1 + e !* z1 + g !* 1 )
q3 = y - y1 - l1 * ( (2 * b) !* y1 + d !* x1 + f !* z1 + h !* 1 )
q4 = z - z1 - l1 * ( (2 * c) !* z1 + e !* x1 + f !* y1 + i !* 1 )
q5 = (x - x1)^2 + (y - y1)^2 + (z - z1)^2 - v * v !* 1

r1,r2 :: PolynomialSym 2
r1 = a !* y1^2 + b !* x1^2 + c !* x1^3
r2 = d !* y1^2 + e !* x1^2 + f !* 1

asc = characteristicWuSetSym [q1, q2, q3, q4] [] 0
--asc = S.ascendentChain [q1, q2, q3, q4, q5] [] [] sEigmainht 0

prueba :: KnownNat n => [Int] -> Monomial n
prueba xs = fromList sing xs

main :: IO()
main = do
      putStrLn "\n Characteristic SET"
      print asc

