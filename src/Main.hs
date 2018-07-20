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
import Algebra.Prelude                   hiding ((>>),(>>=))
import Library.Wu

--ZONA DE SNATS
sTwo :: SNat 2
sTwo = sing

sThree :: SNat 3
sThree = sing

sFour :: SNat 4
sFour = sing

sFive :: SNat 5
sFive = sing

sSix :: SNat 6
sSix = sing

sEight :: SNat 8
sEight = sing

sNine :: SNat 9
sNine = sing


sTwelve :: SNat 12
sTwelve = sing

sTwenty :: SNat 20
sTwenty = sing

sSixty :: SNat 60
sSixty = sing


x = var 0
y= var 1
z= var 2
v= var 3
l1= var 4
l2= var 5
l3= var 6
l4= var 7
x1= var 8
y1= var 9
z1= var 10
x2= var 11
y2= var 12
z2= var 13
x3= var 14
y3= var 15
z3= var 16
x4= var 17
y4= var 18
z4= var 19
a = var 20
b = var 21
c = var  22
d = var  23
e = var 24
f = var  25
g = var  26
h = var  27
i = var  28
j = var  29
a2 = var 30
b2 = var 31
c2 = var 32
d2 = var 33
e2 = var 34
f2 = var 35
g2 = var 36
h2 = var 37
i2 = var 38
j2 = var 39
a3 = var 40
b3 = var 41
c3 = var 42
d3 = var 43
e3 = var 44
f3 = var 45
g3 = var 46
h3 = var 47
i3 = var 48
j3 = var 49
a4 = var 50
b4 = var 51
c4 = var 52
d4 = var 53
e4 = var 54
f4 = var 55
g4 = var 56
h4 = var 57
i4 = var 58
j4 = var 59











q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13,q14,q15,q16,q17,q18,q19,q20 :: Polynomial' 60
-- First Cuadric
---------------------------------------------------
-- grad x
q1 = x - x1 - l1*(2 * a* x1 + d*
y1 + e*z1 + g )
-- grad y
q2 = y - y1 - l1*(2 * b* y1 + d*x1 + f *z1 + h )
-- grad z
q3 = z - z1 - l1*(2 * c* z1 + e*x1 + f *z1 + i )
-- distance
q4 = (x-x1)^2 + (y-y1)^2 + (z-z1)^2 - v^2
-- Quadic
q5 = a * x1^2 + b*y1^2 + c*z1^2 + d*x1*y1 + e*x1*z1 + f*y1 *z1 + g*x1 + h*y1 + i *z1 + j
---------------------------------------------------
-- Second Quadric
---------------------------------------------------
-- grad x
q6= x - x2 - l2*(2 * a2* x2 + d2*y2 + e2 *z2 + g2 )
-- grad y
q7 = y - y2 - l2*(2 * b2* y2 + d2*x2 + f2 *z2 + h2 )
-- grad z
q8 = z - z2 - l2*(2 * c2* z2 + e3*x2 + f2 *z2 + i2 )
-- distance
q9 = (x-x2)^2 + (y-y2)^2 + (z-z2)^2 - v^2
-- Quadic
q10 =  a2 * x2^2 +  b2 *y2^2 + c2 *z2^2 + d2*x2*y2 + e2*x2*z2 + f2 *y2 *z2 + g2*x2 + h2*y2 + i *z2 + j2
---------------------------------------------------
-- Third Quadric
---------------------------------------------------
-- grad x
q11 = x - x3 - l3*(2 *a3* x3 + d3*y3 + e3 *z3 + g3 )
-- grad y
q12 = y - y3 - l3*(2 *b3* y3 + d3*x3 + f3 *z3 + h3 )
-- grad z
q13 = z - z3 - l3*(2 *c3* z3 + e3*x3 + f3 *z3 + i3 )
-- distance
q14 = (x-x3)^2 + (y-y3)^2 + (z-z3)^2 - v^2
-- Quadic
q15 =  a3 * x3^2 + b3*y3^2 + c3*z3^2 + d3*x3*y3 + e3*x3*z3 + f3*y3 *z3 + g3*x3 + h3*y3 + i3 *z3 + j3
---------------------------------------------------
---------------------------------------------------
-- Fourth Quadric
-- grad x
q16 = x - x4 - l4*(2 * a4* x4 + d4*y4 + e4 *z4 + g4 )
-- grad y
q17 = y - y4 - l4*(2 * b4* y4 + d4*x4 + f4 *z4 + h4 )
-- grad z
q18 = z - z4 - l4*(2 * c4* z4 + e4*x4 + f4 *z4 + i4 )
-- distance
q19 = (x-x4)^2 + (y-y4)^2 + (z-z4)^2 - v^2
-- Quadic
q20 = a4* x4^2 + b4*y4^2 + c4*z4^2 + d4*x4*y4 + e4*x4*z4 + f4*y4 *z4 + g4*x4 + h4*y4 + i4 *z4 + f4
---------------------------------------------------

charSet = ascendentChainWithConstants [q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13,q14,q15,q16,q17,q18,q19,q20] [] [] sSixty 0 20

main :: IO()
main = do
      putStrLn "\n Characteristic SET"
      print charSet
