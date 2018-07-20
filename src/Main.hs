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


q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13,q14,q15,q16,q17,q18,q19,q20 :: Polynomial' 20
-- First Cuadric
---------------------------------------------------
-- grad x
q1 = x - x1 - l1*(2 * 3* x1 + 11*y1 + 13 *z1 + 19 )
-- grad y
q2 = y - y1 - l1*(2 * 5* y1 + 11*x1 + 17 *z1 + 23 )
-- grad z
q3 = z - z1 - l1*(2 * 7* z1 + 11*x1 + 17 *z1 + 29 )
-- distance
q4 = (x-x1)^2 + (y-y1)^2 + (z-z1)^2 - v^2
-- Quadic
q5 = 3 * x1^2 + 5*y1^2 +7*z1^2 + 11*x1*y1 + 13*x1*z1 + 17 *y1 *z1 + 19*x1 + 23*y1 + 29 *z1 +31
---------------------------------------------------
-- Second Quadric
---------------------------------------------------
-- grad x
q6= x - x2 - l2*(2 * 37* x2 + 47*y2 + 53 *z2 + 61 )
-- grad y
q7 = y - y2 - l2*(2 * 41* y2 + 47*x2 + 59 *z2 + 67 )
-- grad z
q8 = z - z2 - l2*(2 * 43* z2 + 53*x2 + 59 *z2 + 71 )
-- distance
q9 = (x-x2)^2 + (y-y2)^2 + (z-z2)^2 - v^2
-- Quadic
q10 = 37 * x2^2 + 41*y2^2 + 43*z2^2 + 47*x2*y2 + 53*x2*z2 + 59 *y2 *z2 + 61*x2 + 67*y2 + 71 *z2 + 73
---------------------------------------------------
-- Third Quadric
---------------------------------------------------
-- grad x
q11 = x - x3 - l3*(2 * 79* x3 + 97*y3 + 101 *z3 + 107 )
-- grad y
q12 = y - y3 - l3*(2 * 83* y3 + 97*x3 + 103 *z3 + 109 )
-- grad z
q13 = z - z3 - l3*(2 * 89* z3 + 101*x3 + 103 *z3 + 113 )
-- distance
q14 = (x-x3)^2 + (y-y3)^2 + (z-z3)^2 - v^2
-- Quadic
q15 =  79 * x3^2 + 83*y3^2 + 89*z3^2 + 97*x3*y3 + 101*x3*z3 + 103 *y3 *z3 + 107*x3 + 109*y3 + 113 *z3 + 127
---------------------------------------------------
-- Fourth Quadric
---------------------------------------------------
-- grad x
q16 = x - x4 - l4*(2 * 131* x4 + 149*y4 + 151 *z4 + 163 )
-- grad y
q17 = y - y4 - l4*(2 * 137* y4 + 149*x4 + 157 *z4 + 167 )
-- grad z
q18 = z - z4 - l4*(2 * 139* z4 + 151*x4 + 157 *z4 + 173 )
-- distance
q19 = (x-x4)^2 + (y-y4)^2 + (z-z4)^2 - v^2
-- Quadic
q20 = 131* x4^2 + 137*y4^2 + 139*z4^2 + 149*x4*y4 + 151*x4*z4 + 157 *y4 *z4 + 163*x4 + 167*y4 + 173 *z4 + 179
---------------------------------------------------

charSet = ascendentChain [q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12,q13,q14,q15,q16,q17,q18,q19,q20] [] [] sTwenty 0

main :: IO()
main = do
      putStrLn "\n Characteristic SET"
      print charSet
