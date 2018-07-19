{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}


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
--[x,y,v,l1,x1,y1,l2,x2,y2] = vars

-- [x,y,v,l1,x1,y1] = vars

--Circulo: (x-2)^(2)+ (y+5)^(2) - 4 
--p1 :: Polynomial' 9
--p1 =  (x-2)^(2)+ (y+5)^(2) - (2-v)^2

-- --Circulo: p2 =  (x+3)^(2)+ (y)^(2)  - 9
-- p2 :: Polynomial' 9
-- p2 =  (x+3)^(2) + (y)^(2)  - (3-v)^2

-- -- --Circulo: (x-4)^(2)+ (y)^(2) - 1
-- -- p3 :: Polynomial' 9
-- -- p3 =  (x-4)^(2)+ (y)^(2)  - (1-v)^2

-- --Parabola: (x-4)^(2) - y - 1
-- p4 :: Polynomial' 9
-- p4 = x - x1 - l1*(2*x1-8)

-- p5 :: Polynomial' 9
-- p5 = y - y1 + l1

-- p6 :: Polynomial' 9
-- p6 = (l1*(2*x1-8))^2 + (l1)^2 - v^2

-- p7 :: Polynomial' 9
-- p7 = (x1-4)^2 - y1 - (1)

-- p47 = [p4,p5,p6,p7]

-- --Parabola: (y+5)^2 - x - 5
-- p8 :: Polynomial' 9
-- p8 = x - x2 + l2

-- p9 :: Polynomial' 9
-- p9 = y - y2 - l2*(2*y2+10)

-- p10 :: Polynomial' 9
-- p10 = (x-x2)^2 + (y-y2)^2 - v^2

-- p11 :: Polynomial' 9
-- p11 = (y2+5)^2 - x2 - (5)

-- p811 = [p8,p9,p10,p11]


------------------
-------------
-------3D

-- [x,y,z,v,l1,x1,y1,z1,l2,x2,y2,z2] = vars

-- -- Esfera: x^2 + y^2 + z^2 - 4
-- -- p1 :: LabPolynomial (Polynomial' 8) '["x","y","z","v","l1","x1","y1","z1"]
-- p1 :: Polynomial' 12
-- p1 =  (x-5)^2 + (y-5)^2 + (z-3)^2  - (2 - v)^2

-- --Esfera: (x-10)^2 + y^2 + (z-10)^2 - 25
-- p2 :: Polynomial' 12
-- p2 =  (x-10)^2 + y^2 + (z-10)^2 - (5 - v)^2

-- --Paraboloid: 3*(x-6)^2 + 2(y+2)^2 + 6z
--  -- Grad x
-- p3 :: Polynomial' 12
-- p3 =  x - x1 - l1 * 6 * (x1-6)

-- --Grad y
-- p4 :: Polynomial' 12
-- p4 = y - y1 - l1 * 4 * (y1+2)

-- -- Grad z
-- p5 :: Polynomial' 12
-- p5 = z - z1 - l1 * 6

-- -- Sphere of distance

-- p6 :: Polynomial' 12
-- p6 = (x-x1)^2 + (y-y1)^2 + (z-z1)^2 - v^2

-- p7 :: Polynomial' 12
-- p7 = 3*(x1-6)^2 + 2*(y1+2)^2 + 6*z1

-- -- Hyperboloid":   3*(x+8)^2 - 2*y^2 + 3*z^2 - 6
-- p8 :: Polynomial' 12
-- p8 = 3*(x2 + 8)^2 - 2*y2^2 + 3*z2^2 - 6

-- -- Grad x
-- p9 :: Polynomial' 12
-- p9 = x - x2 - l2 * 6 *(x2+8)

-- -- Grad y
-- p10 :: Polynomial' 12
-- p10 = y -y2 + l2 * 4 * y2

-- -- Grad z
-- p11 :: Polynomial' 12
-- p11 = z - z2 - l2 * 6 * z2

-- p12 :: Polynomial' 12
-- p12 = (x-x2)^2 + (y-y2)^2 + (z-z2)^2 - v^2




[x,y] = vars

p1 :: Polynomial' 2
p1 = y^2 - x^2 - x^3


p2 :: Polynomial' 2
p2 = y^2 + x^2 - 1




---- SIMBOLICO----

--n, m :: Polynomial' 2
--[n,m] = vars

--[n, m] = map (injectVar . flip Variable Nothing) "nm"






-- p12 :: (LabPolynomial (Polynomial' 2) '["m", "n"])
-- p12 = #n^2 - #m^2 - #m^3

-- p13 :: (LabPolynomial (Polynomial' 2) '["m", "n"])
-- p13 = #m^2 + #n^2 -1

-- p14 :: Polynomial' 2
-- p14 = n^2 - m^2 - m^3

-- p15 :: Polynomial' 2
-- p15 = m^2 + n^2 -1



----------

--Se debe comparar "fu a" con "faster_f a" funciona para numeros grandes 



main :: IO()
main = do
--     putStrLn "\n Chain 2D"
--     print problem_chain
--     putStrLn "\n Chain Cuadrics"
--     print chainq
        print p2


