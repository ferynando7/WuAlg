{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}

module Ejemplos.BisectorSurface (idealBisec, idealBisecReduced, idealBisecReducedWithNewCoeffs) where

import Algebra.Prelude hiding (fromString)
--import MT.TSymbolic.TWu
import Data.Map.Strict as M hiding (map)
--import MT.TSymbolic.TExpr
import Util.Coeff
import Symbolic.Expr
import Symbolic.Wu


v = var 0
x = var 1
y = var 2
z = var 3


a,b,c,d,e,f,g,h,i,j :: Expr Integer
a = fromString "a"
b = fromString "b"
c = fromString "c"
d = fromString "d"
e = fromString "e"
f = fromString "f"
g = fromString "g"
h = fromString "h"
i = fromString "i"
j = fromString "j"

--
q11, q12:: PolynomialSym 4
q11 = x^1*0 +x^9 *y +x^8 *y^2 +x^7 *y^3 +x^6 *y^4 +x^5 *y^5 +x^4 *y^6 +x^3 *y^7 +x^2 *y^8 +x *y^9 +y^1*0 +x^9 *z +x^8 *y *z +x^7 *y^2 *z +x^6 *y^3 *z +x^5 *y^4 *z +x^4 *y^5 *z +x^3 *y^6 *z +x^2 *y^7 *z +x *y^8 *z +y^9 *z +x^8 *z^2 +x^7 *y *z^2 +x^6 *y^2 *z^2 +x^5 *y^3 *z^2 +x^4 *y^4 *z^2 +x^3 *y^5 *z^2 +x^2 *y^6 *z^2 +x *y^7 *z^2 +y^8 *z^2 +x^7 *z^3 +x^6 *y *z^3 +x^5 *y^2 *z^3 +x^4 *y^3 *z^3 +x^3 *y^4 *z^3 +x^2 *y^5 *z^3 +x *y^6 *z^3 +y^7 *z^3 +x^6 *z^4 +x^5 *y *z^4 +x^4 *y^2 *z^4 +x^3 *y^3 *z^4 +x^2 *y^4 *z^4 +x *y^5 *z^4 +y^6 *z^4 +x^5 *z^5 +x^4 *y *z^5 +x^3 *y^2 *z^5 +x^2 *y^3 *z^5 +x *y^4 *z^5 +y^5 *z^5 +x^4 *z^6 +x^3 *y *z^6 +x^2 *y^2 *z^6 +x *y^3 *z^6 +y^4 *z^6 +x^3 *z^7 +x^2 *y *z^7 +x *y^2 *z^7 +y^3 *z^7 +x^2 *z^8 +x *y *z^8 +y^2 *z^8 +x *z^9 +y *z^9 +z^1*0 +x^8 *v^2 +x^7 *y *v^2 +x^6 *y^2 *v^2 +x^5 *y^3 *v^2 +x^4 *y^4 *v^2 +x^3 *y^5 *v^2 +x^2 *y^6 *v^2 +x *y^7 *v^2 +y^8 *v^2 +x^7 *z *v^2 +x^6 *y *z *v^2 +x^5 *y^2 *z *v^2 +x^4 *y^3 *z *v^2 +x^3 *y^4 *z *v^2 +x^2 *y^5 *z *v^2 +x *y^6 *z *v^2 +y^7 *z *v^2 +x^6 *z^2 *v^2 +x^5 *y *z^2 *v^2 +x^4 *y^2 *z^2 *v^2 +x^3 *y^3 *z^2 *v^2 +x^2 *y^4 *z^2 *v^2 +x *y^5 *z^2 *v^2 +y^6 *z^2 *v^2 +x^5 *z^3 *v^2 +x^4 *y *z^3 *v^2 +x^3 *y^2 *z^3 *v^2 +x^2 *y^3 *z^3 *v^2 +x *y^4 *z^3 *v^2 +y^5 *z^3 *v^2 +x^4 *z^4 *v^2 +x^3 *y *z^4 *v^2 +x^2 *y^2 *z^4 *v^2 +x *y^3 *z^4 *v^2 +y^4 *z^4 *v^2 +x^3 *z^5 *v^2 +x^2 *y *z^5 *v^2 +x *y^2 *z^5 *v^2 +y^3 *z^5 *v^2 +x^2 *z^6 *v^2 +x *y *z^6 *v^2 +y^2 *z^6 *v^2 +x *z^7 *v^2 +y *z^7 *v^2 +z^8 *v^2 +x^6 *v^4 +x^5 *y *v^4 +x^4 *y^2 *v^4 +x^3 *y^3 *v^4 +x^2 *y^4 *v^4 +x *y^5 *v^4
q13 = y^6 *v^4 +x^5 *z *v^4 +x^4 *y *z *v^4 +x^3 *y^2 *z *v^4 +x^2 *y^3 *z *v^4 +x *y^4 *z *v^4 +y^5 *z *v^4 +x^4 *z^2 *v^4 +x^3 *y *z^2 *v^4 +x^2 *y^2 *z^2 *v^4 +x *y^3 *z^2 *v^4 +y^4 *z^2 *v^4 +x^3 *z^3 *v^4 +x^2 *y *z^3 *v^4 +x *y^2 *z^3 *v^4 +y^3 *z^3 *v^4 +x^2 *z^4 *v^4 +x *y *z^4 *v^4 +y^2 *z^4 *v^4 +x *z^5 *v^4 +y *z^5 *v^4 +z^6 *v^4 +x^9 +x^8 *y +x^7 *y^2 +x^6 *y^3 +x^5 *y^4 +x^4 *y^5 +x^3 *y^6 +x^2 *y^7 +x *y^8 +y^9 +x^8 *z +x^7 *y *z +x^6 *y^2 *z +x^5 *y^3 *z +x^4 *y^4 *z +x^3 *y^5 *z +x^2 *y^6 *z +x *y^7 *z +y^8 *z +x^7 *z^2 +x^6 *y *z^2 +x^5 *y^2 *z^2 +x^4 *y^3 *z^2 +x^3 *y^4 *z^2 +x^2 *y^5 *z^2 +x *y^6 *z^2 +y^7 *z^2 +x^6 *z^3 +x^5 *y *z^3 +x^4 *y^2 *z^3 +x^3 *y^3 *z^3 +x^2 *y^4 *z^3 +x *y^5 *z^3 +y^6 *z^3 +x^5 *z^4 +x^4 *y *z^4 +x^3 *y^2 *z^4 +x^2 *y^3 *z^4 +x *y^4 *z^4 +y^5 *z^4 +x^4 *z^5 +x^3 *y *z^5 +x^2 *y^2 *z^5 +x *y^3 *z^5 +y^4 *z^5 +x^3 *z^6 +x^2 *y *z^6 +x *y^2 *z^6 +y^3 *z^6 +x^2 *z^7 +x *y *z^7 +y^2 *z^7 +x *z^8 +y *z^8 +z^9 +x^7 *v^2 +x^6 *y *v^2 +x^5 *y^2 *v^2 +x^4 *y^3 *v^2 +x^3 *y^4 *v^2 +x^2 *y^5 *v^2 +x *y^6 *v^2 +y^7 *v^2 +x^6 *z *v^2 +x^5 *y *z *v^2 +x^4 *y^2 *z *v^2 +x^3 *y^3 *z *v^2 +x^2 *y^4 *z *v^2 +x *y^5 *z *v^2 +y^6 *z *v^2 +x^5 *z^2 *v^2 +x^4 *y *z^2 *v^2 +x^3 *y^2 *z^2 *v^2 +x^2 *y^3 *z^2 *v^2 +x *y^4 *z^2 *v^2 +y^5 *z^2 *v^2 +x^4 *z^3 *v^2 +x^3 *y *z^3 *v^2 +x^2 *y^2 *z^3 *v^2 +x *y^3 *z^3 *v^2 +y^4 *z^3 *v^2 +x^3 *z^4 *v^2 +x^2 *y *z^4 *v^2 +x *y^2 *z^4 *v^2 +y^3 *z^4 *v^2 +x^2 *z^5 *v^2 +x *y *z^5 *v^2 +y^2 *z^5 *v^2 +x *z^6 *v^2 +y *z^6 *v^2 +z^7 *v^2 +x^5 *v^4 +x^4 *y *v^4 +x^3 *y^2 *v^4 +x^2 *y^3 *v^4 +x *y^4 *v^4 +y^5 *v^4 +x^4 *z *v^4 +x^3 *y *z *v^4 +x^2 *y^2 *z *v^4 +x *y^3 *z *v^4 +y^4 *z *v^4 +x^3 *z^2 *v^4 +x^2 *y *z^2 *v^4
q12 = x *y^2 *z^2 *v^4 +y^3 *z^2 *v^4 +x^2 *z^3 *v^4 +x *y *z^3 *v^4 +y^2 *z^3 *v^4 +x *z^4 *v^4 +y *z^4 *v^4 +z^5 *v^4 +x^8 +x^7 *y +x^6 *y^2 +x^5 *y^3 +x^4 *y^4 +x^3 *y^5 +x^2 *y^6 +x *y^7 +y^8 +x^7 *z +x^6 *y *z +x^5 *y^2 *z +x^4 *y^3 *z +x^3 *y^4 *z +x^2 *y^5 *z +x *y^6 *z +y^7 *z +x^6 *z^2 +x^5 *y *z^2 +x^4 *y^2 *z^2 +x^3 *y^3 *z^2 +x^2 *y^4 *z^2 +x *y^5 *z^2 +y^6 *z^2 +x^5 *z^3 +x^4 *y *z^3 +x^3 *y^2 *z^3 +x^2 *y^3 *z^3 +x *y^4 *z^3 +y^5 *z^3 +x^4 *z^4 +x^3 *y *z^4 +x^2 *y^2 *z^4 +x *y^3 *z^4 +y^4 *z^4 +x^3 *z^5 +x^2 *y *z^5 +x *y^2 *z^5 +y^3 *z^5 +x^2 *z^6 +x *y *z^6 +y^2 *z^6 +x *z^7 +y *z^7 +z^8 +x^6 *v^2 +x^5 *y *v^2 +x^4 *y^2 *v^2 +x^3 *y^3 *v^2 +x^2 *y^4 *v^2 +x *y^5 *v^2 +y^6 *v^2 +x^5 *z *v^2 +x^4 *y *z *v^2 +x^3 *y^2 *z *v^2 +x^2 *y^3 *z *v^2 +x *y^4 *z *v^2 +y^5 *z *v^2 +x^4 *z^2 *v^2 +x^3 *y *z^2 *v^2 +x^2 *y^2 *z^2 *v^2 +x *y^3 *z^2 *v^2 +y^4 *z^2 *v^2 +x^3 *z^3 *v^2 +x^2 *y *z^3 *v^2 +x *y^2 *z^3 *v^2 +y^3 *z^3 *v^2 +x^2 *z^4 *v^2 +x *y *z^4 *v^2 +y^2 *z^4 *v^2 +x *z^5 *v^2 +y *z^5 *v^2 +z^6 *v^2 +x^4 *v^4 +x^3 *y *v^4 +x^2 *y^2 *v^4 +x *y^3 *v^4 +y^4 *v^4 +x^3 *z *v^4 +x^2 *y *z *v^4 +x *y^2 *z *v^4 +y^3 *z *v^4 +x^2 *z^2 *v^4 +x *y *z^2 *v^4 +y^2 *z^2 *v^4 +x *z^3 *v^4 +y *z^3 *v^4 +z^4 *v^4 +x^7 +x^6 *y +x^5 *y^2 +x^4 *y^3 +x^3 *y^4 +x^2 *y^5 +x *y^6 +y^7 +x^6 *z +x^5 *y *z +x^4 *y^2 *z +x^3 *y^3 *z +x^2 *y^4 *z +x *y^5 *z +y^6 *z +x^5 *z^2 +x^4 *y *z^2 +x^3 *y^2 *z^2 +x^2 *y^3 *z^2 +x *y^4 *z^2 +y^5 *z^2 +x^4 *z^3 +x^3 *y *z^3 +x^2 *y^2 *z^3 +x *y^3 *z^3 +y^4 *z^3 +x^3 *z^4 +x^2 *y *z^4 +x *y^2 *z^4 +y^3 *z^4 +x^2 *z^5 +x *y *z^5 +y^2 *z^5+x *z^6 +y *z^6 +z^7 +x^5 *v^2 +x^4 *y *v^2 +x^3 *y^2 *v^2 +x^2 *y^3 *v^2 +x *y^4 *v^2 +y^5 *v^2
q14 = x^4 *z *v^2 +x^3 *y *z *v^2 +x^2 *y^2 *z *v^2 +x *y^3 *z *v^2 +y^4 *z *v^2 +x^3 *z^2 *v^2 +x^2 *y *z^2 *v^2 +x *y^2 *z^2 *v^2 +y^3 *z^2 *v^2 +x^2 *z^3 *v^2 +x *y *z^3 *v^2 +y^2 *z^3 *v^2 +x *z^4 *v^2 +y *z^4 *v^2 +z^5 *v^2 +x^3 *v^4 +x^2 *y *v^4 +x *y^2 *v^4 +y^3 *v^4 +x^2 *z *v^4 +x *y *z *v^4 +y^2*z *v^4 +x *z^2 *v^4 +y *z^2 *v^4 +z^3 *v^4 +x^6 +x^5 *y +x^4 *y^2 +x^3 *y^3 +x^2 *y^4 +x *y^5 +y^6 +x^5 *z +x^4 *y *z +x^3 *y^2 *z +x^2 *y^3 *z +x *y^4 *z +y^5 *z +x^4 *z^2 +x^3 *y *z^2 +x^2 *y^2 *z^2 +x *y^3 *z^2 +y^4 *z^2 +x^3 *z^3 +x^2 *y *z^3 +x *y^2 *z^3 +y^3 *z^3 +x^2 *z^4 +x *y *z^4 +y^2 *z^4 +x *z^5 +y *z^5 +z^6 +x^4 *v^2 +x^3 *y *v^2 +x^2 *y^2*v^2 +x *y^3 *v^2 +y^4 *v^2 +x^3 *z *v^2 +x^2 *y *z *v^2 +x *y^2 *z *v^2 +y^3 *z *v^2 +x^2 *z^2 *v^2 +x *y *z^2 *v^2 +y^2 *z^2 *v^2 +x*z^3 *v^2 +y *z^3 *v^2 +z^4 *v^2 +x^2 *v^4 +x *y *v^4 +y^2 *v^4 +x *z *v^4 +y *z *v^4 +z^2 *v^4 +x^5 +x^4 *y +x^3 *y^2 +x^2 *y^3 +x *y^4 +y^5 +x^4 *z +x^3 *y *z +x^2 *y^2 *z +x *y^3 *z +y^4 *z +x^3 *z^2 +x^2 *y *z^2 +x *y^2 *z^2 +y^3 *z^2 +x^2 *z^3 +x *y *z^3 +y^2 *z^3 +x *z^4 +y *z^4 +z^5 +x^3 *v^2 +x^2 *y*v^2 +x *y^2 *v^2 +y^3 *v^2 +x^2 *z *v^2 +x *y *z *v^2 +y^2 *z *v^2 +x *z^2 *v^2 +y*z^2 *v^2 +z^3 *v^2 +x *v^4 +y *v^4 +z*v^4 +x^4 +x^3 *y +x^2 *y^2 +x *y^3 +y^4 +x^3 *z +x^2 *y *z +x *y^2 *z +y^3 *z +x^2 *z^2 +x *y *z^2 +y^2 *z^2 +x *z^3 +y *z^3 +z^4 +x^2 *v^2 +x *y *v^2 +y^2 *v^2 +x *z *v^2 +y *z *v^2 +z^2 *v^2 +v^4 +x^3 +x^2 *y +x *y^2 +y^3 +x^2 *z +x *y *z +y^2 *z +x *z^2 +y*z^2 +z^3 +x*v^2 +y*v^2 +z *v^2 +x^2 +x *y +y^2 +x *z +y *z +z^2 +v^2 +x +y +z +1

q1 = q11 + q12 + q13 + q14
qq1 = changeVariables q1 4 (Coeff "a")
qq2 = changeVariables q1 5 (Coeff "a")

idealBisec = [qq1, qq2]

--- Los polinomios se quedan expresados en la variable x que corresponde al numero 1.
idealBisecReduced = map (reducePolynomial 1) idealBisec
--- Los polinomios tienen nuevas variables
idealBisecReducedWithNewCoeffs = changeVariablesList idealBisecReduced 0 (Coeff "a")

