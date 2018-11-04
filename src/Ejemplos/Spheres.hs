{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}

module Ejemplos.Spheres (idealSpheres) where

    import Algebra.Prelude hiding (fromString)
    import Symbolic.Wu
    import Data.Map.Strict as M
    import Symbolic.Expr

    x = var 0
    y = var 1
    z = var 2
    v = var 3

    a,b,c,d,e,f,g,h,i,s,t,u :: Expr Integer
    a = fromString "a"
    b = fromString "b"
    c = fromString "c"
    s = fromString "s"
    d = fromString "d"
    e = fromString "e"
    f = fromString "f"
    t = fromString "t"
    g = fromString "g"
    h = fromString "h"
    i = fromString "i"
    u = fromString "u"


    s1,s2,s3,s4 :: PolynomialSym 4

    s1 = x^2 + y^2 + z^2 - v^2
    s2 = (x-a!*1)^2 + (y-b!*1)^2 + (z-c!*1)^2 - (s!*1-v)^2
    s3 = (x-d!*1)^2 + (y-e!*1)^2 + (z-f!*1)^2 - (t!*1-v)^2
    s4 = (x-g!*1)^2 + (y-h!*1)^2 + (z-i!*1)^2 - (u!*1-v)^2

    idealSpheres = [s1,s2,s3,s4]