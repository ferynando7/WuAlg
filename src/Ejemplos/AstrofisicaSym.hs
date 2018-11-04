{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}

module Ejemplos.AstrofisicaSym (idealastrosym,idealastrosym2) where

    import Algebra.Prelude hiding (fromString)
    import Symbolic.Wu
    import Data.Map.Strict as M
    import Symbolic.Expr

    x = var 0
    y = var 1
    t = var 2


    a,b,c,d,e,f,g,h,i,s,v,u :: Expr Integer
    a = fromString "a"
    b = fromString "b"
    c = fromString "c"
    s = fromString "s"
    d = fromString "d"
    e = fromString "e"
    f = fromString "f"
    v = fromString "v"
    g = fromString "g"
    h = fromString "h"
    i = fromString "i"
    u = fromString "u"


    d1,d2,d3:: PolynomialSym 3
    d1 = (x - a!*1)^2 + (y - b!*1)^2 - (t - c!*1)^2 - s!*1
    d2 = (x - d!*1)^2 + (y - e!*1)^2 - (t - f!*1)^2 - v!*1
    d3 = (x + g!*1)^2 + (y + h!*1)^2 - (t - i!*1)^2 - u!*1

    idealastrosym = [d1,d2]
    idealastrosym2 = [d1,d2,d3]
