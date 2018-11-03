{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}

module Ejemplos.Strofoid (idealStrofoid,r3) where

    import Algebra.Prelude hiding (fromString)
    import Symbolic.Wu
    import Data.Map.Strict as M
    import Symbolic.Expr

    a = fromString "a"
    b = fromString "b"
    c = fromString "c"
    d = fromString "d"
    e = fromString "e"
    f = fromString "f"

    x = var 0
    y = var 1

    r1,r2 :: PolynomialSym 2
    r1 = a !* y^2 +   b !* x^2 +  c !* x^3
    r2 = d !* y^2 + e !* x^2 + f !* 1
    r3= (2*a + 4*b)!* y^2 +   (8*b+2*a) !* x^2 + 2* c !* x^3

    idealStrofoid = [r1,r2]
