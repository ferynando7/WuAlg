{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}

module Ejemplos.GeneralizedSurface (idealGenSur) where

    import Algebra.Prelude
    import Symbolic.Wu
    import Data.Map.Strict as M
    import Symbolic.Expr


    x1 = var 0
    y1 = var 1
    z1 = var 2
    l1 = var 3
    x = var 4
    y = var 5
    z = var 6


    a,b,c,d,e,f,g,h,i,j :: Expr Integer
    a = Expr $ M.fromList [(["a"],1)]
    b = Expr $ M.fromList [(["b"],1)]
    c = Expr $ M.fromList [(["c"],1)]
    d = Expr $ M.fromList [(["d"],1)]
    e = Expr $ M.fromList [(["e"],1)]
    f = Expr $ M.fromList [(["f"],1)]
    g = Expr $ M.fromList [(["g"],1)]
    h = Expr $ M.fromList [(["h"],1)]
    i = Expr $ M.fromList [(["i"],1)]
    j = Expr $ M.fromList [(["j"],1)]
    v = Expr $ M.fromList [(["v"],1)]


    q1,q2,q3,q4,q5:: PolynomialSym 7
    q1 = a !* x1^2 + b !* y1^2 + c !* z1^2 + d!* (x1 * y1) + e !* (x1 * z1)  + f!* (y1 * z1) +  g !* x1 +  h !* y1 + i !* z1 + j !* 1
    q2 = x - x1 - l1 * ( (2 * a) !* x1 + d !* y1 + e !* z1 + g !* 1 )
    q3 = y - y1 - l1 * ( (2 * b) !* y1 + d !* x1 + f !* z1 + h !* 1 )
    q4 = z - z1 - l1 * ( (2 * c) !* z1 + e !* x1 + f !* y1 + i !* 1 )
    q5 = (x - x1)^2 + (y - y1)^2 + (z - z1)^2 - v * v !* 1

    idealGenSur = [q1,q2,q3,q4,q5]