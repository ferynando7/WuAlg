{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}

module Ejemplos.Spheres (idealSpheres) where

    import Algebra.Prelude
    import Symbolic.Wu
    import Data.Map.Strict as M
    import Symbolic.Expr

    x = var 0
    y = var 1
    z = var 2
    v = var 3

    a,b,c,d,e,f,g,h,i,s,t,u :: Expr Integer
    a = Expr $ M.fromList [(["a"],1)]
    b = Expr $ M.fromList [(["b"],1)]
    c = Expr $ M.fromList [(["c"],1)]
    s = Expr $ M.fromList [(["s"],1)]
    d = Expr $ M.fromList [(["d"],1)]
    e = Expr $ M.fromList [(["e"],1)]
    f = Expr $ M.fromList [(["f"],1)]
    t = Expr $ M.fromList [(["t"],1)]
    g = Expr $ M.fromList [(["g"],1)]
    h = Expr $ M.fromList [(["h"],1)]
    i = Expr $ M.fromList [(["i"],1)]
    u = Expr $ M.fromList [(["u"],1)]


    s1,s2,s3,s4 :: PolynomialSym 4

    s1 = x^2 + y^2 + z^2 - v^2
    s2 = (x-a!*1)^2 + (y-b!*1)^2 + (z-c!*1)^2 - (s!*1-v)^2
    s3 = (x-d!*1)^2 + (y-e!*1)^2 + (z-f!*1)^2 - (t!*1-v)^2
    s4 = (x-g!*1)^2 + (y-h!*1)^2 + (z-i!*1)^2 - (u!*1-v)^2

    idealSpheres = [s1,s2,s3,s4]