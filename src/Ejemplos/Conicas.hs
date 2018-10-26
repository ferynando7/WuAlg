{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}

module Ejemplos.Conicas (idealConicas) where

    import Algebra.Prelude
    import Symbolic.Wu
    import Data.Map.Strict as M
    import Symbolic.Expr

    x = var 0
    y = var 1
    x1 = var 2
    y1 = var 3
    l1 = var 4
    x2 = var 5
    y2 = var 6
    l2 = var 7
    x3 = var 8
    y3 = var 9
    l3 = var 10
    v = var 11


    a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r :: Expr Integer
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
    k = Expr $ M.fromList [(["k"],1)]
    l = Expr $ M.fromList [(["l"],1)]
    m = Expr $ M.fromList [(["m"],1)]
    n = Expr $ M.fromList [(["n"],1)]
    o = Expr $ M.fromList [(["o"],1)]
    p = Expr $ M.fromList [(["p"],1)]
    q = Expr $ M.fromList [(["q"],1)]
    r = Expr $ M.fromList [(["r"],1)]

    q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12:: PolynomialSym 12
    q1 = a !* x1^2 + b !* x1*y1 + c !* y1^2 + d !* x1 + e !* y1 + f!*1
    q2 = x - x1 - l1 * ( (2 * a) !* x1 + b !* y1 + d!*1 )
    q3 = y - y1 - l1 * ( (2 * c) !* y1 + b !* x1 + e!*1)
    q4 = (x - x1)^2 + (y - y1)^2 - v^2

    q5 = g !* x2^2 + h !* x2*y2 + i !* y2^2 + j !* x2 + k !* y2 + l!*1
    q6 = x - x2 - l2 * ( (2 * g) !* x2 + h !* y2 + j!*1 )
    q7 = y - y2 - l2 * ( (2 * i) !* y2 + h !* x2 + k!*1 )
    q8 = (x - x2)^2 + (y - y2)^2 - v^2

    q9 = m !* x3^2 + n !* x3*y3 + o !* y3^2 + p !* x3 + q !* y3 + r!*1
    q10 = x - x3 - l3 * ( (2 * m) !* x3 + n !* y3 + p!*1 )
    q11 = y - y3 - l3 * ( (2 * o) !* y3 + n !* x3 + q !*1)
    q12 = (x - x3)^2 + (y - y3)^2 - v^2

    idealConicas = [q1,q2,q3,q4,q5,q6,q7,q8,q9,q10,q11,q12]