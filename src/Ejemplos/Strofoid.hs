{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}

module Ejemplos.Strofoid (idealStrofoid) where

    import Algebra.Prelude
    import Symbolic.Wu
    import Data.Map.Strict as M
    import Symbolic.Expr

    a = Expr $ M.fromList [(["a"],1)]
    b = Expr $ M.fromList [(["b"],1)]
    c = Expr $ M.fromList [(["c"],1)]
    d = Expr $ M.fromList [(["d"],1)]
    e = Expr $ M.fromList [(["e"],1)]
    f = Expr $ M.fromList [(["f"],1)]

    x = var 0
    y = var 1

    r1,r2 :: PolynomialSym 2
    r1 = a !* y^2 + b !* x^2 + c !* x^3
    r2 = d !* y^2 + e !* x^2 + f !* 1

    idealStrofoid = [r1,r2]