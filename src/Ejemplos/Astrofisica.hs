{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}

module Ejemplos.Astrofisica (idealastro,idealastro2) where

    import Algebra.Prelude hiding (fromString)
    import Library.Wu
    import Data.Map.Strict as M
    import Symbolic.Expr

    x = var 0
    y = var 1
    t = var 2
    v = var 3
    u = var 4
    w = var 5


    d1,d2,d3:: Polynomial' 6
    d1 = (x - 5)^2 + (y - 5)^2 - (t - 2)^2 - v
    d2 = (x - 1)^2 + (y - 1)^2 - (t - 1)^2 - u
    d3 = (x + 1)^2 + (y + 2)^2 - (t - 3)^2 - w

    idealastro = [d1,d2]
    idealastro2 = [d1,d2,d3]
