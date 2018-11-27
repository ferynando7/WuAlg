{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE NoMonomorphismRestriction, QuasiQuotes, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ExplicitNamespaces, GeneralizedNewtypeDeriving, IncoherentInstances #-}

module Main where
import Algebra.Prelude                   hiding ((>>),(>>=), null, fromString)
import Symbolic.Wu
import Symbolic.Expr
import qualified Data.Map.Strict as M
import Library.Wu
import Library.Mon
import Library.PolyClass
import Util.Coeff
import Library.PolyCompare
import Test.TestCases
import Ejemplos.Conicas
import Ejemplos.GeneralizedSurface
import Ejemplos.GeneralizedSurfaceNum
import Ejemplos.Spheres
import Ejemplos.Strofoid
import qualified Data.Sized.Builtin as S


-- x1 = var 0
-- y1 = var 1
-- z1 = var 2
-- l1 = var 3
-- x = var 4
-- y = var 5
-- z = var 6

-- a,b,c,d,e,f,g,h,i,j :: Integer
-- a = 1
-- b = 1
-- c = 1
-- d = 1
-- e = 1
-- f = 1
-- g = 1
-- h = 1
-- i = 1
-- j = 1
-- v = 1

-- q1,q2,q3,q4,q5:: Polynomial' 7
-- q1 =  b !* y1^2 + c !* z1^2 + d!* (x1 * z1) + e !* (x1 * z1)  + f!* (y1 * z1) +  h !* y1 + i !* z1 + j !* 1
-- q2 = x - x1 - l1 * ( (2 * a) !* x1 + d !* y1 + e !* z1 + g !* 1 )
-- q3 = y - y1 - y1 * ( (2 * b) !* y1 + d !* x1 + f !* z1 + h !* 1 )
-- q4 = z - z1 - l1 * ( (2 * c) !* z1 + e !* x1 + f !* y1 + i !* 1 )
-- q5 = (x - x1)^2 + (y - y1)^2 + (z - z1)^2 - v * v !* 1

main :: IO()
main = do
      putStrLn "\n Characteristic SET"
     -- print asc
