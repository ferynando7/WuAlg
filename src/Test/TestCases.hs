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


module Test.TestCases (test) where

import Algebra.Prelude hiding (fromString)
import Library.Wu
import Symbolic.Wu
import Symbolic.Expr
import qualified Data.Map.Strict as M


-- Test for strofoid numeric
x = var 0
y = var 1

s1,s2 :: Polynomial' 2

s1 = y^2 - x^2 - x^3
s2 = x^2 + y^2 - 1

strofoidNumeric :: (IsOrder n order, KnownNat n, Eq k, Num k, PrettyCoeff k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
    => [OrderedPolynomial k order n] -> IO()
strofoidNumeric pols
    | (show $ characteristicWuSet pols [] 0) == "[X_0 X_1^2 + 2 X_1^2 - X_0 - 1,X_1^4 + X_1^2 - 1]" = putStrLn "strofoidNumeric SUCCESSFUL"
    | otherwise = putStrLn "strofoidNumeric FAILED"


--Test for strofoid symbolic
a,b,c,d,e,f :: Expr Integer
a = fromString "a"
b = fromString "b"
c = fromString "c"
d = fromString "d"
e = fromString "e"
f = fromString "f"

ss1,ss2:: PolynomialSym 2
ss1 = a !* y^2 +  b !* x^2 + c !* x^3
ss2 = d !* y^2 + e !* x^2 + f !* 1

strofoidSymbolic :: (IsOrder n order, KnownNat n, Eq k, Num k, PrettyCoeff k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
    => [OrderedPolynomial k order n] -> IO()
strofoidSymbolic pols
    | (show $ characteristicWuSetSym pols [] 0) == "[(-1cde) X_0 X_1^2 + (1aee+-1bde) X_1^2 + (-1cef) X_0 + (-1bef),(1ccdddee) X_1^6 + (1aaeeeee+-2abdeeee+1bbddeee+3ccddeef) X_1^4 + (-2abeeeef+2bbdeeef+3ccdeeff) X_1^2 + (1bbeeeff+1cceefff)]" = putStrLn "strofoidSymbolic SUCCESSFUL"
    | otherwise = putStrLn "strofoidSymbolic FAILED"

test :: IO ()
test = do
    strofoidNumeric [s1, s2]
    strofoidSymbolic [ss1, ss2]
