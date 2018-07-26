{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE NoMonomorphismRestriction, QuasiQuotes, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels, TypeOperators #-}


module Main where
import Algebra.Prelude                   hiding ((>>),(>>=))
import Simbolic.Wu
import Simbolic.PolyClass
import Simbolic.Expr
import Simbolic.Mon

sTwo :: SNat 2
sTwo = sing

x = var 0
y = var 1

a :: Expr Integer
a = Const 1
-- aa :: OrderedMonomial order n
-- aa = x

-- xx :: PolynomialSym 2
-- xx = (Const 1) * x
aa = 2

q1,q2, q3 :: PolynomialSym 2
q1 = y^2 - x^2 -x^3
q2 = y^2 + x^2 - 1
q3 = x
-- charSet = ascendentChain [q1,q2] [] [] sTwo 0


main :: IO()
main = do
      putStrLn "\n Characteristic SET"
--      print charSet
