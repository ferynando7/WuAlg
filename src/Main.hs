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
import Test.TestCases
import Ejemplos.Conicas
import Ejemplos.GeneralizedSurface
import Ejemplos.Spheres
import Ejemplos.Strofoid
import qualified Data.Sized.Builtin as S


a = fromString "a"
b = fromString "b"

--expr = (2a2*cc2*cc2*g2*j2+4a2*cc2*ce2*f2*g2)

evaluate :: Expr Integer -> String -> Integer -> Expr Integer
evaluate (Expr a) str val = Expr $ newMap
      where
            newMap = M.fromList $ map (evalTerm str val) $ M.toList a
            evalTerm _ _ ([""], n) = ([""], n)
            evalTerm str val (lst, n) = (filter (/= str) lst, n*val^value)
                  where 
                        lengthList = length lst
                        lengthFiltered = length $ filter (/= str) lst
                        value =  fromIntegral (lengthList - lengthFiltered)


main :: IO()
main = do
      putStrLn "\n Characteristic SET"
     -- print asc

