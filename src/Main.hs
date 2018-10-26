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
import Algebra.Prelude                   hiding ((>>),(>>=), null)
import Symbolic.Wu
import Symbolic.Expr
import qualified Data.Map.Strict as V
import Library.Wu
import Library.Mon
import Library.PolyClass
import Util.Coeff
import Test.TestCases
import Ejemplos.Conicas
import Ejemplos.GeneralizedSurface
import Ejemplos.Spheres
import Ejemplos.Strofoid
import qualified Data.Sized.Builtin       as M

main :: IO()
main = do
      putStrLn "\n Characteristic SET"
     -- print asc

