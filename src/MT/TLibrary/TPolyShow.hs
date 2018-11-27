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

module MT.TLibrary.TPolyShow where

import Algebra.Prelude hiding (appendFile)
import qualified Data.Map.Strict as M
import System.IO (writeFile, appendFile)


--Dado una lista de polinomios y un path, imprime los polinomios linea por linea en dicho path
printPolys :: (IsOrder n order, KnownNat n, PrettyCoeff k, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k) 
        => [OrderedPolynomial k order n] -> FilePath -> IO ()
printPolys [] _ = return ()
printPolys (x:xs) path = do 
                        a <- appendFile path $ show x ++ "\n"
                        printPolys xs path  

-- toma dos listas a b, imprime sus terminos haciendolos corresponder de la siguente forma
-- a1 = b1
-- a2 = b2
--    .
--    .
-- an = bn
printList :: (Show k) => [k] -> [k] -> FilePath -> IO ()
printList [] _ _ = return ()
printList (x:xs) (y:ys) path = do
                        a <- appendFile path $ show x ++ " = " ++ show y ++ "\n"
                        printList xs ys path

printCoeffs :: (IsOrder n order, KnownNat n, Eq k, Show k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k) 
        => [OrderedPolynomial k order n] -> [OrderedPolynomial k order n] -> FilePath -> IO ()
printCoeffs [] [] _ = return ()
printCoeffs new@(n:ns) old@(o:os) path = do
                                        let coeffsNew = ((map (snd)) . reverse . M.toList . terms) n
                                        let coeffsOld = ((map (snd)) . reverse . M.toList . terms) o
                                        printHead <- printList coeffsNew coeffsOld path
                                        printTail <- printCoeffs ns os path
                                        return ()
