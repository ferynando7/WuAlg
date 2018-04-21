{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE NoMonomorphismRestriction, QuasiQuotes, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import Algebra.Algorithms.Groebner
import Algebra.Field.Finite
import Algebra.Prelude
import Data.Type.Ordinal.Builtin
import Algebra.Ring.Polynomial.Class
import Algebra.Ring.Polynomial.Labeled
import qualified Data.Sized.Builtin       as V
import qualified Data.Foldable            as F
import qualified Data.HashSet             as HS
import qualified Prelude                  as P
import Control.Lens
import Data.Int
import qualified Data.Map.Strict          as M



---------------DEFINICION DE LA FUNCION PSEUDOREMAINDER-----------------------------

pseudoRemainder :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
                    => OrderedPolynomial k order n -> OrderedPolynomial k order n -> SNat n -> Int -> OrderedPolynomial k order n
pseudoRemainder f g sN i
                | classVarDeg f sN i < classVarDeg g sN i || classVarDeg g sN i == 0 = f
                | otherwise = pseudoRemainder (sPolynomial' f g sN i) g sN i
------------------------------------------------------------------------


-----------DEFINCION DE LAS FUNCIONES QUE OBTIENEN EL GRADO DE LA VARIABLE DE CLASE---------------
--Returns the array of exponents of the leading monomial
leadingMonomialDegs :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
       => OrderedPolynomial k order n -> SNat n -> Int -> [Int]
leadingMonomialDegs f sN i = V.toList ( getMonomial  (leadingMonomial' f sN i))


--Returns the degree of the class variable
classVarDeg :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n  -> SNat n -> Int -> Int
classVarDeg f sN i =  (leadingMonomialDegs f sN i) !! i
---------------------------------------------------------------

------FUNCIONES PARA ACTUALIZAR LA CADENA ASCENDENTE----------------
--Funcion que obtiene el minimo polinomio con respecto a la variable de clase.
--Esto es util para saber para cual polinomio dividir
getMinimalPoly:: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> SNat n -> Int -> OrderedPolynomial k order n
getMinimalPoly [f] _ _ = f
getMinimalPoly (x:xs) sN i
    | classVarDeg x sN i <= classVarDeg (getMinimalPoly xs sN i) sN i = x
    | otherwise = getMinimalPoly xs sN i


--Funcion que compara dos polinomios
isEqualTo :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n -> Bool
isEqualTo f g
    | f == g = True
    | otherwise = False


--Funcion que obtiene los polinomios que seran los divisores
getDividendPolys::(IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> SNat n -> Int -> [OrderedPolynomial k order n]
getDividendPolys (x:xs) sN i
    | isEqualTo x (getMinimalPoly (x:xs) sN i) = xs
    | otherwise = (x: getDividendPolys xs sN i)

------------------FUNCION QUE OBTIENE LOS PSEUDOREMAINDERS DE UN CONJUNTO DE POLINOMIOS
getPseudoRemainders :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> SNat n -> Int -> [OrderedPolynomial k order n]
getPseudoRemainders [a] _ _ = []
getPseudoRemainders polys sN i =
    let minPoly = getMinimalPoly polys sN i
        dividend = getDividendPolys polys sN i
        in ((pseudoRemainder (head(dividend)) minPoly sN i) : (getPseudoRemainders (minPoly:tail(dividend)) sN i))


---------FUNCION QUE OBTIENE LA CADENA ASCENDENTE-------
fullAscendentChain :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> SNat n-> Int -> [OrderedPolynomial k order n]
fullAscendentChain [a] _ _ = [a]
fullAscendentChain polys sN i = (getMinimalPoly polys sN i : fullAscendentChain (getPseudoRemainders polys sN i) sN (i+1))


ascendentChain :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> [OrderedPolynomial k order n]  -> SNat n -> Int -> Int -> [OrderedPolynomial k order n]
ascendentChain polys [] sN i p
                    | i == 0 && i == p = [possiblePoly ]
                    | i == 0 = (possiblePoly : ascendentChain polys (getPseudoRemainders polys sN i) sN (i+1) p)
                    | i >= p  =  []
                    where  possiblePoly = getMinimalPoly polys sN i
ascendentChain polys pseudos sN i p
                    | i < p && i /= 0 =  (checkChainPoly : ascendentChain polys (getPseudoRemainders pseudos sN i) sN (i+1) p)
                    | i >= p  =  []
                    where   checkChainPoly = getMinimalPoly (invPseudoRemainders (ascendentChain polys [] sN 0 (i-1)) possiblePoly sN i) sN i
                            possiblePoly = getMinimalPoly pseudos sN i
                            --getMinimalPoly (possiblePoly: [checkChainPoly]) sN i

--FUNCIONES NUEVAS--

invPseudoRemainders :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> OrderedPolynomial k order n -> SNat n -> Int -> [OrderedPolynomial k order n]
invPseudoRemainders [] _ _ _ = []
invPseudoRemainders (x:xs) poly sN i = ((pseudoRemainder poly x sN i) : (invPseudoRemainders xs poly sN i))


--Funciones que obtiene el maximo de cada posicion de dos arreglos
maxDegrees :: [Int] -> [Int] -> [Int]
maxDegrees [a] [b] = [max a b]
maxDegrees (x:xs) (y:ys) = (max x y : maxDegrees xs ys)

--Funcion que obtiene el lcmMonomial entre dos monomios expresados en arreglo de Int cada uno

lcmMonomial' :: SNat n -> [Int] -> [Int] -> OrderedMonomial ord n
lcmMonomial' n a b = 
    let monomList = maxDegrees a b
    in toMonomial n monomList 

--Funcion que converte un arreglo de Int en un Monomio
toMonomial :: SNat n -> [Int] -> OrderedMonomial ord n
toMonomial n a = orderMonomial Proxy (fromList n a)


--Funcion que obtiene el leadingMonomial en terminos de arreglos de Int
maxMonomial :: [[Int]] -> Int -> [Int]
maxMonomial [a] _ = a
maxMonomial (x:xs) n
    | (x!!n) > (maxMonomial xs n)!!n = x
    | otherwise = maxMonomial xs n

--Funcion que obtiene el leadingMonomial de forma algebraica
leadingMonomial' :: (IsOrderedPolynomial poly, Field (Coefficient poly))
                    => poly -> SNat n -> Int -> OrderedMonomial ord n
leadingMonomial' f n i = 
    let a = map (V.toList) (HS.toList (monomials f))
    in toMonomial n (maxMonomial a i)





--Algunos polinomios

--Ideal 1
p1 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
p1 =
    let [x,y,z] = vars
    in x - y^2 +2*z

p2 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
p2 =
    let [x,y,z] = vars
    in x^2 - 13*y - 10*z

p3 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
p3 =
    let [x,y,z] = vars
    in x^2 -12*y -13*z
---

--Ideal 2
p4 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
p4 =
    let [x,y,z] = vars
    in y^2 - x^2 - x^3

p5 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
p5 =
    let [x,y,z] = vars
    in x^2 + y^2 -1
------------------------------------------------------------------------------



--Nueva funcion sPolynomial en la cual se indica la variable con respecto a la cual se debe obtener el sPolynomial

----- FUNCIONES ANTHONY

leadingTerm' ::  (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        =>  OrderedPolynomial k order n -> SNat n -> Int -> (k, OrderedMonomial order n)
leadingTerm' f n i =
        let idx = indexMax f n i
        in (getTerms' f)!!idx

getTerms' :: OrderedPolynomial k order n -> [(k, OrderedMonomial order n)]
getTerms' = map (snd &&& fst) . M.toAscList . _terms

polToList :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n  -> [[Int]]
polToList polynom = (map V.toList (HS.toList (monomials polynom)))

--indexMax :: poly -> SNat n -> Int -> Int
indexMax :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
         => OrderedPolynomial k order n -> SNat n -> Int -> Int
indexMax f n i = M.findIndex (leadingMonomial' f n i) (_terms f)


sPolynomial' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
           => OrderedPolynomial k order n  -> OrderedPolynomial k order n  -> SNat n -> Int -> OrderedPolynomial k order n
sPolynomial' f g n i =
      let h = (one, lcmMonomial' n (maxMonomial (polToList f) i) (maxMonomial (polToList g) i) )
      in toPolynomial (h `tryDiv` (leadingTerm' f n i) )* f - toPolynomial (h `tryDiv` (leadingTerm' g n i) ) * g

---- ACABA FUNCIONES ANTHONY--------------


-------- ZONA DE PREUBAS ---------------------
numVar :: SNat 3
numVar = sing

chain = ascendentChain [p4,p5] [] numVar 0 2
chainAn = fullAscendentChain [p4,p5] numVar 0
------------------------------------------------


-----------------------------------------------------
main :: IO()
main = do
    putStrLn "Cadena Mejorada"
    print chain
    putStrLn "Cadena Antigua"
    print chainAn
    putStrLn "Termino que deberia ir"

