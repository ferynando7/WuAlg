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

pseudoRemainder :: (IsOrderedPolynomial poly, Field (Coefficient poly))
                    => poly -> poly -> Int -> poly
pseudoRemainder f g n
                | classVarDeg f n < classVarDeg g n = f
                | otherwise = pseudoRemainder ( sPolynomial f g) g n
------------------------------------------------------------------------


-----------DEFINCION DE LAS FUNCIONES QUE OBTIENEN EL GRADO DE LA VARIABLE DE CLASE---------------
--Returns the array of exponents of the leading monomial
leadingMonomialDegs :: (IsOrderedPolynomial poly, Field (Coefficient poly))
       => poly -> [Int]
leadingMonomialDegs = V.toList . getMonomial . leadingMonomial


--Returns the degree of the class variable
classVarDeg :: (IsOrderedPolynomial poly, Field (Coefficient poly))
        => poly -> Int -> Int
classVarDeg f n =  (leadingMonomialDegs f ) !! n
---------------------------------------------------------------

------FUNCIONES PARA ACTUALIZAR LA CADENA ASCENDENTE----------------
--Funcion que obtiene el minimo polinomio con respecto a la variable de clase.
--Esto es util para saber para cual polinomio dividir
getMinimalPoly::(IsOrderedPolynomial poly, Field (Coefficient poly))
        => [poly] -> Int -> poly
getMinimalPoly [f] _ = f
getMinimalPoly (x:xs) n
    | classVarDeg x n <= classVarDeg (getMinimalPoly xs n) n = x
    | otherwise = getMinimalPoly xs n


--Funcion que compara dos polinomios
isEqualTo :: (IsOrderedPolynomial poly, Field (Coefficient poly))
        => poly -> poly -> Bool
isEqualTo f g
    | f == g = True
    | otherwise = False


--Funcion que obtiene los polinomios que seran los divisores
getDividendPolys::(IsOrderedPolynomial poly, Field (Coefficient poly))
        => [poly] -> Int -> [poly]
getDividendPolys (x:xs) n
    | isEqualTo x (getMinimalPoly (x:xs) n) = xs
    | otherwise = (x: getDividendPolys xs n)

------------------FUNCION QUE OBTIENE LOS PSEUDOREMAINDERS DE UN CONJUNTO DE POLINOMIOS
getPseudoRemainders :: (IsOrderedPolynomial poly, Field (Coefficient poly))
        => [poly] -> Int -> [poly]
getPseudoRemainders [a] _ = []
getPseudoRemainders polys n =
    let minPoly = getMinimalPoly polys n
        dividend = getDividendPolys polys n
        in ((pseudoRemainder (head(dividend)) minPoly n) : (getPseudoRemainders (minPoly:tail(dividend)) n))


---------FUNCION QUE OBTIENE LA CADENA ASCENDENTE-------
fullAscendentChain :: (IsOrderedPolynomial poly, Field (Coefficient poly))
        => [poly] -> Int -> [poly]
fullAscendentChain [a] _ = [a]
fullAscendentChain polys n = (getMinimalPoly polys n : fullAscendentChain (getPseudoRemainders polys n) (n+1))




--FUNCIONES NUEVAS--

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

--indexMaxMonomial :: [[Int]] -> Int -> Int
--indexMaxMonomial [a] _ = 0
--indexMaxMonomial (x:xs) n
--    | (x!!n) >= (maxMonomial xs n)!!n = indexMaxMonomial xs n
--    | otherwise = 1 + indexMaxMonomial xs n


-- indexMaxMon :: [[Int]] -> [Int] -> Int
-- indexMaxMon a b = M.findIndex b (a)

--indexMaxi :: (IsOrderedPolynomial poly, Field (Coefficient poly))
--            => poly -> SNat n ->  Int -> Int
--indexMaxi poly n i = M.findIndex (leadingMonomial' poly n i) (_terms poly)

--Funcion que obtiene el leadingMonomial de forma algebraica
leadingMonomial' :: (IsOrderedPolynomial poly, Field (Coefficient poly))
                    => poly -> SNat n -> Int -> OrderedMonomial ord n
leadingMonomial' f n i = 
    let a = map (V.toList) (HS.toList (monomials f))
    in toMonomial n (maxMonomial a i)


--Nueva funcion sPolynomial en la cual se indica la variable con respecto a la cual se debe obtener el sPolynomial
-- sPolynomial' :: (IsOrderedPolynomial poly, Field (Coefficient poly))
--              => poly -> poly -> Int -> poly
-- sPolynomial' f g n =
--      let h = (one, lcmMonomial (leadingMonomial' f sONE n) (leadingMonomial' g sONE n))
--      in toPolynomial (h tryDiv leadingTerm f) * f - toPolynomial (h tryDiv leadingTerm g) * g



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



pol4 = polToList p4
pol5 = polToList p5


--leadMon1 = maxMonomial pol 1
--maximoMonomio = indexMaxMonomial pol 1 
-- maxi = maxMonomial pol 1
-- indexMax = M.findIndex 3 p4

sONE :: SNat 3
sONE = sing

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
polInTerms = (getTerms p4)!!0
chain = fullAscendentChain [p4,p5] 0
--indexMaxi = M.findIndex ld polInTerms
spol1 = sPolynomial' (chain!!1) (chain!!0) sONE 1
spol2 = sPolynomial' spol1 (chain!!0) sONE 1
--leadingp5 = leadingMonomial' p5 sONE 1
--leadingTerm5 = leadingTerm' p5 sONE 1
-- leadingTerm4 = leadingTerm' p4 sONE 1
-- idxMax  =  indexMax p5 sONE 1
-- h = (one, lcmp4p5)
-- division =  toPolynomial (h `tryDiv` leadingTerm4) * p4
-- ldp4 = maxMonomial pol4 0
-- ldp5 = maxMonomial pol5 0

-- lcmp4p5 = lcmMonomial' sONE ldp4 ldp5


-----------------------------------------------------
main :: IO()
main = do
    -- print pol
    -- print leadMon1
    -- print maximoMonomio
    -- putStrLn "lcm p4 p5 lista"
    -- print lcmp4p5
    -- --print ld
    -- putStrLn "Indice"
    -- print idxMax
    -- putStrLn "Leading Term 5"
    -- print leadingTerm5
    -- print division
    putStrLn "Cadena"
    print chain
    putStrLn "Termino que deberia ir"
    print spol1
    print spol2
    --putStrLn "lcm p4 p5"
    --print lcmp4p5
    --putStrLn "Terms of pol"
    --print polInTerms
