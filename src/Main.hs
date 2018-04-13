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

--Funcion que converte un arreglo de 
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
--


monom1 = map (V.toList) (HS.toList (monomials p4))
monom2 = map (V.toList) (HS.toList (monomials p5))

maxM1 = maxMonomial monom1 1
maxM2 = maxMonomial monom2 1


leastCM = lcmMonomial' sONE maxM1 maxM2

-- leadM1 = leadingMonomial' monom1 sONE 1
-- leadM2 = leadingMonomial' monom2 sONE 1

-- leadMON = lcmMonomial leadM1 leadM2


ll1 = leadingMonomial' p1 sONE 0
ll2 = leadingMonomial' p1 sONE 1
ll3 = leadingMonomial' p1 sONE 2


sONE :: SNat 3
sONE = sing


main :: IO()
main = do
--    print monom1
--    print monom2
--    print maxM1
--    print maxM2
--    print leastCM
--    print leadM1
--    print leadM2
--    print leadMON
    print ll1
    print ll2
    print ll3
