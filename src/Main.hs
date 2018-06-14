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
import Algebra.Ring.Polynomial.Monomial
import qualified Data.Sized.Builtin       as V
import qualified Data.Foldable            as F
import qualified Data.Set as Set
import qualified Data.HashSet             as HS
import qualified Prelude                  as P
import Control.Lens
import Data.Int
import qualified Data.Map.Strict          as M
import Data.Maybe                         (fromJust)



---------------DEFINICION DE LA FUNCION PSEUDOREMAINDER-----------------------------

pseudoRemainder :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
                    => OrderedPolynomial k order n -> OrderedPolynomial k order n -> SNat n -> Int -> OrderedPolynomial k order n
pseudoRemainder f g sN i
                | classVarDeg f sN i < classVarDeg g sN i || classVarDeg g sN i == 0 = f
                | otherwise = pseudoRemainder (sPolynomial' f g sN i) g sN i
------------------------------------------------------------------------


--pol :: OrderedPolynomial Rational Lex 3
pol :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
pol =
    let [x,y,z] = vars
    in x * y^2 + x^3 * y + x^3 * y^5 * z + x^3 * y^2 * z^7 + x^3 * y^2 * z^3


-----------DEFINCION DE LAS FUNCIONES QUE OBTIENEN EL GRADO DE LA VARIABLE DE CLASE---------------
--Returns the array of exponents of the leading monomial

leadingMonomialDegs :: (IsOrder n order, KnownNat n , Eq k, IsMonomialOrder n order, Euclidean k, Division k)
       => OrderedPolynomial k order n -> SNat n -> Int -> [Int]
leadingMonomialDegs f sN i = V.toList ( getMonomial  (leadingMonomial'' f sN i))


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
    | classVarDeg x sN i == 0 = getMinimalPoly xs sN i
    | classVarDeg (getMinimalPoly xs sN i) sN i == 0 = x
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
-- La funcion necesita una condicion de parada P que debe ser igual al numero de variables de los polinomios
ascendentChain polys [a] sN i p = [a]
  --[getMinimalPoly (invPseudoRemainders (ascendentChain polys [] sN 0 (i-1)) a sN i) sN i]
ascendentChain polys [] sN i p
                    | i == 0 && i == p = [possiblePoly ]
                    | i == 0 =  (possiblePoly: ascendentChain polys pseudos sN (i+1) p)
                    | i >= p  =  []
                    where  minimalPoly = getMinimalPoly polys sN i
                           possiblePoly = getMinimalPoly (minimalPoly : (getPseudoRemainders polys sN i) ) sN i
                           pseudos = map (\p -> if p == possiblePoly && minimalPoly /= possiblePoly then pseudoRemainder minimalPoly possiblePoly sN i else p) (getPseudoRemainders polys sN (i))
ascendentChain polys pseudos sN i p
--                    | i < p && i /= 0 =  (checkChainPoly : ascendentChain polys (getPseudoRemainders pseudos sN i) sN (i+1) p)
                    | i < p && i /= 0 =  (checkChainPoly : ascendentChain polys pseudos1 sN (i+1) p)
                    | i >= p  =  []
                    -- En caso de que i == p entonces paramos la funcion
                    where   checkChainPoly = getMinimalPoly (invPseudoRemainders (ascendentChain polys [] sN 0 (i-1)) possiblePoly sN i) sN i
                            possiblePoly = getMinimalPoly pseudos sN i
                            pseudos1 = map (\p -> if p == possiblePoly && checkChainPoly /= possiblePoly then pseudoRemainder checkChainPoly possiblePoly sN i else p) (getPseudoRemainders pseudos sN (i))


                    -- checkChainPoly obtiene el minimo polynomio entre los pseudoRemainders del polinomio candidato a ser añadido a la cadena y la cadena
                    -- possiblePoly es el polynomio candidato a ser añadido a la cadena
                    -- Si el possiblePoly no se puede dividir con respecto a ningun elemento de la cadena entonces, este pasa directamente a la cadena


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


leadingTerm'' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n -> SNat n -> Int -> (k, OrderedMonomial order n)
leadingTerm'' pol nat pos = (snd &&& fst) $ fromJust $ M.lookupLE chosenTerm (_terms pol)
        where
                chosenTerm = toMonomial nat (foldr foo firstTerm polToList)
                polToList = map (V.toList . getMonomial) (M.keys $ _terms pol)
                firstTerm = polToList!!0
                foo = \monomCoeffs acc -> if monomCoeffs!!pos > acc!!pos then monomCoeffs else acc

leadingMonomial'' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n -> SNat n -> Int -> OrderedMonomial order n
leadingMonomial'' pol nat pos = snd $ leadingTerm'' pol nat pos

leadingCoeff'' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n -> SNat n -> Int -> k
leadingCoeff'' pol nat pos = fst $ leadingTerm'' pol nat pos



-- Funcion que calcula el spolynomial factorizando y simplificando el resultado
sPolynomial' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
           => OrderedPolynomial k order n  -> OrderedPolynomial k order n  -> SNat n -> Int -> OrderedPolynomial k order n
sPolynomial' f g n i = simplyfy (toPolynomial (h `tryDiv` (one, commonLeadf )) * (simplyfy factorsg 0 n ) * f - toPolynomial (h `tryDiv` (one, commonLeadg ) ) * (simplyfy factorsf 0 n)* g) 0 n
                      where
                        h = (one, lcmMonomial (leadingMonomial'' f n i) (leadingMonomial'' g n i) )
                        factorsg = polyFactors g indexesg n i
                        factorsf = polyFactors f indexesf n i
                        commonLeadf = if (deg_f == 0 ) then (leadingMonomial'' f n i) else (gcdPolynomial' factorsf 0 n)  -- Obtiene el factor comun de la variable de clase del polinomio f
                        commonLeadg =  if (deg_g == 0 ) then (leadingMonomial'' g n i) else (gcdPolynomial' factorsg 0 n) -- Obtiene el factor comun de la variable de clase del polinomio g
                        deg_f = classVarDeg f n i -- Obtiene el grado de la variable de clase del polinomio f
                        deg_g = classVarDeg g n i -- Obtiene el grado del a variable de clase del polinomio g
                        indexesf = if (deg_f == 0 ) then ([0]) else (findIndices (\x -> x!!i == deg_f) (map (\x -> V.toList (getMonomial x)) (M.keys $ _terms f))) -- Obtiene los indicies de manera ascendente del arreglo
                        indexesg = if (deg_g == 0 ) then ([0]) else (findIndices (\x -> x!!i == deg_g) (map (\x -> V.toList (getMonomial x)) (M.keys $ _terms g))) -- Obtiene los indicies de manera ascendente del arreglo

polyFactors :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
          => OrderedPolynomial k order n  -> [Int] -> SNat n -> Int -> OrderedPolynomial k order n
polyFactors _ [] _  _= 0
polyFactors f index_list sN i = toPolynomial (snd auxMonom, fst auxMonom) + polyFactors f (tail index_list) sN i
                    where
                      auxMonom = M.elemAt (head index_list) (_terms f) -- Obtenemos los monomios que corresponden a los indices


-- Funcion que permite simplificar un polinomio en caso de que este tenga una expresion en commun en todos los monomios.
simplyfy ::(IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
           => OrderedPolynomial k order n  -> Int ->  SNat n -> OrderedPolynomial k order n
simplyfy poly i sN
                | i == len = toPolynomial ((snd auxMonom, fst auxMonom) `tryDiv` (one, gcdpol))
                | i /= len = toPolynomial ((snd auxMonom, fst auxMonom) `tryDiv` (one, gcdpol)) + simplyfy poly (i+1) sN
                           where
                             auxMonom = M.elemAt i (_terms poly)
                             len = (M.size (_terms poly)) - 1
                             gcdpol = gcdPolynomial' poly 0 sN -- Obtenemos el factor commun de todos los monomios que conforman el polinomio

-- Funcion que obtiene el gcd de un polinomio, en este caso se refiere al termino en comun de todos los monomios que conforman el polinomio
gcdPolynomial' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
              => OrderedPolynomial k order n  -> Int  -> SNat n -> OrderedMonomial order n
gcdPolynomial' poly i sN
                  | i == n =  gcdMonomial (fst (M.elemAt i (_terms poly))) (fst (M.elemAt (i) (_terms poly)))
                  | i /= n = gcdMonomial (fst (M.elemAt i (_terms poly))) (gcdPolynomial' poly (i+1) sN)
                  where
                     n = (M.size (_terms poly)) - 1 -- transformamos el sNat n que nos indica el numero de variables que tiene a un Int



-------- ZONA DE PREUBAS ---------------------
numVar :: SNat 3
numVar = sing

numVarD :: SNat 2
numVarD = sing

[x,y,z] = vars

p1 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
p1 = x * y^2 + x^3 * y + x^3 * y^2 * z + x^3 * y^2 * z^2 + x^3 * y^2 * z^3

p2 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
p2 = z + 1

p3 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
p3 = (x)^2 + (y - 1 )^2 + (z)^2 - 4
---------------------------------------------------------------------------------

--Ideal 2---------------------------------------------------------------------
p4 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
p4 = y^2 - x^2 - x^3

p5 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
p5 = x^2 + y^2 -1

p6 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
p6 = y*x^2 + 2*x^2  + 5*y*x^2
------------------------------------------------------------------------------

problem_chain = ascendentChain [p4,p5] [] numVar 0 3


------------------------PROJECT-QUADRICS---------------------------
q1 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
q1 = x^2 +y^2 +(z+5)^2 -25

q2 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
q2 = x^2 + y^2 + (z-5)^2 -1

q3 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
q3 = x^2 + (y-5)^2 + z^2 - 4


q4 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
q4 = (x-5)^2 + (y+3)^2 + (z-2)^2  -9
------------------------PROJECT----------------------------


chainq = ascendentChain [q1,q2,q3,q4] [] numVar 0 3

-----------------------------------------------------

-------------------FERCHO----------------------------------

[x1,y1,x2,y2,xc,yc,l1,l2,r] = vars

f1 :: OrderedPolynomial Rational (ProductOrder 1 8 Lex Lex) 9
f1 = 3*(x1-2)^2 + 20*y1^2 - 6

f2 :: OrderedPolynomial Rational (ProductOrder 1 8 Lex Lex) 9
f2 = (x2-6)^2 + y2^2 - 1

f3 :: OrderedPolynomial Rational (ProductOrder 1 8 Lex Lex) 9
f3 = x1 + l1*(6*x1 -12) -xc

f4 :: OrderedPolynomial Rational (ProductOrder 1 8 Lex Lex) 9
f4 = y1 + l1*(40*y1) - yc

f5 :: OrderedPolynomial Rational (ProductOrder 1 8 Lex Lex) 9
f5 = x2 + l2*(2*x2 -12) -xc

f6 :: OrderedPolynomial Rational (ProductOrder 1 8 Lex Lex) 9
f6 = y2 + l2*(2*y2) - yc

f7 :: OrderedPolynomial Rational (ProductOrder 1 8 Lex Lex) 9
f7 = 17*yc^2 + 24*xc -99

f8 :: OrderedPolynomial Rational (ProductOrder 1 8 Lex Lex) 9
f8 = (xc-x1)^2 + (yc-y1)^2 -r^2

f9 :: OrderedPolynomial Rational (ProductOrder 1 8 Lex Lex) 9
f9 = (xc-x2)^2 + (yc-y2)^2 -r^2

monIde :: OrderedPolynomial Rational (ProductOrder 1 8 Lex Lex) 9
monIde =
    let [x1,y1,x2,y2,xc,yc,l1,l2,r] = vars
    in 1

sNine :: SNat 9
sNine = sing

main :: IO()
main = do
    putStrLn "\n Chain 2D"
    print problem_chain
    putStrLn "\n Chain Cuadrics"
    print chainq
