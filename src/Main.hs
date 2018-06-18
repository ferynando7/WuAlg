{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE NoMonomorphismRestriction, QuasiQuotes, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Data.Set                 as Set
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

-----------DEFINCION DE LAS FUNCIONES QUE OBTIENEN EL GRADO DE LA VARIABLE DE CLASE---------------

--Returns the degree of the class variable
classVarDeg :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n  -> SNat n -> Int -> Int
classVarDeg pol nat pos =  leadingMonomialDegs !! pos
        where 
                leadingMonomialDegs = V.toList $ getMonomial $ leadingMonomial' pol nat pos
---------------------------------------------------------------

------FUNCIONES PARA ACTUALIZAR LA CADENA ASCENDENTE----------------
--Funcion que obtiene el minimo polinomio con respecto a la variable de clase.
--Esto es util para saber para cual polinomio dividir
minimalPoly ::(IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> SNat n -> Int -> OrderedPolynomial k order n
minimalPoly pols nat pos = foldl1 foo pols
                where
                        foo = \acc pol -> if classVarDeg pol nat pos < classVarDeg acc nat pos && classVarDeg pol nat pos>0 then pol else acc

--Funcion que obtiene los polinomios que seran los divisores
dividendPolys :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> SNat n -> Int -> [OrderedPolynomial k order n]
dividendPolys pols nat pos = filter notMinPol pols
                        where
                                notMinPol = \pol -> pol /= minimalPoly pols nat pos


------------------FUNCION QUE OBTIENE LOS PSEUDOREMAINDERS DE UN CONJUNTO DE POLINOMIOS
getPseudoRemainders :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> SNat n -> Int -> [OrderedPolynomial k order n]
getPseudoRemainders [a] _ _ = []
getPseudoRemainders polys sN i =
    let minPoly = minimalPoly polys sN i
        dividend = dividendPolys polys sN i
        in ((pseudoRemainder (head(dividend)) minPoly sN i) : (getPseudoRemainders (minPoly:tail(dividend)) sN i))


---------FUNCION QUE OBTIENE LA CADENA ASCENDENTE-------
fullAscendentChain :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> SNat n-> Int -> [OrderedPolynomial k order n]
fullAscendentChain [a] _ _ = [a]
fullAscendentChain polys sN i = (minimalPoly polys sN i : fullAscendentChain (getPseudoRemainders polys sN i) sN (i+1))


ascendentChain :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> [OrderedPolynomial k order n]  -> SNat n -> Int -> Int -> [OrderedPolynomial k order n]
-- La funcion necesita una condicion de parada P que debe ser igual al numero de variables de los polinomios
ascendentChain polys [a] sN i p = [a]
  --[minimalPoly (invPseudoRemainders (ascendentChain polys [] sN 0 (i-1)) a sN i) sN i]
ascendentChain polys [] sN i p
                    | i == 0 && i == p = [possiblePoly ]
                    | i == 0 =  (possiblePoly: ascendentChain polys pseudos sN (i+1) p)
                    | i >= p  =  []
                    where  minPoly = minimalPoly polys sN i
                           possiblePoly = minimalPoly (minPoly : (getPseudoRemainders polys sN i) ) sN i
                           pseudos = map (\p -> if p == possiblePoly && minPoly /= possiblePoly then pseudoRemainder minPoly possiblePoly sN i else p) (getPseudoRemainders polys sN (i))
ascendentChain polys pseudos sN i p
                    | i < p && i /= 0 =  (checkChainPoly : ascendentChain polys pseudos1 sN (i+1) p)
                    | i >= p  =  []
                    -- En caso de que i == p entonces paramos la funcion
                    where   checkChainPoly = minimalPoly (invPseudoRemainders (ascendentChain polys [] sN 0 (i-1)) possiblePoly sN i) sN i
                            possiblePoly = minimalPoly pseudos sN i
                            pseudos1 = map (\p -> if p == possiblePoly && checkChainPoly /= possiblePoly then pseudoRemainder checkChainPoly possiblePoly sN i else p) (getPseudoRemainders pseudos sN (i))


                    -- checkChainPoly obtiene el minimo polynomio entre los pseudoRemainders del polinomio candidato a ser añadido a la cadena y la cadena
                    -- possiblePoly es el polynomio candidato a ser añadido a la cadena
                    -- Si el possiblePoly no se puede dividir con respecto a ningun elemento de la cadena entonces, este pasa directamente a la cadena


--FUNCIONES NUEVAS--

invPseudoRemainders :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> OrderedPolynomial k order n -> SNat n -> Int -> [OrderedPolynomial k order n]
invPseudoRemainders [] _ _ _ = []
invPseudoRemainders (x:xs) poly sN i = ((pseudoRemainder poly x sN i) : (invPseudoRemainders xs poly sN i))

--Funcion que converte un arreglo de Int en un Monomio
toMonomial :: SNat n -> [Int] -> OrderedMonomial ord n
toMonomial n a = orderMonomial Proxy (fromList n a)


leadingTerm' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n -> SNat n -> Int -> (k, OrderedMonomial order n)
leadingTerm' pol nat pos = (snd &&& fst) $ fromJust $ M.lookupLE chosenTerm (_terms pol)
        where
                chosenTerm = toMonomial nat (foldr1 foo polToList)
                polToList = map (V.toList . getMonomial) (M.keys $ _terms pol)
                foo = \monomCoeffs acc -> if monomCoeffs!!pos > acc!!pos then monomCoeffs else acc

leadingMonomial' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n -> SNat n -> Int -> OrderedMonomial order n
leadingMonomial' pol nat pos = snd $ leadingTerm' pol nat pos

leadingCoeff' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n -> SNat n -> Int -> k
leadingCoeff' pol nat pos = fst $ leadingTerm' pol nat pos


-- Funcion que calcula el spolynomial factorizando y simplificando el resultado
sPolynomial' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
           => OrderedPolynomial k order n  -> OrderedPolynomial k order n  -> SNat n -> Int -> OrderedPolynomial k order n
sPolynomial' f g n i = simplify (toPolynomial (h `tryDiv` (one, commonLeadf )) * (simplify factorsg) * f - toPolynomial (h `tryDiv` (one, commonLeadg ) ) * (simplify factorsf)* g)
                      where
                        h = (one, lcmMonomial (leadingMonomial' f n i) (leadingMonomial' g n i) )
                        factorsg = chooseTerms g indexesg i
                        factorsf = chooseTerms f indexesf i
                        commonLeadf = if (deg_f == 0 ) then (leadingMonomial' f n i) else (gcdPolynomial' factorsf)  -- Obtiene el factor comun de la variable de clase del polinomio f
                        commonLeadg =  if (deg_g == 0 ) then (leadingMonomial' g n i) else (gcdPolynomial' factorsg) -- Obtiene el factor comun de la variable de clase del polinomio g
                        deg_f = classVarDeg f n i -- Obtiene el grado de la variable de clase del polinomio f
                        deg_g = classVarDeg g n i -- Obtiene el grado del a variable de clase del polinomio g
                        indexesf = if (deg_f == 0 ) then ([0]) else (findIndices (\x -> x!!i == deg_f) (map (\x -> V.toList (getMonomial x)) (M.keys $ _terms f))) -- Obtiene los indicies de manera ascendente del arreglo
                        indexesg = if (deg_g == 0 ) then ([0]) else (findIndices (\x -> x!!i == deg_g) (map (\x -> V.toList (getMonomial x)) (M.keys $ _terms g))) -- Obtiene los indicies de manera ascendente del arreglo

chooseTerms :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n  -> [Int] -> Int -> OrderedPolynomial k order n
chooseTerms _ [] _ = 0
chooseTerms pol idxs pos = foldl foo 0 idxs
                        where
                                foo = \acc idx -> acc + toPolynomial (snd $ auxMonom pol idx , fst $ auxMonom pol idx)
                                auxMonom = \poly idx -> M.elemAt idx $ _terms poly

-- Funcion que permite simplificar un polinomio en caso de que este tenga una expresion en commun en todos los monomios.
simplify :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n
simplify pol  = pol // gcdPolynomial' pol

-- Funcion que intentará dividir un polinomio por un monomio
(//) :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
         => OrderedPolynomial k order n  -> OrderedMonomial order n -> OrderedPolynomial k order n
pol // mon = sum $ map toPolynomial $ map (`tryDiv` (one, mon)) (map (snd &&& fst) terms)
         where  
                 terms = M.toList $ _terms pol

-- Funcion que obtiene el gcd de un polinomio, en este caso se refiere al termino en comun de todos los monomios que conforman el polinomio
gcdPolynomial' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n  -> OrderedMonomial order n
gcdPolynomial' pol  = foldl1 foo terms
                        where
                                foo = \acc term -> gcdMonomial acc term
                                terms = M.keys $ _terms pol 



-------- ZONA DE PREUBAS ---------------------
sThree :: SNat 3
sThree = sing

sTwo :: SNat 2
sTwo = sing

[x,y,z] = vars

pp1 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
pp1 = x*y

pp2 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
pp2 = x^2*y

pp3 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
pp3 = x*y^2

pp4 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
pp4 = x*y + x 



p1 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
p1 = x * y^2 + x^3 * y + x^3 * y^2 * z + x^3 * y^2 * z^2 + x^3 * y^2 * z^3

p2 :: OrderedPolynomial Rational (ProductOrder 1 2 Lex Lex) 3
p2 = z+1

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

problem_chain = ascendentChain [p4,p5] [] sThree 0 3


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


chainq = ascendentChain [q1,q2,q3,q4] [] sThree 0 3

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
--     putStrLn "\n Chain 2D"
--     print problem_chain
--     putStrLn "\n Chain Cuadrics"
--     print chainq
        print chainq
