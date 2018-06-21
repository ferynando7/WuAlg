{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE NoMonomorphismRestriction, QuasiQuotes, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}


module Main where
import Algebra.Algorithms.Groebner
import Algebra.Field.Finite
import Algebra.Prelude                   hiding ((>>),(>>=))
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




type Polynomial' n = OrderedPolynomial Rational (ProductOrder 1 (n-1) Lex Lex) n

---------------DEFINICION DE LA FUNCION PSEUDOREMAINDER-----------------------------

pseudoRemainder :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
                    => SNat n -> Int -> OrderedPolynomial k order n -> OrderedPolynomial k order n -> OrderedPolynomial k order n
pseudoRemainder sN var g f
                | classVarDeg f sN var < classVarDeg g sN var || classVarDeg g sN var == 0 = f
                | otherwise = pseudoRemainder  sN var g (sPolynomial' f g sN var)
------------------------------------------------------------------------

sortPolys :: (IsOrder n order, KnownNat n, Eq k, Ord k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> [OrderedPolynomial k order n]
sortPolys [] = []
sortPolys (x:xs) =
        let     smallerOrEqual = [a | a <- xs, a <<= x]
                larger = [a | a <- xs, a >> x]
        in sortPolys smallerOrEqual ++ [x] ++ sortPolys larger



-----------DEFINCION DE LAS FUNCIONES QUE OBTIENEN EL GRADO DE LA VARIABLE DE CLASE---------------

--Returns the degree of the class variable
classVarDeg :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n  -> SNat n -> Int -> Int
classVarDeg pol nat var =  leadingMonomialDegs !! var
        where 
                leadingMonomialDegs = V.toList $ getMonomial $ leadingMonomial' pol nat var
        ---------------------------------------------------------------

------FUNCIONES PARA ACTUALIZAR LA CADENA ASCENDENTE----------------
--Funcion que obtiene el minimo polinomio con respecto a la variable de clase.
--Esto es util para saber para cual polinomio dividir

minimalPolyWithVar ::(IsOrder n order, KnownNat n, Eq k, Ord k, IsMonomialOrder n order, Euclidean k, Division k)
         => [OrderedPolynomial k order n] -> SNat n -> Int -> OrderedPolynomial k order n
minimalPolyWithVar pols nat var 
        | listOfPossiblePolys == [] = minimalPoly pols
        | otherwise = minimalPoly listOfPossiblePolys
        where listOfPossiblePolys = filter (varInPoly nat var) pols

varInPoly :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => SNat n -> Int -> OrderedPolynomial k order n -> Bool
varInPoly nat var pol 
        | classVarDeg pol nat var == 0 = False
        | otherwise = True 


minimalPoly :: (IsOrder n order, KnownNat n, Eq k, Ord k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> OrderedPolynomial k order n
minimalPoly pols = foldl1 (\acc pol -> if acc << pol then acc else pol) pols
                


--Funcion que obtiene los polinomios que seran los divisores
dividendPolys :: (IsOrder n order, KnownNat n, Eq k, Ord k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> SNat n -> Int -> [OrderedPolynomial k order n]
dividendPolys pols nat var = filter notMinPol pols
                        where
                                notMinPol = \pol -> pol /= minimalPolyWithVar pols nat var




------------------FUNCION QUE OBTIENE LOS PSEUDOREMAINDERS DE UN CONJUNTO DE POLINOMIOS
getPseudoRemainders :: (IsOrder n order, KnownNat n, Eq k, Ord k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> SNat n -> Int -> [OrderedPolynomial k order n]
getPseudoRemainders pols nat var = map (pseudoRemainder nat var divisor) dividens
        where 
                divisor = minimalPolyWithVar pols nat var
                dividens = dividendPolys pols nat var

---------FUNCION QUE OBTIENE LA CADENA ASCENDENTE-------
ascendentChain :: (IsOrder n order, KnownNat n, Eq k, Ord k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> [OrderedPolynomial k order n]  ->  [OrderedPolynomial k order n] -> SNat n -> Int -> Int -> [OrderedPolynomial k order n]
-- La funcion necesita una condicion de parada P que debe ser igual al numero de variables de los polinomios
ascendentChain polys [a] _ sN var p = [a]
  --[minimalPoly (invPseudoRemainders (ascendentChain polys [] sN 0 (i-1)) a sN i) sN i]
ascendentChain polys [] [] sN var p
                    | var == 0 && var == p = [possiblePoly ]
                    | var == 0 =  (possiblePoly: ascendentChain polys pseudos [possiblePoly] sN (var+1) p)
                    | var >= p  =  []
                    where  minPoly = minimalPolyWithVar polys sN var
                           possiblePoly = minimalPolyWithVar (minPoly : (getPseudoRemainders polys sN var) ) sN var
                           pseudos = map (\p -> if p == possiblePoly && minPoly /= possiblePoly then pseudoRemainder sN var possiblePoly minPoly else p) (getPseudoRemainders polys sN var)
ascendentChain polys pseudos oldChain sN var p
                    | var < p && var /= 0 =  (checkChainPoly : ascendentChain polys pseudos1 (oldChain ++ [checkChainPoly]) sN (var+1) p)
                    | var >= p  =  []
                    -- En caso de que var == p entonces paramos la funcion
                    where   checkChainPoly = minimalPolyWithVar (invPseudoRemainders oldChain possiblePoly sN var) sN var
                            possiblePoly = minimalPolyWithVar pseudos sN var
                            pseudos1 = map (\p -> if p == possiblePoly && checkChainPoly /= possiblePoly then pseudoRemainder sN var possiblePoly checkChainPoly else p) (getPseudoRemainders pseudos sN var)

--FUNCIONES NUEVAS--
invPseudoRemainders :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => [OrderedPolynomial k order n] -> OrderedPolynomial k order n -> SNat n -> Int -> [OrderedPolynomial k order n]
invPseudoRemainders pols pol nat var = map (foo nat var pol) pols
                where
                        foo = \sN vari g f -> pseudoRemainder sN vari f g


--Funcion que converte un arreglo de Int en un Monomio
toMonomial :: SNat n -> [Int] -> OrderedMonomial ord n
toMonomial n a = orderMonomial Proxy (fromList n a)


leadingTerm' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n -> SNat n -> Int -> (k, OrderedMonomial order n)
leadingTerm' pol nat var = (snd &&& fst) $ fromJust $ M.lookupLE chosenTerm (_terms pol)
        where
                chosenTerm = toMonomial nat (foldr1 foo polToList)
                polToList = map (V.toList . getMonomial) (M.keys $ _terms pol)
                foo = \monomCoeffs acc -> if monomCoeffs!!var > acc!!var then monomCoeffs else acc

leadingMonomial' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n -> SNat n -> Int -> OrderedMonomial order n
leadingMonomial' pol nat var = snd $ leadingTerm' pol nat var

leadingCoeff' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n -> SNat n -> Int -> k
leadingCoeff' pol nat var = fst $ leadingTerm' pol nat var


-- Funcion que calcula el spolynomial factorizando y simplificando el resultado
sPolynomial' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
           => OrderedPolynomial k order n  -> OrderedPolynomial k order n  -> SNat n -> Int -> OrderedPolynomial k order n
sPolynomial' f g n i = simplify (toPolynomial (h `tryDiv` (one, commonLeadf )) * (simplify factorsg) * f - toPolynomial (h `tryDiv` (one, commonLeadg ) ) * (simplify factorsf)* g)
                      where
                        h = (one, lcmMonomial (leadingMonomial' f n i) (leadingMonomial' g n i) )
                        factorsg = chooseTermsWithVar g n i
                        factorsf = chooseTermsWithVar f n i
                        commonLeadf = commonFactor factorsf  -- Obtiene el factor comun de la variable de clase del polinomio f
                        commonLeadg = commonFactor factorsg -- Obtiene el factor comun de la variable de clase del polinomio g



chooseTermsWithVar :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n -> SNat n -> Int -> OrderedPolynomial k order n
chooseTermsWithVar pol sN var
                | not $ varInPoly sN var pol = chooseTermsWithVar pol sN (var + 1)
                | otherwise = foldl foo 0 idxs
                where
                        deg_pol = classVarDeg pol sN var
                        idxs = findIndices (\x -> x!!var == deg_pol) (map (V.toList . getMonomial) (M.keys $ _terms pol))
                        foo = \acc idx -> acc + toPolynomial (snd $ auxMonom pol idx , fst $ auxMonom pol idx)
                        auxMonom = \poly idx -> M.elemAt idx $ _terms poly


chooseTermsWithList :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n  -> [Int] -> Int -> OrderedPolynomial k order n
chooseTermsWithList _ [] _ = 1
chooseTermsWithList pol idxs var = foldl foo 0 idxs
                where
                        foo = \acc idx -> acc + toPolynomial (snd $ auxMonom pol idx , fst $ auxMonom pol idx)
                        auxMonom = \poly idx -> M.elemAt idx $ _terms poly


                        -- Funcion que permite simplificar un polinomio en caso de que este tenga una expresion en commun en todos los monomios.
simplify :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n
simplify pol  = pol // commonFactor pol

-- Funcion que intentará dividir un polinomio por un monomio
(//) :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
         => OrderedPolynomial k order n  -> OrderedMonomial order n -> OrderedPolynomial k order n
pol // mon = sum $ map toPolynomial $ map (`tryDiv` (one, mon)) (map (snd &&& fst) terms)
         where  
                 terms = M.toList $ _terms pol

-- Funcion que obtiene el gcd de un polinomio, en este caso se refiere al termino en comun de todos los monomios que conforman el polinomio
commonFactor :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k)
        => OrderedPolynomial k order n  -> OrderedMonomial order n
commonFactor pol  = foldl1 foo terms
                        where
                                foo = \acc term -> gcdMonomial acc term
                                terms = M.keys $ _terms pol 


(<<) :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k, Ord k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n -> Bool
pol1 << pol2 = foldl1 (\acc bol -> acc && bol) bools
        where
        termsP1 = reverse $ M.toList $ _terms pol1
        termsP2 = reverse $ M.toList $ _terms pol2
        bools = zipWith (\x y -> x <= y) termsP1 termsP2
                    
(<<=) :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k, Ord k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n -> Bool
pol1 <<=  pol2 = pol1 << pol2 || pol1 == pol2

(>>) :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k, Ord k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n -> Bool
pol1 >>  pol2 = not (pol1 <<= pol2)

(>>=) :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Division k, Ord k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n -> Bool
pol1 >>=  pol2 = not (pol1 << pol2)


-------- ZONA DE PREUBAS ---------------------
sThree :: SNat 3
sThree = sing

sTwo :: SNat 2
sTwo = sing

[x,y,z] = vars

pp1 :: Polynomial' 3
pp1 = x*y

pp2 :: Polynomial' 3
pp2 = x^2*y

pp3 ::  Polynomial' 3
pp3 = x*y^2

pp4 ::  Polynomial' 3
pp4 = x*y + x 

pp5 ::  Polynomial' 3
pp5 = x^2 + z + 1

pp6 ::  Polynomial' 3
pp6 = x^2 + z + 5

pp7 ::  Polynomial' 3
pp7 = x^2*y

pp8 ::  Polynomial' 3
pp8 =  y + 9



p1 ::  Polynomial' 3
p1 = x * y^2 + x^3 * y + x^3 * y^2 * z + x^3 

p2 ::  Polynomial' 3
p2 = z+1

p3 ::  Polynomial' 3
p3 = (x)^2 + (y - 1 )^2 + (z^2) - 4 + z

---------------------------------------------------------------------------------

--Ideal 2---------------------------------------------------------------------
p4 ::  Polynomial' 3
p4 = y^2 - x^2 - x^3

p5 ::  Polynomial' 3
p5 = x^2 + y^2 -1

p6 ::  Polynomial' 3
p6 = y*x^2 + 2*x^2  + 5*y*x^2
------------------------------------------------------------------------------



------------------------PROJECT-QUADRICS---------------------------
q1 ::  Polynomial' 3
q1 = x^2 +y^2 +(z+5)^2 -25

q2 ::  Polynomial' 3
q2 = x^2 + y^2 + (z-5)^2 -1

q3 ::  Polynomial' 3
q3 = x^2 + (y-5)^2 + z^2 - 4


q4 ::  Polynomial' 3
q4 = (x-5)^2 + (y+3)^2 + (z-2)^2  -9
------------------------PROJECT----------------------------



-----------------------------------------------------

-------------------FERCHO----------------------------------

[x1,y1,x2,y2,xc,yc,l1,l2,r] = vars

f1 ::  Polynomial' 9
f1 = 3*(x1-2)^2 + 20*y1^2 - 6

f2 ::  Polynomial' 9
f2 = (x2-6)^2 + y2^2 - 1

f3 ::  Polynomial' 9
f3 = x1 + l1*(6*x1 -12) -xc

f4 ::  Polynomial' 9
f4 = y1 + l1*(40*y1) - yc

f5 ::  Polynomial' 9
f5 = x2 + l2*(2*x2 -12) -xc

f6 ::  Polynomial' 9
f6 = y2 + l2*(2*y2) - yc

f7 ::  Polynomial' 9
f7 = 17*yc^2 + 24*xc -99

f8 ::  Polynomial' 9
f8 = (xc-x1)^2 + (yc-y1)^2 -r^2

f9 ::  Polynomial' 9
f9 = (xc-x2)^2 + (yc-y2)^2 -r^2



monIde ::  Polynomial' 9
monIde =
    let [x1,y1,x2,y2,xc,yc,l1,l2,r] = vars
    in 1

sNine :: SNat 9
sNine = sing



-------------------EJEMPLO MEMOIZACION----------------------
--Sin memoizacion
fu :: Int -> Int
fu 0 = 0
fu n = max n (fu (n `div` 2) + fu (n `div` 3) + fu (n `div` 4))
-----
---Con memoizacion
f :: (Int -> Int) -> Int -> Int
f mf 0 = 0
f mf n = max n $ mf (n `div` 2) +
                 mf (n `div` 3) +
                 mf (n `div` 4)

f_list :: [Int]
f_list = map (f faster_f) [0..]

faster_f :: Int -> Int
faster_f n = f_list !! n
----------

--Se debe comparar "fu a" con "faster_f a" funciona para numeros grandes 



main :: IO()
main = do
--     putStrLn "\n Chain 2D"
--     print problem_chain
--     putStrLn "\n Chain Cuadrics"
--     print chainq
        print p1



--modifiqué la funcion getPseudoRemainders
--modifiqué la firma de la funcion pseudoRemainder