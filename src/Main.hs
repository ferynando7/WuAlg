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




--type Polynomial' n = OrderedPolynomial Integer (ProductOrder 1 (n-1) Lex Lex) n
type Polynomial' n = OrderedPolynomial Integer Lex n

---------------DEFINICION DE LA FUNCION PSEUDOREMAINDER-----------------------------


--Genera un polinomio del tipo p(x1,x2,...xn) = xi^k
mon :: SNat n -> Int -> Int -> OrderedMonomial order n
mon nat var exp = toMonomial nat exps
        where
                zeros = replicate (sNatToInt nat) 0
                exps = insertAt var exp zeros

insertAt :: Int -> Int-> [Int] -> [Int]
insertAt z y xs = as ++ (y:(tail bs))
                        where (as,bs) = splitAt z xs
------

-------------
pseudoRemainder :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
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
classVarDeg :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k)
        => OrderedPolynomial k order n  -> SNat n -> Int -> Int
classVarDeg pol nat var =  leadingMonomialDegs !! var
        where
                leadingMonomialDegs = V.toList $ getMonomial $ leadingMonomial' pol nat var
        ---------------------------------------------------------------

------FUNCIONES PARA ACTUALIZAR LA CADENA ASCENDENTE----------------
--Funcion que obtiene el minimo polinomio con respecto a la variable de clase.
--Esto es util para saber para cual polinomio dividir

minimalPolyWithVar ::(IsOrder n order, KnownNat n, Eq k, Ord k, IsMonomialOrder n order, Euclidean k)
         => [OrderedPolynomial k order n] -> SNat n -> Int -> OrderedPolynomial k order n
minimalPolyWithVar pols nat var
        | listOfPossiblePolys == [] = minimalPoly pols
        | otherwise = minimalPoly listOfPossiblePolys
        where listOfPossiblePolys = filter (varInPoly nat var) pols

varInPoly :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k)
        => SNat n -> Int -> OrderedPolynomial k order n -> Bool
varInPoly nat var pol
        | classVarDeg pol nat var == 0 = False
        | otherwise = True

varInPolys :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k)
        => SNat n -> Int -> [OrderedPolynomial k order n] -> Int
varInPolys nat var pols = foldl foo 0 pols
                where foo = \acc pol -> if varInPoly nat var pol then acc + 1 else acc

minimalPoly :: (IsOrder n order, KnownNat n, Eq k, Ord k, IsMonomialOrder n order, Euclidean k)
        => [OrderedPolynomial k order n] -> OrderedPolynomial k order n
minimalPoly pols = foldl1 (\acc pol -> if acc << pol then acc else pol) pols



--Funcion que obtiene los polinomios que seran los divisores
dividendPolys :: (IsOrder n order, KnownNat n, Eq k, Ord k, IsMonomialOrder n order, Euclidean k)
        => [OrderedPolynomial k order n] -> SNat n -> Int -> [OrderedPolynomial k order n]
dividendPolys pols nat var = filter notMinPol pols
                        where
                                notMinPol = \pol -> pol /= minimalPolyWithVar pols nat var




------------------FUNCION QUE OBTIENE LOS PSEUDOREMAINDERS DE UN CONJUNTO DE POLINOMIOS
getPseudoRemainders :: (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> SNat n -> Int -> [OrderedPolynomial k order n]
getPseudoRemainders pols nat var = map (pseudoRemainder nat var divisor) dividens
        where
                divisor = minimalPolyWithVar pols nat var
                dividens = dividendPolys pols nat var

---------FUNCION QUE OBTIENE LA CADENA ASCENDENTE (Intento fallido...)-------

ascChain :: (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> [OrderedPolynomial k order n] -> SNat n -> Int -> [OrderedPolynomial k order n]
ascChain [] chain _ _ = chain
ascChain [a] chain nat var = getNewChain chain a nat var
ascChain ideal [] nat var = ascChain newIdeal newChain nat (var+1)
        where
                newIdeal = getNewIdeal pseudos divisor minPoly nat var
                newChain = getNewChain [] divisor nat var
                pseudos = map (pseudoRemainder nat var minPoly) dividends
                minPoly = minimalPolyWithVar ideal nat var
                dividends = dividendPolys ideal nat var
                divisor = minimalPolyWithVar (minPoly:pseudos) nat var
ascChain ideal chain nat var = ascChain newIdeal newChain nat (var+1)

        where
                minPoly = minimalPolyWithVar ideal nat var
                dividends = dividendPolys ideal nat var
                pseudos = map (pseudoRemainder nat var minPoly) dividends
                divisor = minimalPolyWithVar (pseudos) nat var
                newChain = getNewChain chain divisor nat var
                newIdeal = getNewIdeal pseudos divisor (newChain!!0) nat var

getNewIdeal :: (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> OrderedPolynomial k order n -> OrderedPolynomial k order n -> SNat n -> Int -> [OrderedPolynomial k order n]
getNewIdeal oldIdeal divisor elemChain nat var = map (\a -> if a == divisor && divisor /= elemChain then pseudoRemainder nat var divisor elemChain else a) oldIdeal

--chain, divisor
getNewChain :: (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] ->  OrderedPolynomial k order n -> SNat n -> Int -> [OrderedPolynomial k order n]
getNewChain [] divisor nat var = [divisor]
getNewChain chain divisor nat var = (minInvs:invPseudos)
        where
                invPseudos = invPseudoRemainders chain divisor nat var
                minInvs = minimalPolyWithVar (invPseudos) nat var

----------------------------- FIN DE INTENTO FALLIDO






ascendentChain :: (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> [OrderedPolynomial k order n]  ->  [OrderedPolynomial k order n] -> SNat n -> Int -> [OrderedPolynomial k order n]
-- La funcion necesita una condicion de parada P que debe ser igual al numero de variables de los polinomios
ascendentChain polys [a] _ sN var = [a]
ascendentChain polys [] [] sN var =  (possiblePoly: ascendentChain polys pseudos [possiblePoly] sN (var+1))
        where
                minPoly = minimalPolyWithVar polys sN var
                possiblePoly = minimalPolyWithVar (minPoly : (getPseudoRemainders polys sN var) ) sN var
                pseudos = map (\p -> if p == possiblePoly && minPoly /= possiblePoly then pseudoRemainder sN var possiblePoly minPoly else p) (getPseudoRemainders polys sN var)
ascendentChain polys pseudos oldChain sN var =  (checkChainPoly : ascendentChain polys pseudos1 (oldChain ++ [checkChainPoly]) sN (var+1) )
                -- En caso de que var == p entonces paramos la funcion
        where
                checkChainPoly = minimalPolyWithVar (invPseudoRemainders oldChain possiblePoly sN var) sN var
                possiblePoly = minimalPolyWithVar pseudos sN var
                pseudos1 = map (\p -> if p == possiblePoly && checkChainPoly /= possiblePoly then pseudoRemainder sN var possiblePoly checkChainPoly else p) (getPseudoRemainders pseudos sN var)


ascendentChainWithConstants :: (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> [OrderedPolynomial k order n]  ->  [OrderedPolynomial k order n] -> SNat n -> Int -> Int -> [OrderedPolynomial k order n]
        -- La funcion necesita una condicion de parada P que debe ser igual al numero de variables de los polinomios
ascendentChainWithConstants polys [a] _ sN var lim = [a]
ascendentChainWithConstants _ _ _ _ _ 0 = []
ascendentChainWithConstants polys [] [] sN var lim =  (possiblePoly: ascendentChainWithConstants polys pseudos [possiblePoly] sN (var+1) (lim-1))
        where
                minPoly = minimalPolyWithVar polys sN var
                possiblePoly = minimalPolyWithVar (minPoly : (getPseudoRemainders polys sN var) ) sN var
                pseudos = map (\p -> if p == possiblePoly && minPoly /= possiblePoly then pseudoRemainder sN var possiblePoly minPoly else p) (getPseudoRemainders polys sN var)
ascendentChainWithConstants polys pseudos oldChain sN var lim =  (checkChainPoly : ascendentChainWithConstants polys pseudos1 (oldChain ++ [checkChainPoly]) sN (var+1) (lim-1))
        -- En caso de que var == p entonces paramos la funcion
        where
                checkChainPoly = minimalPolyWithVar (invPseudoRemainders oldChain possiblePoly sN var) sN var
                possiblePoly = minimalPolyWithVar pseudos sN var
                pseudos1 = map (\p -> if p == possiblePoly && checkChainPoly /= possiblePoly then pseudoRemainder sN var possiblePoly checkChainPoly else p) (getPseudoRemainders pseudos sN var)




--FUNCIONES NUEVAS--
invPseudoRemainders :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> OrderedPolynomial k order n -> SNat n -> Int -> [OrderedPolynomial k order n]
invPseudoRemainders pols pol nat var = map (foo nat var pol) pols
                where
                        foo = \sN vari g f -> pseudoRemainder sN vari f g


--Funcion que converte un arreglo de Int en un Monomio
toMonomial :: SNat n -> [Int] -> OrderedMonomial ord n
toMonomial n a = orderMonomial Proxy (fromList n a)


leadingTerm' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k)
        => OrderedPolynomial k order n -> SNat n -> Int -> (k, OrderedMonomial order n)
leadingTerm' pol nat var = (snd &&& fst) $ fromJust $ M.lookupLE chosenTerm (_terms pol)
        where
                chosenTerm = toMonomial nat (foldr1 foo polToList)
                polToList = map (V.toList . getMonomial) (M.keys $ _terms pol)
                foo = \monomCoeffs acc -> if monomCoeffs!!var > acc!!var then monomCoeffs else acc

leadingMonomial' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k)
        => OrderedPolynomial k order n -> SNat n -> Int -> OrderedMonomial order n
leadingMonomial' pol nat var = snd $ leadingTerm' pol nat var

leadingCoeff' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k)
        => OrderedPolynomial k order n -> SNat n -> Int -> k
leadingCoeff' pol nat var = fst $ leadingTerm' pol nat var

leadingAlgCoeff' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Integral k)
        => OrderedPolynomial k order n -> SNat n -> Int -> (k, OrderedMonomial order n)
leadingAlgCoeff' pol nat var = (leadingTerm' pol nat var) `tryDiv'` (1, mon nat var degVar)
                where degVar = classVarDeg pol nat var
-- Funcion que calcula el spolynomial factorizando y simplificando el resultado
sPolynomial' :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
           => OrderedPolynomial k order n  -> OrderedPolynomial k order n  -> SNat n -> Int -> OrderedPolynomial k order n
sPolynomial' f g n i = simplifyTerm (toPolynomial (h `tryDiv'` (one, commonLeadf )) * (simplifyMonomial factorsg) * f - toPolynomial (h `tryDiv'` (one, commonLeadg ) ) * (simplifyMonomial factorsf)* g)
                      where
                        h = (one, lcmMonomial (leadingMonomial' f n i) (leadingMonomial' g n i) )
                        factorsg = chooseTermsWithVar g n i
                        factorsf = chooseTermsWithVar f n i
                        commonLeadf = commonMonomial factorsf  -- Obtiene el factor comun de la variable de clase del polinomio f
                        commonLeadg = commonMonomial factorsg -- Obtiene el factor comun de la variable de clase del polinomio g



chooseTermsWithVar :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k)
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
simplifyMonomial :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n
simplifyMonomial pol = pol // (1, commonMonomial pol)

simplifyTerm :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n
simplifyTerm pol  = pol // (commonCoeff pol, commonMonomial pol)


-- Funcion que intentará dividir un polinomio por un monomio
(//) :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
         => OrderedPolynomial k order n  -> (k, OrderedMonomial order n) -> OrderedPolynomial k order n
pol // (coeff, mon) = sum $ map toPolynomial $ map (`tryDiv'` (coeff, mon)) (map (snd &&& fst) terms)
         where
                 terms = M.toList $ _terms pol

tryDiv' :: (Integral r) => (r, OrderedMonomial ord n) -> (r, OrderedMonomial ord n) -> (r, OrderedMonomial ord n)
tryDiv' (a, f) (b, g)
        | g `divs` f = (a `div` b, OrderedMonomial $ V.zipWithSame (-) (getMonomial f) (getMonomial g))
        | otherwise  = error "cannot divide."


-- Funcion que obtiene el gcd de un polinomio, en este caso se refiere al termino en comun de todos los monomios que conforman el polinomio
commonMonomial :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k)
        => OrderedPolynomial k order n  -> OrderedMonomial order n
commonMonomial pol  = foldl1 foo monomials
                        where
                                foo = \acc monomial -> gcdMonomial acc monomial
                                monomials = M.keys $ _terms pol

commonCoeff :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Integral k)
        => OrderedPolynomial k order n -> k
commonCoeff pol  = foldl1 foo coeffs
                where
                        foo = \acc coeff -> gcdCoeff acc coeff
                        coeffs = M.elems $ _terms pol

gcdCoeff :: (Integral a) => a -> a -> a
gcdCoeff a b
        | abs a > abs b = if b == 0 then abs a else abs $ gcdCoeff b $ a `mod` b
        | otherwise = if a == 0 then abs b else abs $ gcdCoeff a $ b `mod` a



compareLists :: (IsOrder n ord, Ord k)
              =>[(OrderedMonomial ord n, k)] -> [(OrderedMonomial ord n, k)] -> Bool
compareLists [] [] = False
compareLists (x:xs) (y:ys)
            |  x /= y = x < y
            | otherwise = compareLists xs ys

(<<=) :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Ord k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n -> Bool
pol1 <<= pol2 = pol1 << pol2 && pol1 == pol2


(<<) :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Ord k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n -> Bool
pol1 << pol2 = compareLists termsP1 termsP2
                where
                termsP1 = reverse $ M.toList $ _terms pol1
                termsP2 = reverse $ M.toList $ _terms pol2

(>>) :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Ord k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n -> Bool
pol1 >> pol2 = not (pol1 <<= pol2)


(>>=) :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Ord k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n -> Bool
pol1 >>  pol2 = not (pol1 << pol2)



sTwo :: SNat 2
sTwo = sing

sThree :: SNat 3
sThree = sing

sFour :: SNat 4
sFour = sing

sFive :: SNat 5
sFive = sing

sSix :: SNat 6
sSix = sing

sNine :: SNat 9
sNine = sing



-- [x,y,v,l1,x1,y1] = vars

--- EJEMPLO 2 ESFERAS 1 PARABOLOIDE
[x,y,z,v,l1,x1,y1,z1] = vars

p1 :: LabPolynomial (Polynomial' 8) '["x","y","z","v","l1","x1","y1","z1"]
p1 :: Polynomial' 8
p1 =  (x-5)^2 + (y-5)^2 + (z-3)^2  - (2 - v)^2

--Esfera: (x-10)^2 + y^2 + (z-10)^2 - 25
p2 :: Polynomial' 8
p2 =  (x-10)^2 + y^2 + (z-10)^2 - (5 - v)^2

--Paraboloid: 3*(x-6)^2 + 2(y+2)^2 + 6z
 -- Grad x
p3 :: Polynomial' 8
p3 =  x - x1 - l1 * 6 * (x1-6)

--Grad y
p4 :: Polynomial' 8
p4 = y - y1 - l1 * 4 * (y1+2)

-- Grad z
p5 :: Polynomial' 8
p5 = z - z1 - l1 * 6
-- Sphere of distance
p6 :: Polynomial' 8
p6 = (x-x1)^2 + (y-y1)^2 + (z-z1)^2 - v^2

p7 :: Polynomial' 8
p7 = 3*(x1-6)^2 + 2*(y1+2)^2 + 6*z1

-- Sphere: (x+1)^2 + (y+3)^2 + (z-5)^2 - 4
p8 :: Polynomial' 8
p8 = (x+1)^2 + (y+3)^2 + (z-5)^2 - (2-v)^2








-- --Circulo: (x-2)^(2)+ (y+5)^(2) - 4
-- --p1 :: Polynomial' 9
-- --p1 =  (x-2)^(2)+ (y+5)^(2) - (2-v)^2
--
-- -- --Circulo: p2 =  (x+3)^(2)+ (y)^(2)  - 9
-- p2 :: Polynomial' 9
-- --p2 = -5*x - 2*y - v + 16
-- -- p2 =  (x+3)^(2) + (y)^(2)  - (3-v)^2
-- p2 = x*y*v - x*y^2 - v - x - y
-- --
-- -- --Circulo: (x-4)^(2)+ (y)^(2) - 1
-- p3 :: Polynomial' 9
-- p3 = x*v - x^2 - v - y + x
-- -- p3 =  (x-4)^(2)+ (y)^(2)  - (1-v)^2
--
-- --Parabola: (x-4)^(2) - y - 1
-- p4 :: Polynomial' 9
-- p4 = v^2 - x^2 - y^2
-- --p4 = x - x1 - l1*(2*x1-8)
--
-- p5 :: Polynomial' 9
-- p5 = y - y1 + l1
--
-- p6 :: Polynomial' 9
-- p6 = (l1*(2*x1-8))^2 + (l1)^2 - v^2
--
-- p7 :: Polynomial' 9
-- p7 = (x1-4)^2 - y1 - (1)
--
-- p47 = [p4,p5,p6,p7]
--
-- --Parabola: (y+5)^2 - x - 5
-- p8 :: Polynomial' 9
-- p8 = x - x2 + l2
--
-- p9 :: Polynomial' 9
-- p9 = y - y2 - l2*(2*y2+10)
--
-- p10 :: Polynomial' 9
-- p10 = (x-x2)^2 + (y-y2)^2 - v^2
--
-- p11 :: Polynomial' 9
-- p11 = (y2+5)^2 - x2 - (5)


---- SIMBOLICO----

--n, m :: Polynomial' 2
--[n,m] = vars

--[n, m] = map (injectVar . flip Variable Nothing) "nm"


type ABCS = (LabPolynomial (Polynomial (Ratio Integer) 4) '["a", "b", "c", "S"])


sEight :: SNat 8
sEight = sing



p12 :: (LabPolynomial (Polynomial' 2) '["m", "n"])
p12 = #n^2 - #m^2 - #m^3

p13 :: (LabPolynomial (Polynomial' 2) '["m", "n"])
p13 = #m^2 + #n^2 -1

p14 :: Polynomial' 2
p14 = n^2 - m^2 - m^3

p15 :: Polynomial' 2
p15 = m^2 + n^2 -1



[m,n] = vars

monIde ::  Polynomial' 9
monIde =
    let [x1,y1,x2,y2,xc,yc,l1,l2,r] = vars
    in 1



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

-- charSet = ascendentChain [p2,p4,p5,p6,p7,p8,p9,p10,p11] [] [] sNine 0
charSet = ascendentChain [q1,q2,q3,q4] [] [] sFour 0

main :: IO()
main = do
    print charSet
--     putStrLn "\n Chain 2D"
--     print problem_chain
--     putStrLn "\n Chain Cuadrics"
--     print chainq
