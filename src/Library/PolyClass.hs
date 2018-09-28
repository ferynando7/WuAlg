{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude #-}

module Library.PolyClass
(
    minimalPolyWithVar,
    simplifyTerm,
    lcmMonomial,
    toPolynomial,
    simplifyMonomial,
    leadingMonomial',
    commonMonomial,
    chooseTermsWithVar,
    varInPoly,
    numVarPolys,
    dividendPolys,
    classVarDeg
) where

import Algebra.Prelude                  hiding ((>>),(>>=))
import qualified Data.Map.Strict        as M
import qualified Data.Sized.Builtin     as V
import Data.Maybe                       (fromJust)
import Library.PolyCompare
import Library.Mon

--Quicksort for polys
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
        => OrderedPolynomial k order n -> Int -> Int
classVarDeg pol var =  leadingMonomialDegs !! var
        where
                leadingMonomialDegs = V.toList $ getMonomial $ leadingMonomial' pol var

                ---------------------------------------------------------------
------FUNCIONES PARA ACTUALIZAR LA CADENA ASCENDENTE----------------
--Funcion que obtiene el minimo polinomio con respecto a la variable de clase.
--Esto es util para saber para cual polinomio dividir

minimalPolyWithVar ::(IsOrder n order, KnownNat n, Eq k, Ord k, IsMonomialOrder n order, Euclidean k)
         => [OrderedPolynomial k order n] -> Int -> OrderedPolynomial k order n
minimalPolyWithVar pols var
        | listOfPossiblePolys == [] = minimalPoly pols
        | otherwise = minimalPoly listOfPossiblePolys
        where listOfPossiblePolys = filter (varInPoly var) pols

numVarPolys :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k) 
        => OrderedPolynomial k order n -> Int
numVarPolys pol =  length $ getMonomial $ fst $ head $ M.toList $ terms pol


varInPoly :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k)
        => Int -> OrderedPolynomial k order n -> Bool
varInPoly var pol
        | classVarDeg pol var == 0 = False
        | otherwise = True

varInPolys :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k)
        => Int -> [OrderedPolynomial k order n] -> Int
varInPolys var pols = foldl foo 0 pols
                where foo = \acc pol -> if varInPoly var pol then acc + 1 else acc

minimalPoly :: (IsOrder n order, KnownNat n, Eq k, Ord k, IsMonomialOrder n order, Euclidean k)
        => [OrderedPolynomial k order n] -> OrderedPolynomial k order n
minimalPoly pols = foldl1 (\acc pol -> if acc << pol then acc else pol) pols


--Funcion que obtiene los polinomios que seran los divisores
dividendPolys :: (IsOrder n order, KnownNat n, Eq k, Ord k, IsMonomialOrder n order, Euclidean k)
        => [OrderedPolynomial k order n] -> Int -> [OrderedPolynomial k order n]
dividendPolys pols var = filter notMinPol pols
                        where
                                notMinPol = \pol -> pol /= minimalPolyWithVar pols var

leadingTerm' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k)
        => OrderedPolynomial k order n -> Int -> (k, OrderedMonomial order n)
leadingTerm' pol var = (snd &&& fst) $ fromJust $ M.lookupLE chosenTerm (_terms pol)
        where
                chosenTerm = toMonomial (foldr1 foo polToList)
                polToList = map (V.toList . getMonomial) (M.keys $ _terms pol)
                foo = \monomCoeffs acc -> if monomCoeffs!!var > acc!!var then monomCoeffs else acc

leadingMonomial' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k)
        => OrderedPolynomial k order n -> Int -> OrderedMonomial order n
leadingMonomial' pol var = snd $ leadingTerm' pol var

leadingCoeff' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k)
        => OrderedPolynomial k order n -> Int -> k
leadingCoeff' pol var = fst $ leadingTerm' pol var

--Takes de leading coef with the non-class variables
leadingAlgCoeff' :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k, Integral k)
        => OrderedPolynomial k order n -> Int -> (k, OrderedMonomial order n)
leadingAlgCoeff' pol var = (leadingTerm' pol var) `tryDiv'` (1, mon var degVar (numVarPolys pol))
                where degVar = classVarDeg pol var

chooseTermsWithVar :: (IsOrder n order, KnownNat n, Eq k, IsMonomialOrder n order, Euclidean k)
        => OrderedPolynomial k order n -> Int -> OrderedPolynomial k order n
chooseTermsWithVar pol var
                | not $ varInPoly var pol = chooseTermsWithVar pol (var + 1)
                | otherwise = foldl foo 0 idxs
                where
                        deg_pol = classVarDeg pol var
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


