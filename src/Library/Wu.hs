{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}

module Library.Wu
(
    Polynomial',
    characteristicWuSet,
    characteristicWuSingleton,
    characteristicWuSetWithStop
) where

import Algebra.Prelude
import Library.Mon
import Library.PolyClass

type Polynomial' n = OrderedPolynomial Integer Lex n

------
-- Funcion que calcula el spolynomial factorizando y simplificando el resultado
sPolynomial' :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
            => OrderedPolynomial k order n  -> OrderedPolynomial k order n -> Int -> OrderedPolynomial k order n
sPolynomial' f g i = simplifyTerm (toPolynomial (h `tryDiv'` (one, commonLeadf )) * (simplifyMonomial factorsg) * f - toPolynomial (h `tryDiv'` (one, commonLeadg ) ) * (simplifyMonomial factorsf)* g)
                        where
                        h = (one, lcmMonomial (leadingMonomial' f i) (leadingMonomial' g i))
                        factorsg = chooseTermsWithVar g i
                        factorsf = chooseTermsWithVar f i
                        commonLeadf = commonMonomial factorsf  -- Obtiene el factor comun de la variable de clase del polinomio f
                        commonLeadg = commonMonomial factorsg -- Obtiene el factor comun de la variable de clase del polinomio g


pseudoRemainder :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
                    => Int -> OrderedPolynomial k order n -> OrderedPolynomial k order n -> OrderedPolynomial k order n
pseudoRemainder var g f
                | classVarDeg f var < classVarDeg g var || classVarDeg g var == 0 = f
                | otherwise = pseudoRemainder var g (sPolynomial' f g var)

------------------FUNCION QUE OBTIENE LOS PSEUDOREMAINDERS DE UN CONJUNTO DE POLINOMIOS
getPseudoRemainders :: (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> Int -> [OrderedPolynomial k order n]
getPseudoRemainders pols var = map (pseudoRemainder var divisor) dividens
        where
                divisor = minimalPolyWithVar pols var
                dividens = dividendPolys pols var

-- En el nuevo inverted psuedoremainders se tiene encuenta la posicion de los elementos para los cuales se esta dividiendo el polinomio
invPseudoRemainders :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> OrderedPolynomial k order n -> [OrderedPolynomial k order n]
invPseudoRemainders pols pol = map (foo pol) polinomials
        where
                foo = \g f -> pseudoRemainder (snd $ f) (fst $ f) g
                polinomials = zip pols [0..]


characteristicWuSet ::  (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] ->  [OrderedPolynomial k order n] -> Int -> [OrderedPolynomial k order n]
characteristicWuSet [a] _ var = [a]
characteristicWuSet polys [] var = (basisPoly: characteristicWuSet pseudos [basisPoly] (var+1))
        where
        -- We compute the minimal polynomial of the set
        minimalPoly = minimalPolyWithVar polys var
        -- We obtain the basis polynomial of the set
        basisPoly = minimalPolyWithVar (minimalPoly: (getPseudoRemainders polys var) ) var
        -- We compute the pseudo remainders for the next iteration
        pseudos = map (\p -> if p == basisPoly && minimalPoly /=  basisPoly then pseudoRemainder var basisPoly minimalPoly else pseudoRemainder var basisPoly p) (getPseudoRemainders polys var)
characteristicWuSet polys oldChain var =  (basisPoly : characteristicWuSet pseudos (oldChain ++ [basisPoly]) (var+1) )
        where
        -- We compute the minimal Polynomial of the set
        minimalPoly = minimalPolyWithVar polys var
        -- We compute the basis polynomial of the set
        basisPoly = minimalPolyWithVar (invPseudoRemainders oldChain minimalPoly) var
        -- We compute the pseudo remainders for the next iteration
        pseudos = map (\p -> if p == basisPoly && basisPoly /= minimalPoly then pseudoRemainder var basisPoly minimalPoly else pseudoRemainder var basisPoly p) (getPseudoRemainders polys var)


characteristicWuSingleton ::  (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] ->  [OrderedPolynomial k order n] -> Int -> ([OrderedPolynomial k order n],[OrderedPolynomial k order n])
characteristicWuSingleton [a] _ var = ([a],[])
characteristicWuSingleton polys [] var = ([basisPoly], pseudos)
        where
        -- We compute the minimal polynomial of the set
        minimalPoly = minimalPolyWithVar polys var
        -- We obtain the basis polynomial of the set
        basisPoly = minimalPolyWithVar (minimalPoly: (getPseudoRemainders polys var) ) var
        -- We compute the pseudo remainders for the next iteration
        pseudos = map (\p -> if p == basisPoly && minimalPoly /=  basisPoly then pseudoRemainder var basisPoly minimalPoly else pseudoRemainder var basisPoly p) (getPseudoRemainders polys var)
characteristicWuSingleton polys oldChain var =  ((basisPoly:oldChain),  pseudos  )
        where
        -- We compute the minimal Polynomial of the set
        minimalPoly = minimalPolyWithVar polys var
        -- We compute the basis polynomial of the set
        basisPoly = minimalPolyWithVar (invPseudoRemainders oldChain minimalPoly) var
        -- We compute the pseudo remainders for the next iteration
        pseudos = map (\p -> if p == basisPoly && basisPoly /= minimalPoly then pseudoRemainder var basisPoly minimalPoly else pseudoRemainder var basisPoly p) (getPseudoRemainders polys var)




characteristicWuSetWithStop ::  (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] ->  [OrderedPolynomial k order n] -> Int -> Int -> [OrderedPolynomial k order n]
characteristicWuSetWithStop _ _ _ 0 = []
characteristicWuSetWithStop [a] _ var stop = [a]
characteristicWuSetWithStop polys [] var stop = (basisPoly: characteristicWuSetWithStop pseudos [basisPoly] (var+1) (stop -1 ))
                                where
                                -- We compute the minimal polinomial of the set
                                minimalPoly = minimalPolyWithVar polys var
                                -- We obtain the basis polynomia of the set
                                basisPoly = minimalPolyWithVar (minimalPoly: (getPseudoRemainders polys var)) var
                                -- We compute the pseudo remainders for the next iteration
                                pseudos = map (\p -> if p == basisPoly && minimalPoly /=  basisPoly then pseudoRemainder var basisPoly minimalPoly else pseudoRemainder var basisPoly p) (getPseudoRemainders polys var)
characteristicWuSetWithStop polys oldChain var stop =  (basisPoly : characteristicWuSetWithStop pseudos (oldChain ++ [basisPoly]) (var+1) (stop -1) )
                                where
                                -- We compute the minimal Polynomial of the set
                                minimalPoly = minimalPolyWithVar polys var
                                -- We compute the basis polynomial of the set
                                basisPoly = minimalPolyWithVar (invPseudoRemainders oldChain minimalPoly) var
                                -- We compute the pseudo remainders for the next iteration
                                pseudos = map (\p -> if p == basisPoly && basisPoly /= minimalPoly then pseudoRemainder var basisPoly minimalPoly else pseudoRemainder var basisPoly p) (getPseudoRemainders polys var)
