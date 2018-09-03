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
            => OrderedPolynomial k order n  -> OrderedPolynomial k order n  -> SNat n -> Int -> OrderedPolynomial k order n
sPolynomial' f g n i = simplifyTerm (toPolynomial (h `tryDiv'` (one, commonLeadf )) * (simplifyMonomial factorsg) * f - toPolynomial (h `tryDiv'` (one, commonLeadg ) ) * (simplifyMonomial factorsf)* g)
                        where
                        h = (one, lcmMonomial (leadingMonomial' f n i) (leadingMonomial' g n i) )
                        factorsg = chooseTermsWithVar g n i
                        factorsf = chooseTermsWithVar f n i
                        commonLeadf = commonMonomial factorsf  -- Obtiene el factor comun de la variable de clase del polinomio f
                        commonLeadg = commonMonomial factorsg -- Obtiene el factor comun de la variable de clase del polinomio g


pseudoRemainder :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
                    => SNat n -> Int -> OrderedPolynomial k order n -> OrderedPolynomial k order n -> OrderedPolynomial k order n
pseudoRemainder sN var g f
                | classVarDeg f sN var < classVarDeg g sN var || classVarDeg g sN var == 0 = f
                | otherwise = pseudoRemainder  sN var g (sPolynomial' f g sN var)

------------------FUNCION QUE OBTIENE LOS PSEUDOREMAINDERS DE UN CONJUNTO DE POLINOMIOS
getPseudoRemainders :: (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> SNat n -> Int -> [OrderedPolynomial k order n]
getPseudoRemainders pols nat var = map (pseudoRemainder nat var divisor) dividens
        where
                divisor = minimalPolyWithVar pols nat var
                dividens = dividendPolys pols nat var

-- En el nuevo inverted psuedoremainders se tiene encuenta la posicion de los elementos para los cuales se esta dividiendo el polinomio
invPseudoRemainders :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> OrderedPolynomial k order n -> SNat n -> [OrderedPolynomial k order n]
invPseudoRemainders pols pol nat = map (foo nat pol) polinomials
        where
                foo = \sN g f -> pseudoRemainder sN (snd $ f) (fst $ f) g
                polinomials = zip pols [0..]


characteristicWuSet ::  (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] ->  [OrderedPolynomial k order n] -> SNat n -> Int -> [OrderedPolynomial k order n]
characteristicWuSet [a] _ sN var = [a]
characteristicWuSet polys [] sN var = (basisPoly: characteristicWuSet pseudos [basisPoly] sN (var+1))
        where
        -- We compute the minimal polynomial of the set
        minimalPoly = minimalPolyWithVar polys sN var
        -- We obtain the basis polynomial of the set
        basisPoly = minimalPolyWithVar (minimalPoly: (getPseudoRemainders polys sN var) ) sN var
        -- We compute the pseudo remainders for the next iteration
        pseudos = map (\p -> if p == basisPoly && minimalPoly /=  basisPoly then pseudoRemainder sN var basisPoly minimalPoly else pseudoRemainder sN var basisPoly p) (getPseudoRemainders polys sN var)
characteristicWuSet polys oldChain sN var =  (basisPoly : characteristicWuSet pseudos (oldChain ++ [basisPoly]) sN (var+1) )
        where
        -- We compute the minimal Polynomial of the set
        minimalPoly = minimalPolyWithVar polys sN var
        -- We compute the basis polynomial of the set
        basisPoly = minimalPolyWithVar (invPseudoRemainders oldChain minimalPoly sN) sN var
        -- We compute the pseudo remainders for the next iteration
        pseudos = map (\p -> if p == basisPoly && basisPoly /= minimalPoly then pseudoRemainder sN var basisPoly minimalPoly else pseudoRemainder sN var basisPoly p) (getPseudoRemainders polys sN var)


characteristicWuSingleton ::  (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] ->  [OrderedPolynomial k order n] -> SNat n -> Int -> ([OrderedPolynomial k order n],[OrderedPolynomial k order n])
characteristicWuSingleton [a] _ sN var = ([a],[])
characteristicWuSingleton polys [] sN var = ([basisPoly], pseudos)
        where
        -- We compute the minimal polynomial of the set
        minimalPoly = minimalPolyWithVar polys sN var
        -- We obtain the basis polynomial of the set
        basisPoly = minimalPolyWithVar (minimalPoly: (getPseudoRemainders polys sN var) ) sN var
        -- We compute the pseudo remainders for the next iteration
        pseudos = map (\p -> if p == basisPoly && minimalPoly /=  basisPoly then pseudoRemainder sN var basisPoly minimalPoly else pseudoRemainder sN var basisPoly p) (getPseudoRemainders polys sN var)
characteristicWuSingleton polys oldChain sN var =  ((basisPoly:oldChain),  pseudos  )
        where
        -- We compute the minimal Polynomial of the set
        minimalPoly = minimalPolyWithVar polys sN var
        -- We compute the basis polynomial of the set
        basisPoly = minimalPolyWithVar (invPseudoRemainders oldChain minimalPoly sN) sN var
        -- We compute the pseudo remainders for the next iteration
        pseudos = map (\p -> if p == basisPoly && basisPoly /= minimalPoly then pseudoRemainder sN var basisPoly minimalPoly else pseudoRemainder sN var basisPoly p) (getPseudoRemainders polys sN var)




characteristicWuSetWithStop ::  (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] ->  [OrderedPolynomial k order n] -> SNat n -> Int -> Int -> [OrderedPolynomial k order n]
characteristicWuSetWithStop _ _ _ _ 0 = []
characteristicWuSetWithStop [a] _ sN var stop = [a]
characteristicWuSetWithStop polys [] sN var stop = (basisPoly: characteristicWuSetWithStop pseudos [basisPoly] sN (var+1) (stop -1 ))
                                where
                                -- We compute the minimal polinomial of the set
                                minimalPoly = minimalPolyWithVar polys sN var
                                -- We obtain the basis polynomia of the set
                                basisPoly = minimalPolyWithVar (minimalPoly: (getPseudoRemainders polys sN var) ) sN var
                                -- We compute the pseudo remainders for the next iteration
                                pseudos = map (\p -> if p == basisPoly && minimalPoly /=  basisPoly then pseudoRemainder sN var basisPoly minimalPoly else pseudoRemainder sN var basisPoly p) (getPseudoRemainders polys sN var)
characteristicWuSetWithStop polys oldChain sN var stop =  (basisPoly : characteristicWuSetWithStop pseudos (oldChain ++ [basisPoly]) sN (var+1) (stop -1) )
                                where
                                -- We compute the minimal Polynomial of the set
                                minimalPoly = minimalPolyWithVar polys sN var
                                -- We compute the basis polynomial of the set
                                basisPoly = minimalPolyWithVar (invPseudoRemainders oldChain minimalPoly sN) sN var
                                -- We compute the pseudo remainders for the next iteration
                                pseudos = map (\p -> if p == basisPoly && basisPoly /= minimalPoly then pseudoRemainder sN var basisPoly minimalPoly else pseudoRemainder sN var basisPoly p) (getPseudoRemainders polys sN var)
