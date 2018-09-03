{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}

module Symbolic.Wu
(
    PolynomialSym,
    pseudoRemainderSym,
    getPseudoRemaindersSym,
    characteristicWuSet,
    characteristicWuSingleton

) where

import Algebra.Prelude
import Library.Mon
import Library.PolyClass
import Library.Wu
import Symbolic.Expr

type PolynomialSym n = OrderedPolynomial (Expr Integer) Lex n


sPolynomialSym' :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
            => OrderedPolynomial k order n  -> OrderedPolynomial k order n  -> SNat n -> Int -> OrderedPolynomial k order n
sPolynomialSym' f g n i = (toPolynomial (h `tryDiv'` (one, commonLeadf )) * (simplifyMonomial factorsg) * f - toPolynomial (h `tryDiv'` (one, commonLeadg ) ) * (simplifyMonomial factorsf)* g)
                        where
                        h = (one, lcmMonomial (leadingMonomial' f n i) (leadingMonomial' g n i) )
                        factorsg = chooseTermsWithVar g n i
                        factorsf = chooseTermsWithVar f n i
                        commonLeadf = commonMonomial factorsf  -- Obtiene el factor comun de la variable de clase del polinomio f
                        commonLeadg = commonMonomial factorsg -- Obtiene el factor comun de la variable de clase del polinomio g


pseudoRemainderSym :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
                    => SNat n -> Int -> OrderedPolynomial k order n -> OrderedPolynomial k order n -> OrderedPolynomial k order n
pseudoRemainderSym sN var g f
                | classVarDeg f sN var < classVarDeg g sN var || classVarDeg g sN var == 0 = f
                | otherwise = pseudoRemainderSym  sN var g (sPolynomialSym' f g sN var)

------------------FUNCION QUE OBTIENE LOS PSEUDOREMAINDERS DE UN CONJUNTO DE POLINOMIOS
getPseudoRemaindersSym :: (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> SNat n -> Int -> [OrderedPolynomial k order n]
getPseudoRemaindersSym pols nat var = map (pseudoRemainderSym nat var divisor) dividends
        where
                divisor = minimalPolyWithVar pols nat var
                dividends = dividendPolys pols nat var

-- En el nuevo inverted psuedoremainders se tiene encuenta la posicion de los elementos para los cuales se esta dividiendo el polinomio
invPseudoRemaindersSym :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> OrderedPolynomial k order n -> SNat n -> [OrderedPolynomial k order n]
invPseudoRemaindersSym pols pol nat = map (foo nat pol) polinomials
        where
                foo = \sN g f -> pseudoRemainderSym sN (snd $ f) (fst $ f) g
                polinomials = zip pols [0..]


characteristicWuSetSym ::  (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] ->  [OrderedPolynomial k order n] -> SNat n -> Int -> [OrderedPolynomial k order n]
characteristicWuSetSym = characteristicWuSet

characteristicWuSingletonSym ::  (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] ->  [OrderedPolynomial k order n] -> SNat n -> Int -> ([OrderedPolynomial k order n],[OrderedPolynomial k order n])
characteristicWuSingletonSym = characteristicWuSingleton

characteristicWuSetWithStopSym ::  (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] ->  [OrderedPolynomial k order n] -> SNat n -> Int -> Int -> [OrderedPolynomial k order n]
characteristicWuSetWithStopSym = characteristicWuSetWithStop
