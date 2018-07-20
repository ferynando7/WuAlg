{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}

module Library.Wu
(
    Polynomial',
    ascendentChain,
    ascendentChainWithConstants
) where

import Algebra.Prelude
import Library.Mon
import Library.PolyClass

type Polynomial' n = OrderedPolynomial Integer Lex n

------

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

invPseudoRemainders :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> OrderedPolynomial k order n -> SNat n -> Int -> [OrderedPolynomial k order n]
invPseudoRemainders pols pol nat var = map (foo nat var pol) pols
        where
                foo = \sN vari g f -> pseudoRemainder sN vari f g
        

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






