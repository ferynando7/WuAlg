{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude  #-}

module Symbolic.Wu
(
    PolynomialSym,
    pseudoRemainderSym,
    getPseudoRemaindersSym,
    characteristicWuSetSym,
    characteristicWuSingletonSym,
    changeVariables,
    changeVariablesList,
    newAscChain,
    printPolys,
    reducePolynomial,
    simplifyNumSym,
    evaluatePoly,
    evaluatePolyList

) where

import Algebra.Prelude hiding (appendFile)
import Library.Mon
import Library.PolyClass
import qualified Data.Map.Strict as V
import Symbolic.Expr
import Util.Coeff
import System.Directory
import qualified Data.Sized.Builtin       as M
import qualified Data.Map.Strict        as MS
import System.IO (writeFile, appendFile)

type PolynomialSym n = OrderedPolynomial (Expr Integer) Lex n


-------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Funcion que saca gcd de los coeficientes de un polinomio
simplifyNumSym :: (KnownNat n )
            => PolynomialSym n -> PolynomialSym n
simplifyNumSym pol =  Polynomial $ V.fromList $ map (\(mon, coef)  -> (mon,  toExpr $ MS.map (`div` gcdnum) $ fromExpr coef) ) $ MS.toList  $ _terms pol
                where
                  coefficients =  map MS.elems $ map (fromExpr) $  map (snd) $  MS.toList $ _terms pol
                  -- Here we obtain the coefficients of the polynomial
                  gcdnum =  foldl1 gcd $ map (foldl1 gcd) $ coefficients
                  --  Here we get the gcd of the coeficients
-------------------------------------------------------------------------------------------------------------------------------------------------------------

sPolynomialSym' :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
            => OrderedPolynomial k order n  -> OrderedPolynomial k order n -> Int -> OrderedPolynomial k order n
sPolynomialSym' f g i = simplifyTermSym (toPolynomial (h `tryDiv'` (one, commonLeadf )) * (simplifyMonomial factorsg) * f - toPolynomial (h `tryDiv'` (one, commonLeadg ) ) * (simplifyMonomial factorsf)* g)
                        where
                        h = (one, lcmMonomial (leadingMonomial' f i) (leadingMonomial' g i) )
                        factorsg = chooseTermsWithVar g i
                        factorsf = chooseTermsWithVar f i
                        commonLeadf = commonMonomial factorsf  -- Obtiene el factor comun de la variable de clase del polinomio f
                        commonLeadg = commonMonomial factorsg -- Obtiene el factor comun de la variable de clase del polinomio g

simplifyTermSym :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
        => OrderedPolynomial k order n -> OrderedPolynomial k order n
simplifyTermSym pol  = pol // (one, commonMonomial pol)

pseudoRemainderSym :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
                    => Int -> OrderedPolynomial k order n -> OrderedPolynomial k order n -> OrderedPolynomial k order n
pseudoRemainderSym var g f
                | classVarDeg f var < classVarDeg g var || classVarDeg g var == 0 = f
                | otherwise = pseudoRemainderSym var g (sPolynomialSym' f g var)

------------------FUNCION QUE OBTIENE LOS pseudoRemainderSymS DE UN CONJUNTO DE POLINOMIOS
getPseudoRemaindersSym :: (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> Int -> [OrderedPolynomial k order n]
getPseudoRemaindersSym pols var = map (pseudoRemainderSym var divisor) dividends
        where
                divisor = minimalPolyWithVar pols var
                dividends = dividendPolys pols var

-- En el nuevo inverted psuedoremainders se tiene encuenta la posicion de los elementos para los cuales se esta dividiendo el polinomio
invPseudoRemaindersSym :: (IsOrder n order, KnownNat n, Eq k, Num k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> OrderedPolynomial k order n -> [OrderedPolynomial k order n]
invPseudoRemaindersSym pols pol = map (foo pol) polinomials
        where
                foo = \g f -> pseudoRemainderSym (snd $ f) (fst $ f) g
                polinomials = zip pols [0..]


characteristicWuSetSym ::  (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] ->  [OrderedPolynomial k order n] -> Int -> [OrderedPolynomial k order n]
characteristicWuSetSym [a] _ var = [a]
characteristicWuSetSym polys [] var = (basisPoly: characteristicWuSetSym pseudos [basisPoly] (var+1))
        where
        -- We compute the minimal polynomial of the set
        minimalPoly = minimalPolyWithVar polys var
        -- We obtain the basis polynomial of the set
        basisPoly = minimalPolyWithVar (minimalPoly: (getPseudoRemaindersSym polys var)) var
        -- We compute the pseudo remainders for the next iteration
        pseudos = map (\p -> if p == basisPoly && minimalPoly /=  basisPoly then pseudoRemainderSym var basisPoly minimalPoly else pseudoRemainderSym var basisPoly p) (getPseudoRemaindersSym polys var)
characteristicWuSetSym polys oldChain var =  (basisPoly : characteristicWuSetSym pseudos (oldChain ++ [basisPoly]) (var+1) )
        where
        -- We compute the minimal Polynomial of the set
        minimalPoly = minimalPolyWithVar polys var
        -- We compute the basis polynomial of the set
        basisPoly = minimalPolyWithVar (invPseudoRemaindersSym oldChain minimalPoly) var
        -- We compute the pseudo remainders for the next iteration
        pseudos = map (\p -> if p == basisPoly && basisPoly /= minimalPoly then pseudoRemainderSym var basisPoly minimalPoly else pseudoRemainderSym var basisPoly p) (getPseudoRemaindersSym polys var)


characteristicWuSingletonSym ::  (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] ->  [OrderedPolynomial k order n] -> Int -> ([OrderedPolynomial k order n],[OrderedPolynomial k order n])
characteristicWuSingletonSym [a] _ var = ([a],[])
characteristicWuSingletonSym polys [] var = ([basisPoly], pseudos)
        where
        -- We compute the minimal polynomial of the set
        minimalPoly = minimalPolyWithVar polys var
        -- We obtain the basis polynomial of the set
        basisPoly = minimalPolyWithVar (minimalPoly: (getPseudoRemaindersSym polys var)) var
        -- We compute the pseudo remainders for the next iteration
        pseudos = map (\p -> if p == basisPoly && minimalPoly /=  basisPoly then pseudoRemainderSym var basisPoly minimalPoly else pseudoRemainderSym var basisPoly p) (getPseudoRemaindersSym polys var)
characteristicWuSingletonSym polys oldChain var =  ((basisPoly:oldChain),  pseudos  )
        where
        -- We compute the minimal Polynomial of the set
        minimalPoly = minimalPolyWithVar polys var
        -- We compute the basis polynomial of the set
        basisPoly = minimalPolyWithVar (invPseudoRemaindersSym oldChain minimalPoly) var
        -- We compute the pseudo remainders for the next iteration
        pseudos = map (\p -> if p == basisPoly && basisPoly /= minimalPoly then pseudoRemainderSym var basisPoly minimalPoly else pseudoRemainderSym var basisPoly p) (getPseudoRemaindersSym polys var)




characteristicWuSetWithStopSym ::  (IsOrder n order, KnownNat n, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] ->  [OrderedPolynomial k order n] -> Int -> Int -> [OrderedPolynomial k order n]
characteristicWuSetWithStopSym _ _ _ 0 = []
characteristicWuSetWithStopSym [a] _ var stop = [a]
characteristicWuSetWithStopSym polys [] var stop = (basisPoly: characteristicWuSetWithStopSym pseudos [basisPoly] (var+1) (stop -1 ))
                                where
                                -- We compute the minimal polinomial of the set
                                minimalPoly = minimalPolyWithVar polys var
                                -- We obtain the basis polynomia of the set
                                basisPoly = minimalPolyWithVar (minimalPoly: (getPseudoRemaindersSym polys var)) var
                                -- We compute the pseudo remainders for the next iteration
                                pseudos = map (\p -> if p == basisPoly && minimalPoly /=  basisPoly then pseudoRemainderSym var basisPoly minimalPoly else pseudoRemainderSym var basisPoly p) (getPseudoRemaindersSym polys var)
characteristicWuSetWithStopSym polys oldChain var stop =  (basisPoly : characteristicWuSetWithStopSym pseudos (oldChain ++ [basisPoly]) (var+1) (stop -1) )
                                where
                                -- We compute the minimal Polynomial of the set
                                minimalPoly = minimalPolyWithVar polys var
                                -- We compute the basis polynomial of the set
                                basisPoly = minimalPolyWithVar (invPseudoRemaindersSym oldChain minimalPoly) var
                                -- We compute the pseudo remainders for the next iteration
                                pseudos = map (\p -> if p == basisPoly && basisPoly /= minimalPoly then pseudoRemainderSym var basisPoly minimalPoly else pseudoRemainderSym var basisPoly p) (getPseudoRemaindersSym polys var)



---------------------------------------
-- Cambia los coeficientes de un polinimo por unos coeficientes nuevos
changeVariables :: (KnownNat n) => PolynomialSym n -> Int -> Coeff -> PolynomialSym n
changeVariables pol step  coeff = Polynomial $ V.fromList $ zipWith (\(a,b) c -> (a,c)) (reverse $ V.toList $ terms  pol) (Algebra.Prelude.map ( fromCoeff step) [coeff ..])

-- Cambia los coeficientes de una lista de polinomios
changeVariablesList :: (KnownNat n) => [PolynomialSym n] -> Int -> Coeff -> [PolynomialSym n]
changeVariablesList [] _ _ = []
changeVariablesList (x:xs) var coeff = (newPolX : changeVariablesList xs var (succ lastCoeff))
        where
                newPolX = changeVariables x var coeff
                lastCoeff = (toCoeff . snd . head . V.toList . terms) newPolX


--Realiza el algoritmo de WU paso a paso imprimiendo los resutados necesarios en los archivos crrespondientes
newAscChain :: (KnownNat n1) => [PolynomialSym n1] -> Int -> IO ()
newAscChain [] _= return ()
newAscChain pols var = do
                placeToSaveAscChain <- fmap (++ ("/src/Results/AscChainStep"++ (show var)++".txt")) getCurrentDirectory
                placeToSaveNewSet <- fmap (++ ("/src/Results/NewSetStep"++ (show var)++".txt")) getCurrentDirectory
                placeToSaveCoeffs <- fmap (++ ("/src/Results/CoeffsStep"++ (show var)++".txt")) getCurrentDirectory
                let thingToSave = ( \(a,b) -> (map simplifyNumSym a, map simplifyNumSym b) ) $ characteristicWuSingletonSym pols [] var
                let newSet = changeVariablesList (snd thingToSave) var (Coeff "a")
                --let newSetReduced = map (reducePolynomial (sN $ (fromIntegral (numVarPolys $ head newSet) :: Nat)) nat2)  newSet
                writeCoeffs <- printCoeffs newSet (snd thingToSave) placeToSaveCoeffs
                writeAscChain <- printPolys (fst thingToSave) placeToSaveAscChain
                writeNewSet <- printPolys (snd thingToSave) placeToSaveNewSet
                newAscChain newSet (var+1)
                --(sN $ ((fromIntegral ((numVarPolys $ head newSetReduced)-1)) :: Nat ))

--Dado una lista de polinomios y un path, imprime los polinomios linea por linea en dicho path
printPolys :: (IsOrder n order, KnownNat n, PrettyCoeff k, Eq k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> FilePath -> IO ()
printPolys [] _ = return ()
printPolys (x:xs) path = do
                        a <- appendFile path $ show x ++ "\n"
                        printPolys xs path

-- toma dos listas a b, imprime sus terminos haciendolos corresponder de la siguente forma
-- a1 = b1
-- a2 = b2
--    .
--    .
-- an = bn
printList :: (Show k) => [k] -> [k] -> FilePath -> IO ()
printList [] _ _ = return ()
printList (x:xs) (y:ys) path = do
                        a <- appendFile path $ show x ++ " = " ++ show y ++ "\n"
                        printList xs ys path

printCoeffs :: (IsOrder n order, KnownNat n, Eq k, Show k, Num k, Ord k, IsMonomialOrder n order, Euclidean k, Integral k)
        => [OrderedPolynomial k order n] -> [OrderedPolynomial k order n] -> FilePath -> IO ()
printCoeffs [] [] _ = return ()
printCoeffs new@(n:ns) old@(o:os) path = do
                                        let coeffsNew = ((map (snd)) . reverse . V.toList . terms) n
                                        let coeffsOld = ((map (snd)) . reverse . V.toList . terms) o
                                        printHead <- printList coeffsNew coeffsOld path
                                        printTail <- printCoeffs ns os path
                                        return ()


reducePolynomial :: (IsOrder n1 order, KnownNat n1, Eq k, Num k, Ord k, IsMonomialOrder n1 order, Euclidean k, Integral k,
                     IsOrder n2 order, KnownNat n2 ,IsMonomialOrder n2 order)
        => SNat n1  -> SNat n2 ->OrderedPolynomial k order n1 -> OrderedPolynomial k order n2
reducePolynomial nat1 nat2 pol = Polynomial $ V.fromList $ zipWith (,) newAlgPart coeffs
--        | varInPoly nat1 0 pol /= 0 = pol
--        |otherwise = Polynomial $ V.fromList $ zipWith (,) newAlgPart coeffs
                where
                        polToList = reverse $ V.toList $ terms pol
                        algPart = map fst polToList
                        coeffs = map snd polToList
                        newAlgPart = map (toMonomial) $ (map (tail . M.toList . getMonomial)) algPart


--  Function that evaluate certain symbolic value in the polynomial
evaluatePoly :: (KnownNat n)
        => PolynomialSym n ->  String -> Integer -> PolynomialSym n
evaluatePoly poly "" _ = poly
evaluatePoly poly str val =  Polynomial $ V.fromList  $ evaluateCoef polyList
                where
                        polyList = MS.toList $ _terms poly
                        evaluateCoef polist = map (\(mon, coeff) ->  (mon, evaluate coeff str val)) polist

-- Function that evaluate a set of symbolic values in the polynomial
evaluatePolyList :: (KnownNat n)
        => PolynomialSym n ->  [(String,Integer)] ->  PolynomialSym n
evaluatePolyList poly [] = poly
evaluatePolyList poly values =  Polynomial $ V.fromList $ evaluateCoef polyList
                where
                        polyList = MS.toList $ _terms poly
                        evaluateCoef polist = map (\(mon, coeff) ->  (mon, eval coeff values)) polist
                              where
                                eval coeff [] = coeff
                                eval coeff [a] = evaluate coeff (fst a) (snd a)
                                eval coeff vals@(v:vs) = eval (evaluate coeff (fst v) (snd v)) vs
