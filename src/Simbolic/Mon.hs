{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude #-}

module Simbolic.Mon
(
    toMonomial,
    mon,
    tryDiv'
) 
where

import Algebra.Prelude   
import qualified Data.Sized.Builtin     as V


--Funcion que converte un arreglo de Int en un Monomio
toMonomial :: SNat n -> [Int] -> OrderedMonomial ord n
toMonomial n a = orderMonomial Proxy (fromList n a)


--Genera un polinomio del tipo p(x1,x2,...xn) = xi^k
mon :: SNat n -> Int -> Int -> OrderedMonomial order n
mon nat var exp = toMonomial nat exps
        where 
                zeros = replicate (sNatToInt nat) 0
                exps = insertAt var exp zeros

insertAt :: Int -> Int-> [Int] -> [Int] 
insertAt z y xs = as ++ (y:(tail bs))
                        where (as,bs) = splitAt z xs
-----

tryDiv' :: (Integral r) => (r, OrderedMonomial ord n) -> (r, OrderedMonomial ord n) -> (r, OrderedMonomial ord n)
tryDiv' (a, f) (b, g)
        | g `divs` f = (a `div` b, OrderedMonomial $ V.zipWithSame (-) (getMonomial f) (getMonomial g))
        | otherwise  = error "cannot divide."
