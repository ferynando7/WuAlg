{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude #-}

module Library.Mon
(
    toMonomial,
    mon,
    tryDiv'
) 
where

import Algebra.Prelude   
import qualified Data.Sized.Builtin     as V

        
--Funcion que converte un arreglo de Int en un Monomio
-- toMonomial :: SNat n -> [Int] -> OrderedMonomial ord n
-- toMonomial n a = orderMonomial Proxy (fromList n a)

toMonomial :: (KnownNat n) => [Int] -> OrderedMonomial ord n
toMonomial a = orderMonomial Proxy (fromList sing a)


--Genera un polinomio del tipo p(x1,x2,...xn) = xi^k
mon :: (IsOrder n order, KnownNat n, IsMonomialOrder n order)
        => Int -> Int -> Int -> OrderedMonomial order n
mon var exp numVars = toMonomial exps
        where 
                zeros = replicate numVars 0
                exps = insertAt var exp zeros

insertAt :: Int -> Int-> [Int] -> [Int] 
insertAt z y xs = as ++ (y:(tail bs))
                        where (as,bs) = splitAt z xs
-----

tryDiv' :: (Integral r) => (r, OrderedMonomial ord n) -> (r, OrderedMonomial ord n) -> (r, OrderedMonomial ord n)
tryDiv' (a, f) (b, g)
        | g `divs` f = (a `div` b, OrderedMonomial $ V.zipWithSame (-) (getMonomial f) (getMonomial g))
        | otherwise  = error "cannot divide."
