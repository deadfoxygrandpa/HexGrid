module Helpers where

replace : Int -> a -> [a] -> [a]
replace n x xs = if (n < 0 || n > (length xs)) then xs else
    let (ys, zs) = splitAt n xs
        zs'      = drop 1 zs
    in  ys ++ (x :: zs')

splitAt : Int -> [a] -> ([a], [a])
splitAt n xs =  (take n xs, drop n xs)

(#) : [a] -> Int -> Maybe a
xs # n = case xs of
           []     -> Nothing
           (h::t) -> if | n < 0     -> Nothing
                        | n == 0    -> Just h
                        | otherwise -> t # (n-1)
