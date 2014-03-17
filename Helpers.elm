module Helpers where

(#) : [a] -> Int -> Maybe a
xs # n = case xs of
           []     -> Nothing
           (h::t) -> if | n < 0     -> Nothing
                        | n == 0    -> Just h
                        | otherwise -> t # (n-1)
