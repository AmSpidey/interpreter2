module Simplified where

flatten :: [[a]] -> [a]
flatten [] = []
flatten [[a]] = [a]
flatten (x: xs) = x++ flatten xs