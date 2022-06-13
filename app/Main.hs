module Main where

-- Integer square root
intSqrt = floor . sqrt . fromIntegral

-- Excluding even divisors greater than 2
divisors i = [x | x <- [2..intSqrt i], (x == 2 || odd x) && i `mod` x == 0]

isPrime i = i > 1 && null (divisors i)

main = print [x | x <- [0..1000], isPrime x]
