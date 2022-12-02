{-
Problem 1: Multiples of 3 or 5
URL: https://projecteuler.net/problem=1

    If we list all the natural numbers below 10 that are multiples of
    3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

    Find the sum of all the multiples of 3 or 5 below 1000.
-}
module PE001(main) where

-- Checks if the LHS is a multiple of the RHS.
isMultipleOf :: (Integral a) => a -> a -> Bool
isMultipleOf left right = (left `mod` right) == 0

-- Checks if a number is multiple of 3 or 5.
isMultipleOfThreeOrFive :: (Integral a) => a -> Bool
isMultipleOfThreeOrFive number = any predicate [3, 5]
    where predicate divisor = number `isMultipleOf` divisor

-- Gets the multiples of 3 or 5 bellow a limit number.
multiplesOfThreeOrFive :: (Integral a) => a -> [a]
multiplesOfThreeOrFive limit = takeWhile (< limit) numbers
    where numbers = [n | n <- [1,2..], isMultipleOfThreeOrFive n]

-- Main entry for the problem solver.
main :: IO()
main = do putStr "The sum of all the multiples of 3 or 5 "
          putStrLn ("below 1000 is " ++ show result ++ ".")
    where result = sum $ multiplesOfThreeOrFive 1000
