{-# OPTIONS_GHC -Wall #-}

import Data.Char (digitToInt)
import Data.List (mapAccumR)

-- Exercise 1: read the digits of a number

toDigits :: Integer -> [Integer]
toDigits xs = map (toInteger . digitToInt) (show xs)

-- toDigitsRev should do the same, but with the digits reversed.
toDigitsRev :: Integer -> [Integer]
toDigitsRev xs = reverse $ map (toInteger . digitToInt) (show xs)

-- Once we have the digits in the proper order, we need to
-- double every other one.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
  let doubleOdd acc x = if odd acc then x * 2 else x
      (_, ys) = mapAccumR (\acc x -> ((acc :: Integer) + 1, doubleOdd acc x)) 0 xs
  in ys

-- The output of doubleEveryOther has a mix of one-digit and two-digit numbers.
-- Define the function
--     sumDigits :: [Integer] -> Integer
-- to calculate the sum of all digits.
sumDigits :: [Integer] -> Integer
sumDigits = sum . (concat . map toDigits)

--  indicates whether an Integer could be a valid credit card number.
validate :: Integer -> Bool
validate n = let digits    = toDigits n
                 doubled   = doubleEveryOther digits
                 summed    = sumDigits doubled
                 remainder = summed `mod` 10
             in remainder == 0
