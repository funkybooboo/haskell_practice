module Utils where

  factorial :: Integer -> Integer
  factorial 0 = 1
  factorial n = n * factorial (n - 1)

  take :: Integer -> [a] -> [a]
  take 0 x = []
  take x [] = []
  take n (x:xs) = x : take (n - 1) xs

  squareAll :: Num a => [a] -> [a]
  squareAll xs [x * x | x <- xs]

  firstSquares :: Num a => a -> [a] -> [a]
  firstSquares n xs = take n (squareAll xs)

  map :: (a -> b) -> [a] -> [b]
  map fn [] = []
  map fn (x:xs) = fn x : map fn xs

  filter :: (a -> Bool) -> [a] -> [a]
  filter even xs

  zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
  zipWith (+) xs ys

  -- fibonacci numbers
  fibonacci :: Integer -> Integer
  fibonacci 0 = 0
  fibonacci 1 = 1
  fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

  -- elem function (does a list contain a given element?)
  elem :: a -> [a] -> Bool
  elem x [] = False
  elem x (y:ys) | x == y = True
                | otherwise = elem x ys

  -- sorted function (is a list sorted or not?)
  check :: Num -> [Num] -> Bool
  check x [] = True
  check x (y:ys) | x <= y = check y ys
                 | otherwise False

  sorted :: [Num] -> Bool
  sorted [] = True
  sorted (x:xs) = check x xs