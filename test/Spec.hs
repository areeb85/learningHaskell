{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}

import Generics.SYB

main :: IO ()


factorial :: (Integral a) => a -> a 
factorial 0 = 1 
factorial n = n * factorial(n-1)

sampleBool ::  Bool -> String
sampleBool val = case val of 
  True -> "True"
  False -> "False"

{-# NOINLINE sampleMaybeBool #-}
sampleMaybeBool :: Maybe Bool -> String
sampleMaybeBool val = case val of 
--  Just x -> case True of 
--    True -> "True"
--    False -> "False"
  Just x -> sampleBool True
  Nothing -> "Nothing"

sampleMaybeBool2 :: Maybe Bool -> String
sampleMaybeBool2 val = case val of 
  Just True -> "True"
  _ -> "Nothing"
  
foldIntsTest = 3 + 4

foo = let x = 4 in 
        let y = x in 
          let z v = v + 1 in
          show (z y) ++ show (z y)




barInput :: [Int]
barInput = [1, 2, 3, 4]
{-# SPECIALIZE mySum :: [Int] -> Int#-}
mySum :: (Num n) => [n] -> n
mySum [] = 0
mySum (x : y) = x + mySum y
baz = mySum barInput

--let f x = x + 1 in
--  f 5

--{-# SPECIALIZE sumSquares :: Int -> Int#-}
sumSquares :: Int -> Int
sumSquares n = go 0 1
  where
    go !acc !i
      | i > n = acc
      | otherwise = go (acc + i * i) (i + 1)



--data Tree = Leaf | Branch Int Tree Tree deriving (Show, Eq, Ord, Typeable, Data)
--
--inc :: Tree -> Tree
--inc = sybInc
--
--{-# SPECIALIZE sybInc :: Tree -> Tree#-}
--sybInc :: (Data a) => a -> a
--sybInc = everywhere (mkT f) where f (x :: Int) = x + 1
--
--exampleInput = Branch 1 (Branch 2 Leaf Leaf) (Branch 3 Leaf Leaf)
--exampleOutput = inc exampleInput
  
  

--data Salary = S Float
--data Company = C [Dept]
--
--
--incSal :: Float -> Salary -> Salary
--incSal inc (S f) = S (inc * f)
--
--incSalSyb :: Company -> Company
--incSalSyb = everywhere (mkt incSal) x
foo2 = let f x = if x == 0 then 1 else x * g (x-1)
           g x = if x == 0 then 1 else x * f (x-1) in 
            f 5

  
main = putStrLn ("Test suite not yet implemented haha" ++ show (factorial (foldIntsTest :: Int)) ++ sampleMaybeBool (Just True) ++ foo ++ foo ++ show baz ++ show (sumSquares 4) ++ show foo2)--show exampleOutput ++ )
