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

--let f x = x + 1 in
--  f 5
  
  
--data Salary = S Float
--data Company = C [Dept]


--incSal :: Float -> Salary -> Salary
--incSal inc (S f) = S (inc * f)
--
--incSalSyb :: Company -> Company
--incSalSyb = everywhere (mkt incSal) x


  
main = putStrLn ("Test suite not yet implemented haha" ++ show (factorial (foldIntsTest :: Int)) ++ sampleMaybeBool (Just True))
