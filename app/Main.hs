module Main (main) where

import Lib()
import Test()
main :: IO ()
main = do
  putStrLn "We are starting off this project"
  funcFour 12 + 13


--sayHello :: String -> IO()
--sayHello x = 
  --putStrLn ("HELLO " ++ x ++ "!")
