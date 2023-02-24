module Test where
import Data.Char (isSpace, toUpper, toLower)
import Data.Type.Coercion (Coercion)
import Data.Kind (Type)
import Data.Typeable

sayHello :: String -> IO()
sayHello x =
  putStrLn ("HELLO, " ++ x ++ "!")

triple :: Int -> Int
triple x = x * 3

--areaOfCircle :: Int -> Either Float Int
areaOfCircle x = pi * (x * x)

printInc :: Int -> IO()
printInc n =
  let plusTwo = n + 3
  in print plusTwo

printAny :: IO()
printAny = do
  putStrLn "Hello"
  putStrLn "World!"
  putStrLn "Areeb"
  putStrLn "Here"

-- declared a new data type with type constructors Blah and Woot

data Mood = Blah | Woot deriving Show

greetIfCool :: String -> IO()
greetIfCool coolness =
  if cool
    then putStrLn "OOOOO cool"
   else
     putStrLn "OOOO not coool"
   where cool =
           coolness == "this person is cool!"


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a =
  a == reverse a


identity :: a -> a
identity x = x


myAbs :: Integer -> Integer 
myAbs int = 
  if int > 0 
    then int
   else
     (-1) * int 
     
funcFour ::  Integer -> Integer -> Integer -> Integer -> Integer
funcFour a b c d = 
   a + b + c + d
   
isLeapYear :: Integer -> Bool
isLeapYear year 
  | year `rem` 400 == 0 = True
  | year `rem` 100 == 0 = False 
  | year `rem` 4 == 0 = True 
  | otherwise = False
  
  
isQuestion :: String -> Bool
isQuestion sentence 
  | drop ((length sentence) - 1) sentence == "?" = True 
  | otherwise = False
  
trim :: String -> String
trim argument = 
  let newString = dropWhile isSpace argument
    in reverse (dropWhile isSpace (reverse newString))
    
isSilence :: String -> Bool
isSilence sentence 
  | trim sentence == "" = True 
  | otherwise = False
  
isCapital :: String -> Bool
isCapital sentence
  | map toUpper sentence == sentence = True 
  | otherwise = False
  
isCapitalQuestion :: String -> Bool
isCapitalQuestion sentence =
  isCapital sentence && isQuestion sentence
  
  
isLowerQuestion :: String -> Bool 
isLowerQuestion sentence = 
  not (isCapital sentence) && isQuestion sentence

doStuff1 :: [Int] -> Int
doStuff1 [] = 0
doStuff1 [x] = x
doStuff1 xs = head xs + head (tail xs)

