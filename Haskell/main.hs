import qualified Data.Set as Set
import qualified Data.Map as Map
import System.IO

-- Definition of fizzbuzz
fizzbuzz1' :: Int -> String  
fizzbuzz1' x
    | fizz && buzz =  "FizzBuzz"  
    | fizz =  "Fizz"  
    | buzz =  "Buzz"  
    | otherwise   = show x
 where fizz = x `mod` 3 == 0
       buzz = x `mod` 5 == 0

-- using list comprehensions
fizzbuzz1 :: [String]
fizzbuzz1  = [fizzbuzz1' x  | x <- [1..100] ]

-- Using a map
fizzbuzz2 :: [String]
fizzbuzz2 = map fizzbuzz1' [1..100]

-- Given an array with rows and columns, split the array into an array of arrays
splitArray ::  Int -> Int -> [b] -> [[b]]
splitArray 1 c xs = [take c xs]
splitArray r c xs = take c xs : splitArray (r-1) c (drop c xs)

-- Given an array of equally sized arrays, create an array containing their max
maxArray :: (Ord a, Num a) => [[a]] -> [a]
maxArray (xs:xsa) = foldl (\acc x -> zipWith max acc x) xs (xs:xsa)

-- Sample problem
sampleArray = [1, 6, 3, 4, 10, 13, 12, 15, 6, 32, 5, 9, 9, 1, 17, 2, 1, 3, 4, 5]
rows = 5 :: Int
columns = 4 :: Int
splitSampleArray = splitArray rows columns sampleArray
maxSampleArray = maxArray splitSampleArray

data User = User { userId :: Int   
                  ,userName :: String  
                  ,password :: String  
                  ,emailAddress:: String  
                  ,dateCreated :: String  
                  ,dateModified :: String  
    } deriving (Show)

inputLine = "0,bonuigbo,test,test@test.com,11/28/2015 11:50:29 AM,11/28/2015 11:58:16 AM"
inputLine2 = "2,sample,oba,\"\"\"AS\"\"Wjfs\",11/29/2015 1:03:24 PM,11/29/2015 1:03:24 PM"


-- Parse a line, "\"" == ['"'] is true. Must use backslash to denote " characters in String
parseLine :: String -> [String]
parseLine [] = []
parseLine ('"': xs) 
    | take 2 xs == ['"','"'] = parseLine xs
    | head xs == '"' = "test" : parseLine xs
    | head xs == '"' = "test" : parseLine xs
    | otherwise   = parseLine xs
parseLine (',': xs) = parseLine xs
parseLine a = let (x, xs) = span (/=',') a in x : parseLine xs



main = do  
    handle <- openFile "Users.csv" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle  
