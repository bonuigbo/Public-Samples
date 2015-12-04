import qualified CsvParser as CParser
import qualified Data.Map as Map
import System.IO

{-|
    1. For parsing errors, look for the line/char before the error char
-}
-- =======================
-- Definition of fizzbuzz
-- =======================
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

-- =======================
-- A method of taking a line of data, splitting it, getting a max array
-- =======================

-- Given an array with rows and columns, split the array into an array of arrays
splitArray ::  Int -> Int -> [b] -> [[b]]
splitArray 1 c xs = [take c xs]
splitArray r c xs = take c xs : splitArray (r-1) c (drop c xs)

-- Given an array of equally sized arrays, create an array containing their max
maxArray :: (Ord a, Num a) => [[a]] -> [a]
-- xs is a list, zipWith takes the max of the current acc list and holds the max values
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

inputFile = "C:\\OProject\\BonuApp\\BioServer\\Data\\CsvFiles\\Users.csv"

input1 = "0,bonuigbo,test,test@test.com,11/28/2015 11:50:29 AM,11/28/2015 11:58:16 AM"
input2 = "2,sample,oba,\",\"\",pl\"\"as,,\"\",\"\"\"\"\",11/29/2015 1:03:24 PM,11/29/2015 1:03:24 PM"
input3 = "\"test\",sample,oba,\",\"\",pl\"\"as,,\"\",\"\"\"\"\",\"11/29/2015 1:03:24 PM\",11/29/2015 1:03:24 PM"

test3 = do  
    handle <- openFile inputFile ReadMode  
    contents <- hGetContents handle
    putStr $ show $ CParser.parseCsvLine contents
    hClose handle
    
test1 = CParser.parseCsvLine input1
test2 = CParser.parseCsvLine input2

test4 = "hello\nworld\ntest"
{-|
main = do  
    handle <- openFile inputFile ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle
    
main = do   
    withFile "something.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle  
        putStr contents) 
        
main = do  
    contents <- readFile inputFile
    putStr contents
-}
main = do  
    handle <- openFile inputFile ReadMode  
    contents <- hGetContents handle
    putStr contents

    putStr $ show $ fmap CParser.parseCsvLine $ lines contents
    --putStr contents  
    hClose handle
