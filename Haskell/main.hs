import qualified CsvParser as CsvParser
import qualified Data.Map as Map
import System.IO
import System.Directory

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
input2 = "2,sample,oba,\",\"\",pl\"\"as,,\"\",\"\"\"\"\",\"11/29/2015 1:03:24 PM\",11/29/2015 1:03:24 PM"
input3 = "1,test1,test@test.com\n\ 
         \2,test2,test1,\"testquote\"\"\"\n\
         \3,test3,test2,\"testcomma,,\"\n\
         \\"4\",\"test4\",\"comma,quote\"\"end\"\n"
    
test1 = CsvParser.parseCsvLine input1
test2 = CsvParser.parseCsvLine input2
test3 = CsvParser.printReadCsvFile inputFile
test4 = CsvParser.parseCsvFile input3
    
projectRootDirectory = "C:\\CCCDesktop\\Platform\\Projects\\cccone.com\\Dev\\CCC.One.Platform.Web"
missionStepApiControllersDirectory = "C:\\CCCDesktop\\Platform\\Projects\\cccone.com\\Dev\\CCC.One.Platform.Web\\Controllers\\Api\\Mission\\Missions"
missionsLessDirectory = "C:\\CCCDesktop\\Platform\\Projects\\cccone.com\\Dev\\CCC.One.Platform.Web\\Content\\MissionDashboard\\Less\\Layouts\\Missions"

inputDirectory = "C:\\OProject\\BonuApp\\BioServer\\Data\\CsvFiles\\"
contents = getDirectoryContents inputDirectory

