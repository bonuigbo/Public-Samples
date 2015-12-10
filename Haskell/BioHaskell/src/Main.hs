import qualified CsvParser as CsvParser
import qualified Data.Map as Map
import MissionConstants
import StringCase
import System.IO
import System.Directory
import Data.List
import System.Environment

printFileContents inputFile = do  
    handle <- openFile inputFile ReadMode  
    contents <- hGetContents handle
    putStr contents
    --putStr $ show contents
    hClose handle
    
main :: IO ()
main = getArgs >>= print . haqify . head 
helloWorld s = "Hello World"