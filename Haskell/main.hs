import qualified CsvParser as CsvParser
import qualified Data.Map as Map
import MissionConstants
import StringCase
import System.IO
import System.Directory
import Data.List




data User = User { userId :: Int   
                  ,userName :: String  
                  ,password :: String  
                  ,emailAddress:: String  
                  ,dateCreated :: String  
                  ,dateModified :: String  
    } deriving (Show)


    
{-
    Each mission has 
-}




{-
    For each mission step, check to see if it has a missions controller
    
-}

{-
    For each mission step, check to see if it has a missions controller
    
-}

printFileContents inputFile = do  
    handle <- openFile inputFile ReadMode  
    contents <- hGetContents handle
    putStr contents
    hClose handle
