import qualified CsvParser as CsvParser
import qualified Data.Map as Map
import MissionConstants
import StringCase
import System.IO
import System.Directory
import Data.List
import System.Environment
import Control.Applicative
import Control.Monad (liftM, ap)

{-|
     
-}


printFileContents inputFile = do  
    handle <- openFile inputFile ReadMode  
    contents <- hGetContents handle
    putStr contents
    --putStr $ show contents
    hClose handle


newtype Counter a = Counter { value :: (a, Int) } deriving (Show)

instance Functor Counter where
    fmap f (Counter (a, b)) = Counter (f a, b)
    
instance Applicative Counter where
    pure a = Counter (a, 0)
    (Counter (f, b)) <*> (Counter (c, d)) = (Counter (f c, d + b))
    
instance Monad Counter where
    return x = Counter (x, 0)
    Counter(x, y) >>= f = let Counter (x', y') = f x 
                          in Counter (x', y+y')
    
    
increment5 :: Int -> Counter Int
increment5 x = Counter(x+5, 1)

sample1 = return 5 >>= increment5 >>= increment5

newtype Logger a = Logger { execLogger :: (a, [String]) }

instance Functor Logger where
    fmap = liftM
    
instance Applicative Logger where
    pure = return
    (<*>) = ap
    
instance Monad Logger where
    return a = Logger (a, [])
    m >>= k = let (a, w) = execLogger m
                  n = k a
                  (b, x) = execLogger n
              in Logger (b, w ++ x)

{-|
     File management. Need to generate a list of files, zip them
     encrypt them, move them, etc.
     First, need a list of all important files and directories,
     Then need to 

-}

homeGitDir = "C:\\Users\\Bonu\\Documents"
homeEmacsFile = "c:\\users\\bonu\\appdata\\roaming\\.emacs"
gitDir = homeGitDir ++ "\\GitHub\\Public-Samples\\Haskell\\BioHaskell\\Env"
gitEmacsFile = gitDir ++ "\\emacs.txt"


-- Sync data from my local to the github

main :: IO ()
main = do
  print "Running setup"
  copyFile homeEmacsFile gitEmacsFile


