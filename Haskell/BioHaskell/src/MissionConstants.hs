module MissionConstants where

import StringCase
import System.IO
import System.Directory
import Data.List
import Control.Monad

data Mission = Mission { missionId :: Int   
                  ,missionName :: String  
    } deriving (Show, Eq)

data MissionStep = MissionStep { missionStepId :: Int   
                  ,missionStepName :: String  
    } deriving (Show, Eq)

missions = [
    Mission 100 "Get noticed on the web"
    ,Mission 200 "Enhance customer service"
    ,Mission 300 "Drive traffic to your website"
    ,Mission 400 "Get more leads"
    ,Mission 500 "Grow your brand image"
    ,Mission 600 "Getting to know you"
    ,Mission 700 "Logo your documents"
    ,Mission 800 "Apps for your smartphone"
    ,Mission 900 "Update Plus web estimate"
    ,Mission 1000 "Get active with Facebook"
    ,Mission 1200 "Configure a rental partner"
    ,Mission 1300 "Facebook review publishing"
    ,Mission 1400 "Touch screen signature"
    ,Mission 1500 "Tablet estimating"
    ,Mission 1600 "Supercharge your website"
    ,Mission 1700 "Enhance your website"]
  
missionSteps = [
    MissionStep 301 "Your website"
    ,MissionStep 401 "Share customer reviews"
    ,MissionStep 501 "Company logo"
    ,MissionStep 601 "Languages spoken"
    ,MissionStep 602 "Company description"
    ,MissionStep 701 "Logo print options"
    ,MissionStep 801 "Introduction"
    ,MissionStep 802 "Download app"
    ,MissionStep 901 "Web estimate"
    ,MissionStep 1001 "Your Facebook page"
    ,MissionStep 1201 "Rental partner"
    ,MissionStep 1301 "Share reviews on Facebook"
    ,MissionStep 1401 "Introduction"
    ,MissionStep 1402 "Configure sign and share"
    ,MissionStep 1501 "Introduction"
    ,MissionStep 1502 "Download app"
    ,MissionStep 1601 "Display customer reviews"
    ,MissionStep 1701 "Display repair status"]

missionNames = fmap missionName missions
missionStepNames = fmap missionStepName missionSteps

projectRootDir = "C:\\CCCDesktop\\Platform\\Projects\\cccone.com\\Dev"
missionStepApiControllersDir = projectRootDir ++ "\\CCC.One.Platform.Web\\Controllers\\Api\\Mission\\Missions"
missionsLessDir = projectRootDir ++ "\\CCC.One.Platform.Web\\Content\\MissionDashboard\\Less\\Layouts\\Missions"
routesDir = projectRootDir ++ "\\CCC.One.Platform.Web.Client.MissionDashboard\\Routes\\Missions"
templatesDir = projectRootDir ++ "\\CCC.One.Platform.Web.Client.MissionDashboard\\UI\\Templates\\Missions"
typescriptControllersDir = projectRootDir ++ "\\CCC.One.Platform.Web.Client.MissionDashboard\\Controllers\\Missions"

gameMissionsFile = projectRootDir ++ "\\CCC.One.Platform.Web.BusinessLogic\\Constants\\GameMissions.cs"
repairServiceFile = projectRootDir ++ "\\CCC.One.Platform.Web.Client.MissionDashboard\\Services\\RepairFacilityService.ts"
missionDashboardFile = projectRootDir ++ "\\CCC.One.Platform.Web.Client.MissionDashboard\\Constants\\MissionDashboard.ts"
apiEndpointsFile = projectRootDir ++ "\\CCC.One.Platform.Web.Client.MissionDashboard\\Constants\\ApiEndpoints.ts"
referencesFile = projectRootDir ++ "\\CCC.One.Platform.Web.Client.MissionDashboard\\_references.ts"
missionsLessFile = projectRootDir ++ "\\CCC.One.Platform.Web\\Content\\MissionDashboard\\Less\\Layouts\\Missions\\Missions.less"

contents = getDirectoryContents typescriptControllersDir



-- Takes two strings and determines if the second string contains the first
containsString :: String -> String -> Bool
containsString x y = if x `isInfixOf` y then True else False


-- Takes two lists and outputs the values that are not in the second list from the first
getMissingStringsFromList :: [String] -> [String] -> [String]
getMissingStringsFromList x [] = x
getMissingStringsFromList [] _ = []
getMissingStringsFromList (x:xs) y = if any (containsString x) y 
                                     then getMissingStringsFromList xs y 
                                     else x : getMissingStringsFromList xs y

-- Takes a list of strings and determines which are not in the given string             
getMissingStringsFromString :: [String] -> String ->  [String]
getMissingStringsFromString x [] = []
getMissingStringsFromString [] _ = []
getMissingStringsFromString (x:xs) y = if any (containsString x) (words y) 
                                       then getMissingStringsFromString xs y 
                                       else x : getMissingStringsFromString xs y
{-|
    Unpure code
-}

-- Takes a list of missions or steps and determines which of these words are not in the input file
getMissingInFile :: [String] -> FilePath ->  IO [String]
getMissingInFile x fileName = do
    contents <- readFile fileName
    --return $ [normalize contents]
    return $ getMissingStringsFromString (fmap normalize x) (normalize contents)
    where normalize = (downcase . removeSpace . removeChar ',' . removeChar '.')

-- Takes a list of mission names, a file directory, and returns the missing ones    
getMissingMissionsFromFileDir :: [String] -> FilePath -> IO [String]
getMissingMissionsFromFileDir x y = do
    z <- getDirectoryContents y
    return $ getMissingStringsFromList (normalize x) (normalize z)
    where normalize = fmap (downcase . removeSpace . removeChar ',' . removeChar '.')
    
-- Takes a 
getAllFilesInPaths :: [IO [String]] -> IO [String]
getAllFilesInPaths (x:xs) = do -- x is a list of files, IO [String]
    y <- x
    z <- getAllFilesInPaths xs
    return $ z ++ (init $ init y)
getAllFilesInPaths x = return []

-- Takes a directory, goes through all the sub directories, and puts all the files from each sub directory
-- into a single list
getSubfilesOfDirectory :: FilePath -> IO [String]
getSubfilesOfDirectory x = do
                                foldersAndFiles <- getDirectoryContents x
                                folders <- let mapRootPathToFile = fmap (\x' -> x ++ "\\" ++ x')
                                               removeNonFolders = filter (not . containsString "." )
                                           in return $ mapRootPathToFile $ removeNonFolders foldersAndFiles -- [String] -> IO [String]
                                subPathFoldersAndFiles <- return $ fmap getDirectoryContents folders -- [IO [String]] -> IO [IO [String]]
                                combinedSubPathFoldersAndFiles <- getAllFilesInPaths subPathFoldersAndFiles
                                return combinedSubPathFoldersAndFiles
                                
-- Takes a list of mission names, a list of files, and returns the missing ones
getMissingMissionsFromFiles :: [String] -> IO [String] -> IO [String]
getMissingMissionsFromFiles x y = do
    z <- y
    return (getMissingStringsFromList (normalize x) (normalize z))
    where normalize = fmap (downcase . removeSpace . removeChar ',' . removeChar '.')
    
-- Grab the list of missing stuff
missingStepControllers = getMissingMissionsFromFileDir missionStepNames missionStepApiControllersDir    
missingRoutes = getMissingMissionsFromFileDir missionNames routesDir
missingTemplateFolders = getMissingMissionsFromFileDir missionNames templatesDir
missingMissionTypeControllers = getMissingMissionsFromFiles missionNames $ getSubfilesOfDirectory typescriptControllersDir
missingStepTypeControllers = getMissingMissionsFromFiles missionStepNames $ getSubfilesOfDirectory typescriptControllersDir

missingGameMissions = getMissingInFile missionNames gameMissionsFile
missingGameSteps = getMissingInFile missionStepNames gameMissionsFile
missingDashboardMissions = getMissingInFile missionNames missionDashboardFile
missingDashboardSteps = getMissingInFile missionStepNames missionDashboardFile
missingApiMissions = getMissingInFile missionNames apiEndpointsFile
missingApiSteps = getMissingInFile missionStepNames apiEndpointsFile
missingRepairSteps = getMissingInFile missionStepNames repairServiceFile
    
printFile inputFile = do  
    handle <- openFile inputFile ReadMode  
    contents <- hGetContents handle
    putStr $ show contents
    putStr contents
    hClose handle
