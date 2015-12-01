module CsvParser 
( parseCsvLine 
) where 

-- =======================
-- CSV Parser
-- =======================

{-|
    Cases to handle
    1. string doesn't start with quotes char "
        a. If a quote is detected within, error
        b. If a comma is detected, that is the end of the field
        c. Else, add char, continue
    2. string starts with quote char
        a. Hit a quote char
            i. if in quoteStatus, add char and end quoteStatus, else set quoteStatus
        b. Hit a comma
            i. if in quoteStatus, end line, else add char
        c. Hit a char
            i. if in quoteStatus, throw error, else add char
-}

input1 = "0,bonuigbo,test,test@test.com,11/28/2015 11:50:29 AM,11/28/2015 11:58:16 AM"
input2 = "2,sample,oba,\",\"\",pl\"\"as,,\"\",\"\"\"\"\",11/29/2015 1:03:24 PM,11/29/2015 1:03:24 PM"
input3 = "\"test\",sample,oba,\",\"\",pl\"\"as,,\"\",\"\"\"\"\",\"11/29/2015 1:03:24 PM\",11/29/2015 1:03:24 PM"

{-|
    Statuses relating to each character as it is read into the input.
    
-}
data CsvFieldStatus = Add | Skip | End | Quote | StartQuote deriving(Eq, Show)

parseCsvChar :: Char -> CsvFieldStatus
parseCsvChar x
    | x == '"' = error "Cannot have quotes inside an unquoted csv string"
    | x == ',' = End
    | otherwise = Add
    
parseCsvField :: String -> String
parseCsvField [] = []
parseCsvField (x:xs)
    | parseCsvChar x == End = []
    | parseCsvChar x == Skip = parseCsvField xs
    | parseCsvChar x == Add = x : parseCsvField xs
    
skipCsvField :: String -> Int
skipCsvField [] = 0
skipCsvField (x:xs)
    | parseCsvChar x == End = 1
    | parseCsvChar x == Skip = 1 + skipCsvField xs
    | parseCsvChar x == Add = 1 + skipCsvField xs
    
{-|
    Takes in the next character, and the status of the read
    from the previous character to get the new status    
-}
parseCsvCharQuotes :: (Char, CsvFieldStatus) -> CsvFieldStatus
parseCsvCharQuotes (x, y)
    | y == StartQuote = Skip
    | x == ',' && y == Quote = End
    | x == ',' = Add
    | x == '"' && y == Quote = Add
    | x == '"' = Quote
    | y == Quote = error "Char cannot be added while in quote status"
    | otherwise = Add


parseCsvFieldQuotes :: (String, CsvFieldStatus) -> String
parseCsvFieldQuotes ([], _) = []
parseCsvFieldQuotes ((x:xs), y)
    | status == End = []
    | status == Skip || status == Quote = nextCsvField
    | status == Add = x : nextCsvField
    where status = parseCsvCharQuotes (x, y)
          nextCsvField = parseCsvFieldQuotes (xs, status)
    
skipCsvFieldQuotes :: (String, CsvFieldStatus) -> Int
skipCsvFieldQuotes ([], _) = 0
skipCsvFieldQuotes ((x:xs), y)
    | status == End = 1
    | otherwise = 1 + skipCsvFieldQuotes (xs, status)
    where status = parseCsvCharQuotes (x, y)

parseCsvData :: String -> (Int, String)
parseCsvData [] = (0, [])
parseCsvData x 
    | head x /= '"' = (skipCsvField x, parseCsvField x)  
    | otherwise = (skipCsvFieldQuotes (x, StartQuote), parseCsvFieldQuotes (x, StartQuote))
    
parseCsvLine :: String -> [String]
parseCsvLine [] = []
parseCsvLine a = let (y, x) = parseCsvData a in x : parseCsvLine (drop y a)