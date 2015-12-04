import System.IO

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


{-|
    Statuses relating to each character as it is read into the input.
    
-}
data QuoteStatus = InQuote | NoQuote deriving(Eq, Show)
data CsvFieldStatus = Next | End | Quote deriving(Eq, Show)
data ParseStatus = ParseStatus { parsedLine :: [String], parseField :: String, quoteStatus :: QuoteStatus, csvFieldStatus :: CsvFieldStatus }  deriving(Eq, Show)

-- A method for updating the last csv field when we reach the end of the line
endStatusLine :: ParseStatus -> ParseStatus
endStatusLine ParseStatus { parsedLine=a, parseField=b, quoteStatus=c, csvFieldStatus=d} = ParseStatus { parsedLine=a ++ [b], parseField=[], quoteStatus=NoQuote, csvFieldStatus=End }

{-|
    Parser. Goes though each character in a string and updates a ParseStatus for it
-}
parseStatusLine :: Char -> ParseStatus -> ParseStatus
-- Next line char
parseStatusLine '\n' ParseStatus { parsedLine=a, parseField=b, quoteStatus=c, csvFieldStatus=d} = ParseStatus { parsedLine=a ++ [b], parseField=[], quoteStatus=NoQuote, csvFieldStatus=End }
-- The case when we encounter a quoted character
parseStatusLine x ParseStatus { parsedLine=a, parseField=b, quoteStatus=InQuote, csvFieldStatus=d}
    -- The start condition, If the first char after the delimiter is not a quote, treat it as an unquoted field
    | x /= '"' && d == End = ParseStatus { parsedLine=a, parseField=b ++ [x], quoteStatus=NoQuote, csvFieldStatus=Next}
    -- The end condition, the delimiter comes right after a single quote char
    | x == ',' && d == Quote = ParseStatus { parsedLine=a ++ [b], parseField=[], quoteStatus=NoQuote, csvFieldStatus=End}
    -- Otherwise, just add it
    | x == ',' = ParseStatus { parsedLine=a, parseField=b ++ [x], quoteStatus=InQuote, csvFieldStatus=Next}
    -- After a quote character, if we detect another one, we add it
    | x == '"' && d == Quote = ParseStatus { parsedLine=a, parseField=b ++ [x], quoteStatus=InQuote, csvFieldStatus=Next}
    | x == '"' = ParseStatus { parsedLine=a, parseField=b, quoteStatus=InQuote, csvFieldStatus=Quote}
    -- General case, if we just passed a single quote character and hit an unauthorized char, throw an error
    | d == Quote = error ("Char cannot be added while in quote status - Char: " ++ (show $ reverse b) ++ (show a))
    | otherwise = ParseStatus { parsedLine=a, parseField=b ++ [x], quoteStatus=InQuote, csvFieldStatus=Next}    
-- The case when we encounter a non quoted
parseStatusLine x ParseStatus { parsedLine=a, parseField=b, quoteStatus=NoQuote, csvFieldStatus=d}
    -- If the first char after the delimiter is a quote, treat it as an quoted field
    | x == '"' && d == End = ParseStatus { parsedLine=a, parseField=b, quoteStatus=InQuote, csvFieldStatus=Next}
    -- If we are not in a quoted field, detecting this character is an error
    | x == '"' = error ("Cannot have quotes inside an unquoted csv string " ++ (show $ reverse b))
    -- The end condition, the delimiter appears
    | x == ',' = ParseStatus { parsedLine=a ++ [b], parseField=[], quoteStatus=NoQuote, csvFieldStatus=End} 
    | otherwise = ParseStatus { parsedLine=a, parseField=b ++ [x], quoteStatus=NoQuote, csvFieldStatus=Next} 

parseCsvLine :: String -> [String]
parseCsvLine [] = []
parseCsvLine (x:xs) = parsedLine $ endStatusLine $ foldl (\acc x -> parseStatusLine x acc) ParseStatus { parsedLine=[], parseField=[], quoteStatus=NoQuote, csvFieldStatus=End} (x:xs)
