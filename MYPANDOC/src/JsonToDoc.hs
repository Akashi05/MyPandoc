{-
-- EPITECH PROJECT, 2024
-- makefile
-- File description:
-- Haskell Learning
-}

module JsonToDoc where
import Data.Maybe (fromMaybe, fromJust)
import System.Exit (exitWith, ExitCode(..))
import Data.List (intercalate)
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import Document

-- This is a simple JSON parser and document structure
type Parser a = String -> Maybe (a , String)

parseChar :: Char ->  Parser Char
parseChar a str = case str of
    (x:xs) | a == x -> Just (a, xs)
        | otherwise -> Nothing
    [] -> Nothing

parseCharSpace :: Char -> Parser Char
parseCharSpace c str =
    let trimmed = skipWhitespace str
    in case parseChar c trimmed of
        Just (_, rest) -> Just (c, skipWhitespace rest)
        Nothing -> Nothing


parseAnyChar :: String -> Parser Char
parseAnyChar (x:xs) str | parseChar x str /= Nothing = parseChar x str
                        | otherwise = parseAnyChar xs str
parseAnyChar [] _ = Nothing

parseSome :: Parser a -> Parser [a]
parseSome p str = case p str of
    Just (result, rest) -> case parseMany p rest of
        Just (results, rest2) -> Just (result : results, rest2)
        Nothing -> Just ([result], rest)
    Nothing -> Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 str = case p1 str of
    Just (result, rest) -> Just (result, rest)
    Nothing -> p2 str

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 str = case p1 str of 
    Just (result1, rest1) -> case p2 rest1 of
        Just (result2, rest2) -> Just ((result1, result2), rest2)
        Nothing -> Nothing
    Nothing -> Nothing


parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 str = case p1 str of
    Just (result1, rest1) -> case p2 rest1 of
        Just (result2, rest2) -> Just (f result1 result2, rest2)
        Nothing -> Nothing
    Nothing -> Nothing

parseMany :: Parser a -> Parser [ a ]
parseMany p str = case p str of
    Just (result, rest) -> case parseMany p rest of
        Just (results, rest2) -> Just (result:results, rest2)
        Nothing -> Just ([result], rest)
    Nothing -> Just ([], str)

parseUInt :: Parser Int
parseUInt str = case parseAnyChar ['0'..'9'] str of
    Just(result, rest) -> case parseUInt rest of
        Just (results, rest2) -> Just (read (result: show results), rest2)
        Nothing -> Just (read [result], rest)
    Nothing -> Nothing

parseInt :: Parser Int
parseInt str = case parseChar '-' str of
    Just (_, rest) -> case parseUInt rest of
        Just (results, rest2) -> Just (-results, rest2)
        Nothing -> Nothing
    Nothing -> case parseUInt str of
        Just (results, rest) -> Just (results, rest)
        Nothing -> Nothing

parseTuple :: Parser a -> Parser(a, a)
parseTuple p str = case p str of
    Just (result1, rest1) -> case p rest1 of
        Just (result2, rest2) -> Just ((result1, result2), rest2)
        Nothing -> Nothing
    Nothing -> Nothing

parseCharWithWhitespace :: Char -> Parser Char
parseCharWithWhitespace c str =
    let trimmed = skipWhitespace str
    in case parseChar c trimmed of
        Just (_, rest) -> Just (c, skipWhitespace rest)
        Nothing -> Nothing

-- Second degree of parsers definition with convert to JsonValue

data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonNumber Double
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show, Eq)

parseJsonNull :: Parser JsonValue
parseJsonNull str = case take 4 str of
    "null" -> Just (JsonNull, drop 4 str)
    _ -> Nothing

parseJsonBool :: Parser JsonValue
parseJsonBool str = case take 4 str of
    "true" -> Just (JsonBool True, drop 4 str)
    _ -> case take 5 str of
        "false" -> Just (JsonBool False, drop 5 str)
        _ -> Nothing

getNumber :: Int -> Int -> String -> String -> (JsonValue, String)
getNumber result result3 rest rest3 =
    let number = JsonNumber (
            fromIntegral result + 1.0 / (10^(length rest - length rest3 - 1)) *
                fromIntegral result3)
    in (number, rest3)

parseJsonNumber :: Parser JsonValue
parseJsonNumber str = case parseInt str of
    Just (result, rest) -> case parseChar '.' rest of
        Just (_, rest2) -> case parseUInt rest2 of
            Just (result3, rest3) ->
                Just (getNumber result result3 rest rest3)
            Nothing -> Just (JsonNumber (fromIntegral result), rest)
        Nothing -> Just (JsonNumber (fromIntegral result), rest)
    Nothing -> Nothing


parseJsonString :: Parser JsonValue
parseJsonString str = case parseChar '\"' str of
    Just (_, rest) -> Just (JsonString (takeWhile (/= '\"') rest),
        drop 1 (dropWhile (/= '\"') rest))
    Nothing -> Nothing

parseOptionalComma :: String -> (Bool, String)
parseOptionalComma str =
    case parseCharSpace ',' str of
        Just (_, rest) -> (True, rest)  -- Comma found
        Nothing        -> (False, str)  -- No comma

skipWhitespace :: String -> String
skipWhitespace = dropWhile (\c -> c == ' ' || c == '\t' || c == '\n')

parseElements :: String -> ([JsonValue], String)
parseElements str = case parseJsonValue str of
    Just (value, rest1) ->  -- Successfully parsed a value
        case parseOptionalComma rest1 of
            (True, rest2) ->  -- Comma found, parse more elements
                let (values, rest3) = parseElements rest2
                in (value : values, rest3)
            (False, rest2) -> -- No comma, stop parsing elements
                ([value], rest2)
    Nothing -> ([], str)

parseJsonArray :: Parser JsonValue
parseJsonArray input =
    case parseCharSpace '[' input of
        Just (_, rest1) ->  -- Successfully parsed '['
            let (elements, rest2) = parseElements rest1
            in case parseCharSpace ']' rest2 of
                -- Successfully parsed ']'
                Just (_, rest3) -> Just (JsonArray elements, rest3)
                Nothing         -> Nothing  -- Failed to parse ']'
        Nothing -> Nothing  -- Failed to parse '[' 

parsePair :: String -> (Maybe (String, JsonValue), String)
parsePair str = case parseJsonString str of
    Just (JsonString key, rest1) -> case parseCharSpace ':' rest1 of
        Just (_, rest2) -> case parseJsonValue rest2 of
            Just (value, rest3) -> (Just (key, value), rest3)
            Nothing -> (Nothing, str)  -- Failed to parse value
        Nothing -> (Nothing, str)  -- Failed to parse ':'
    Nothing -> (Nothing, str)  -- Failed to parse key

-- Main function to parse multiple key-value pairs
parsePairs :: String -> ([(String, JsonValue)], String)
parsePairs str = case parsePair str of
    (Just (key, value), rest1) -> case parseOptionalComma rest1 of
        (True, rest2) -> let (morePairs, rest3) = parsePairs rest2
                        in ((key, value) : morePairs, rest3)
        (False, rest2) -> ([(key, value)], rest2)
    (Nothing, rest) -> ([], rest)  -- No more pairs to parse

parseJsonObject :: Parser JsonValue
parseJsonObject input =
    case parseCharSpace '{' input of
        Just (_, rest1) ->  -- Successfully parsed '{'
            let (pairs, rest2) = parsePairs rest1
            in case parseCharSpace '}' rest2 of
                -- Successfully parsed '}'
                Just (_, rest3) -> Just (JsonObject pairs, rest3)
                Nothing         -> Nothing  -- Failed to parse '}'
        Nothing -> Nothing  -- Failed to parse '{'
    --Function to parse zero or more key-value pairs

tryParsers :: [Parser JsonValue] -> Parser JsonValue
tryParsers [] _ = Nothing  -- No parsers left to try
tryParsers (p:ps) str = case p str of
    Just res -> Just res  -- Return result if parser succeeds
    Nothing -> tryParsers ps str  -- Try the next parser

-- Main function to parse a JSON value
parseJsonValue :: Parser JsonValue
parseJsonValue str =
    let trimmed = skipWhitespace str
    in tryParsers
        [ parseJsonNull, parseJsonBool, parseJsonNumber
        , parseJsonString, parseJsonArray, parseJsonObject
        ] trimmed

parseJsonfileContent :: String -> Either String JsonValue
parseJsonfileContent str = let trimmed = skipWhitespace str
    in case parseJsonValue trimmed of
        Just (result, rest) ->
            let remaining = skipWhitespace rest
            in if null remaining
                then Right result
                else Left "Unexpected trailing characters after JSON value"
        Nothing -> Left "Failed to parse JSON value"

parseJsonfile :: String -> IO (Either String JsonValue)
parseJsonfile filename = do
    content <- readFile filename
    return (parseJsonfileContent content)
-- Now I convert Json values to a Document structure
-- Document structure
-- parse

isValidKey :: JsonValue -> [String] -> Bool
isValidKey (JsonObject pairs) allowedKeys =
    let keys = map fst pairs
    in isRedundant (JsonObject pairs) && all (`elem` allowedKeys) keys
isValidKey _ _ = False

isRedundant :: JsonValue -> Bool
isRedundant (JsonObject pairs) =
    let keys = map fst pairs
    in not $ hasDuplicates keys
isRedundant _ = True 

hasDuplicates :: [String] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs)
    | x `elem` xs = True
    | otherwise   = hasDuplicates xs

parseHeader :: JsonValue -> Maybe Header
parseHeader (JsonObject headerObj) =
    if isValidKey (JsonObject headerObj) ["title", "author", "date"] then
        (case lookup "title" headerObj of
           Just (JsonString title) -> let
                  author = lookupField "author" headerObj >>= parseString
                  date = lookupField "date" headerObj >>= parseString
                in Just (Header (Just title) author date)
           _ -> Nothing)
    else Nothing
parseHeader _ = Nothing  -- Fail if input is not a JsonObject

parseString :: JsonValue -> Maybe String
parseString (JsonString str) = Just str
parseString _ = Nothing

lookupField :: String -> [(String, JsonValue)] -> Maybe JsonValue
lookupField = lookup


parserText :: JsonValue -> Maybe BodyPart
parserText (JsonString str) = Just (Text str)
parserText _ = Nothing

parseBold :: [(String, JsonValue)] -> Maybe BodyPart
parseBold obj = case lookupField "bold" obj of
    Just value ->
        case parseBody value of
            Just body -> Just (Bold body)
            Nothing   -> Nothing
    Nothing -> Nothing

parseItalic :: [(String, JsonValue)] -> Maybe BodyPart
parseItalic obj = case lookupField "italic" obj of
    Just value ->
        case parseBody value of
            Just body -> Just (Italic body)
            Nothing   -> Nothing
    Nothing -> Nothing

parseCode :: [(String, JsonValue)] -> Maybe BodyPart
parseCode obj = case lookupField "code" obj of
    Just value ->
        case parseBody value of
            Just body -> Just (Code body)
            Nothing   -> Nothing
    Nothing -> Nothing

parserformat :: JsonValue -> Maybe BodyPart
parserformat (JsonObject obj) =
    case parseBold obj of
        Just res -> Just res
        Nothing  -> case parseItalic obj of
            Just res -> Just res
            Nothing  -> parseCode obj
parserformat _ = Nothing

--Function to parse a link
parseLink :: [(String, JsonValue)] -> Maybe BodyPart
parseLink linkObj =
    case (lookupField "url" linkObj, lookupField "content" linkObj) of
        (Just (JsonString url), Just (JsonArray contents)) ->
            case mapM parseBody contents of
                Just parsedContents -> Just (Link url parsedContents)
                Nothing            -> Nothing
        _ -> Nothing

-- Main parserImageLinks function
parserImageLinks :: JsonValue -> Maybe BodyPart
parserImageLinks (JsonObject jsonValue) =
    case lookupField "image" jsonValue of
        Just (JsonObject imgObj) -> parseImage imgObj
        Nothing ->
            case lookupField "link" jsonValue of
                Just (JsonObject linkObj) -> parseLink linkObj
                Nothing                   -> Nothing
        _ -> Nothing
parserImageLinks _ = Nothing

parseImage :: [(String, JsonValue)] -> Maybe BodyPart
parseImage imgObj =
    case (lookupField "url" imgObj, lookupField "alt" imgObj) of
        (Just (JsonString url), Just (JsonArray altItems)) ->
            case mapM parseBody altItems of
                Just parsedAlts -> Just (Image url parsedAlts)
                Nothing -> Nothing
        _ -> Nothing

--Function to parse an image
parserImage :: JsonValue -> Maybe BodyPart
parserImage (JsonObject jsonValue) = 
    case lookupField "image" jsonValue of
        Just (JsonObject imgObj) -> 
            case (lookupField "url" imgObj, lookupField "alt" imgObj) of
                (Just (JsonString url), Just (JsonArray altItems)) ->
                    case mapM parseBody altItems of
                        Just parsedAlts -> Just (Image url parsedAlts)
                        Nothing -> Nothing
                _ -> Nothing
        _ -> Nothing
parserImage _ = Nothing

parserParagraph :: JsonValue -> Maybe BodyPart
parserParagraph jsonValue =
    case parseArray jsonValue of
        Just bodyParts -> Just (Paragraph bodyParts)
        Nothing -> Nothing


parseArray :: JsonValue -> Maybe [BodyPart]
parseArray (JsonArray []) = Just []
parseArray (JsonArray (x:xs)) =
    case parseBody x of
        Just body -> case parseArray (JsonArray xs) of
            Just bodies -> Just (body : bodies)
            Nothing -> Nothing
        Nothing -> Nothing
parseArray _ = Nothing


extractTitle :: [(String, JsonValue)] -> Maybe String
extractTitle secObj =
    case lookupField "title" secObj of
        Just (JsonString t) -> Just t
        _                   -> Nothing  -- Treat non-string titles as Nothing

extractContent :: [(String, JsonValue)] -> Maybe BodyPart
extractContent secObj =
    case lookupField "content" secObj of
        Just c -> parseBody c
        _      -> Nothing  -- Missing or invalid content

-- Main parseSection function
parseSection :: JsonValue -> Maybe BodyPart
parseSection (JsonObject jsonValue) = case lookupField "section" jsonValue of
    Just (JsonObject secObj) ->
        let title   = extractTitle secObj
            content = case lookupField "content" secObj of
                        Just (JsonArray items) -> mapM parseBody items
                        _ -> Nothing
        in case content of
            Just bodies -> Just (Section title bodies)
            Nothing     -> Nothing
    _ -> Nothing
parseSection _ = Nothing

parseList :: JsonValue -> Maybe BodyPart
parseList (JsonObject obj)=
    case lookupField "list" obj of
        Just (JsonArray lst) ->
            let items = map (\item -> case parseBody item of
                                Just body -> Just (Item body)
                                Nothing -> Nothing) lst
            in if Nothing `notElem` items
            then Just (List (map fromJust items))
            else Nothing
        _ -> Nothing
parseList _ = Nothing

parseCodeBlock :: JsonValue -> Maybe BodyPart
parseCodeBlock (JsonObject jsonValue) =
    case lookupField "codeblock" jsonValue of
        Just (JsonArray items) -> 
            case mapM parseBody items of
                Just bodies -> Just (CodeBlock bodies)
                Nothing -> Nothing
        _ -> Nothing
parseCodeBlock _ = Nothing

parseParagraph :: JsonValue -> Maybe BodyPart
parseParagraph jsonValue =
    case parseArray jsonValue of
        Just bodyParts -> Just (Paragraph bodyParts)
        Nothing ->
            case parseBody jsonValue of
                Just bodyPart -> Just (Paragraph [bodyPart])
                Nothing -> Nothing

tryBodyParsers :: [JsonValue -> Maybe BodyPart] -> JsonValue -> Maybe BodyPart
tryBodyParsers [] _ = Nothing  -- No parsers left to try
tryBodyParsers (p:ps) jsonValue = case p jsonValue of
    Just res -> Just res  -- Return result if parser succeeds
    Nothing  -> tryBodyParsers ps jsonValue  -- Try the next parser

-- Main function to parse a BodyPart
parseBody :: JsonValue -> Maybe BodyPart
parseBody  = tryBodyParsers
        [parserText, parserformat, parserImageLinks
        , parseSection, parseCodeBlock, parseList
        , parseParagraph
        ]


validateJsonObject :: JsonValue -> Either String [(String, JsonValue)]
validateJsonObject jsonValue = case jsonValue of
    JsonObject obj -> Right obj
    _              -> Left "Input JSON must be an object."

--Function to extract and validate the "header" field
extractHeader :: [(String, JsonValue)] -> Either String JsonValue
extractHeader jsonObject = case lookupField "header" jsonObject of
    Just h -> Right h
    Nothing -> Left "Missing 'header' field in the document."

-- function to extract and validate the "body" field
extractBody :: [(String, JsonValue)] -> Either String JsonValue
extractBody jsonObject = case lookupField "body" jsonObject of
    Just b -> Right b
    Nothing -> Left "Missing 'body' field in the document."

--Function to parse the header
parseHeaderField :: JsonValue -> Either String Header
parseHeaderField headerJson = case parseHeader headerJson of
    Just h -> Right h
    Nothing -> Left "Invalid or missing header in the document."

--Function to parse the body
parseBodyField :: JsonValue -> Either String [BodyPart]
parseBodyField bodyJson = case parseBody bodyJson of
    Just (Paragraph body) -> if null body
        then Left "The document body cannot be empty."
        else Right body
    Just _ -> Left "Invalid body format."
    Nothing -> Left "Failed to parse the document body."

--Function to validate the title in the header
validateTitle :: Header -> Either String ()
validateTitle header = case title header of
    Just _ -> Right ()
    Nothing -> Left "The document header must contain a title."

parseBodyArray :: JsonValue -> Maybe [BodyPart]
parseBodyArray (JsonArray items) = mapM parseBody items
parseBodyArray _ = Nothing

-- Main function to parse a Document
parseDocument :: JsonValue -> Either String Document
parseDocument jsonValue = do
    jsonObject <- validateJsonObject jsonValue
    headerJson <- extractHeader jsonObject
    bodyJson <- extractBody jsonObject
    header <- parseHeaderField headerJson
    content <- parseBodyField bodyJson
    _ <- validateTitle header
    Right (Document header content)

-- Main function to parse and validate the entire document
parseAndValidateDocument :: JsonValue -> IO Document
parseAndValidateDocument jsonValue = case parseDocument jsonValue of
    Right doc -> return doc
    -- Write error message to stderr
    Left errMsg -> putStrLn errMsg  >> exitWith (ExitFailure 84)

parseContentToDocument :: String -> IO Document
parseContentToDocument fileContent = case parseJsonfileContent fileContent of
        Left errMsg -> putStrLn ("Error parsing JSON file: " ++ errMsg) >>
            exitWith (ExitFailure 84)
        Right jsonValue -> parseAndValidateDocument jsonValue

parseFileToDocument :: String -> IO Document
parseFileToDocument filePath = do
    jsonResult <- parseJsonfile filePath
    case jsonResult of
        Left errMsg -> putStrLn ("Error parsing JSON file: " ++ errMsg)
            >> exitWith (ExitFailure 84)
        Right jsonValue -> parseAndValidateDocument jsonValue

-- Convert Document to JSON
escapeJsonString :: String -> String
escapeJsonString = concatMap escapeChar
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c    = [c]
-- "{",","

-- Extract the header and convert it to JSON
headerToJson :: Header -> String
headerToJson (Header title author date) =
    "{\"title\": \"" ++ escapeJsonString (fromMaybe "" title) ++ "\""
    ++ formatOptionalField "author" author
    ++ formatOptionalField "date" date
    ++ "}"

formatOptionalField :: String -> Maybe String -> String
formatOptionalField fieldName value =
    case value of
        Just v -> ",\"" ++ fieldName ++ "\": \"" ++ escapeJsonString v ++ "\""
        Nothing -> ""

-- Format different body parts to JSON
textToJson :: String -> String
textToJson str = "\"" ++ escapeJsonString str ++ "\""

boldToJson :: BodyPart -> String
boldToJson bp = "{\"bold\": " ++ bodyPartToJson bp ++ "}"

italicToJson :: BodyPart -> String
italicToJson bp = "{\"italic\": " ++ bodyPartToJson bp ++ "}"

codeToJson :: BodyPart -> String
codeToJson bp = "{\"code\": " ++ bodyPartToJson bp ++ "}"

linkToJson :: String -> [BodyPart] -> String
linkToJson url bps =
    "{\"link\": {\"url\": \"" ++ escapeJsonString url ++ "\",\"content\": ["
    ++ intercalate "," (map bodyPartToJson bps) ++ "]}}"

imageToJson :: String -> [BodyPart] -> String
imageToJson url alts =
    "{\"image\": {\"url\": \"" ++ escapeJsonString url ++ "\",\"alt\": [" ++
    intercalate "," (map bodyPartToJson alts) ++ "]}}"

-- Updated codeBlockToJson for [BodyPart]
codeBlockToJson :: [BodyPart] -> String
codeBlockToJson bps =
    "{\"codeblock\": [" ++ intercalate "," (map bodyPartToJson bps) ++ "]}"

paragraphToJson :: [BodyPart] -> String
paragraphToJson bps =
    "[" ++ intercalate "," (map bodyPartToJson bps) ++ "]"

sectionToJson :: Maybe String -> [BodyPart] -> String
sectionToJson title bps =
    "{\"section\": {" ++ titleToJson title ++ "\"content\": [" ++
    intercalate "," (map bodyPartToJson bps) ++ "]}}"
  where
    titleToJson (Just t) = "\"title\": \"" ++ escapeJsonString t ++ "\","
    titleToJson Nothing  = ""

listToJson :: [Item] -> String
listToJson items =
    "{\"list\": [" ++ intercalate "," (map itemToJson items) ++ "]}"

itemToJson :: Item -> String
itemToJson (Item bp) = bodyPartToJson bp

-- Summarize the body parts to JSON
bodyPartToJson :: BodyPart -> String
bodyPartToJson (Text str) = textToJson str
bodyPartToJson (Bold bp) = boldToJson bp
bodyPartToJson (Italic bp) = italicToJson bp
bodyPartToJson (Code bp) = codeToJson bp
bodyPartToJson (Link url bp) = linkToJson url bp
bodyPartToJson (Image url bp) = imageToJson url bp
bodyPartToJson (Paragraph bps) = paragraphToJson bps
bodyPartToJson (Section title bp) = sectionToJson title bp
bodyPartToJson (CodeBlock bp) = codeBlockToJson bp
bodyPartToJson (List items) = listToJson items

bodyToJson :: [BodyPart] -> String
bodyToJson content =
    "[" ++
    intercalate "," (map bodyPartToJson content) ++
    "]"

-- Convert a document to JSON String
documentToJson :: Document -> String
documentToJson (Document header content) =
    "{\"header\": " ++ headerToJson header ++ ",\"body\": ["
    ++ intercalate "," (map bodyPartToJson content)
    ++ "]}"


--ajouter des maybe pour les cas d'erreur
--parseDocument :: JsonValue -> Maybe Document
--gérer les espacle et tabulations
--passer de String à Document
