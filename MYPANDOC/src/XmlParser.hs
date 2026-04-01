{-
-- EPITECH PROJECT, 2025
-- Pandoc
-- File description:
-- main
-}
module XmlParser where
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import JsonToDoc
import Data.List (find)
import Data.Char (isAlphaNum)
import Control.Monad (guard)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Document


data XmlValue
    = XmlElement String [(String, String)] [XmlValue]
    | XmlText String
    deriving (Show, Eq)

parseQuotedString :: Parser String
parseQuotedString str =
    let (value, rest) = span (/= '"') str
    in if null rest
    then Nothing
    else Just (value, tail rest)


parseTextContent :: Parser XmlValue
parseTextContent str =
    case parseSome parseValidChar str of
        Just (text, rest) ->
            Just (XmlText text, skipWhitespace rest)
        Nothing ->
            Nothing

parseValidChar :: Parser Char
parseValidChar str =
    case parseAnyChar ['\x20'..'\x7E'] str of
        Just (c, rest)
            | c /= '<' -> Just (c, rest)
            | otherwise -> Nothing
        Nothing -> Nothing

parseTagName :: Parser String
parseTagName str =
    let (name, rest) = span (\c -> isAlphaNum c || c == '-' || c == '_')
            (skipWhitespace str)
    in if null name
    then Nothing
    else Just (name, skipWhitespace rest)

parseAttributeName :: Parser String
parseAttributeName str =
    let (name, rest) = span (\c -> isAlphaNum c || c == '-'
            || c == '_' || c == ':') (skipWhitespace str)
    in if null name
    then Nothing
    else Just (name, skipWhitespace rest)

parseAttributeValue :: Parser String
parseAttributeValue str =
    case parseCharWithWhitespace '=' str of
        Just (_, rest1) -> case parseCharWithWhitespace '"' rest1 of
            Just (_, rest2) -> case parseQuotedString rest2 of
                Just (value, rest3) -> Just (value, skipWhitespace rest3)
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing

parseOneAttribute :: Parser (String, String)
parseOneAttribute str =
    case parseAttributeName str of
        Just (name, rest1) ->
            case parseAttributeValue rest1 of
                Just (value, rest2) -> Just ((name, value), rest2)
                Nothing -> Nothing
        Nothing -> Nothing

parseAttributes :: Parser [(String, String)]
parseAttributes = parseMany parseOneAttribute

parseOpeningTag :: Parser (String, [(String, String)], Bool)
parseOpeningTag str = do
    (_, rest1) <- parseCharWithWhitespace '<' str
    (name, rest2) <- parseTagName rest1
    (attrs, rest3) <- parseAttributes rest2
    parseClosingInfo name attrs rest3

parseClosingInfo :: String -> [(String, String)] -> String
    -> Maybe ((String, [(String, String)], Bool), String)
parseClosingInfo name attrs str = case parseCharWithWhitespace '/' str of
    Just (_, rest) -> Just ((name, attrs, True), rest)
    Nothing -> do
        (_, rest) <- parseCharWithWhitespace '>' str
        Just ((name, attrs, False), rest)

parseXmlString :: Parser String
parseXmlString str =
    case parseCharWithWhitespace '"' str of
        Just (_, rest) ->
            let (text, remaining) = span (/= '"') rest
            in case parseCharWithWhitespace '"' remaining of
                Just (_, rest2) -> Just (text, skipWhitespace rest2)
                Nothing -> Nothing
        Nothing -> Nothing

parseXmlElement :: Parser XmlValue
parseXmlElement str = do
    ((tagName, attrs, isSelfClosing), rest) <- parseOpeningTag str
    if isSelfClosing 
        then Just (XmlElement tagName attrs [], rest)
        else parseNonSelfClosingElement tagName attrs rest

parseNonSelfClosingElement :: String -> [(String, String)]
    -> String -> Maybe (XmlValue, String)
parseNonSelfClosingElement tagName attrs str = do
    (children, rest2) <- parseMany parseXmlContent str
    (_, rest3) <- parseFixedString "</" rest2
    (closingTagName, rest4) <- parseTagName rest3
    guard (closingTagName == tagName)
    (_, rest5) <- parseCharWithWhitespace '>' rest4
    Just (XmlElement tagName attrs children, rest5)

parseXmlContent :: Parser XmlValue
parseXmlContent =
    parseOr parseTextContent parseXmlElement

parseFixedString :: String -> Parser String
parseFixedString expected str =
    let (prefix, rest) = splitAt (length expected) str
    in if prefix == expected
    then Just (prefix, skipWhitespace rest)
    else Nothing

lookupAttribute :: String -> [(String, String)] -> Maybe String
lookupAttribute = lookup

findXmlElement :: String -> [XmlValue] -> Maybe XmlValue
findXmlElement tagName elements =
    find (\element -> case element of
            XmlElement name _ _ -> name == tagName
            _ -> False
    ) elements

findXmlText :: Maybe XmlValue -> Maybe String
findXmlText (Just (XmlElement _ _ [XmlText text])) = Just text
findXmlText (Just (XmlText text)) = Just text
findXmlText (Just _) = Nothing
findXmlText _ = Nothing

findHeader :: [XmlValue] -> Maybe Header
findHeader imbrications = 
    let headerElement = findXmlElement "header" imbrications
    in case headerElement of
        Just (XmlElement _ attributs imbric) ->
            let title = lookupAttribute "title" attributs
                authorElement = findXmlElement "author" imbric
                dateElement = findXmlElement "date" imbric
                in let author = findXmlText authorElement
                in let date = findXmlText dateElement
                    in Just (Header title author date); _ -> Nothing

convertToBodyPart :: XmlValue -> Maybe BodyPart
convertToBodyPart (XmlElement "paragraph" _ children) =
    Just (Paragraph (mapMaybe convertToBodyPart children))
convertToBodyPart (XmlElement "bold" _ children) =
    Just (Bold (fromMaybe (Text "")
    (convertToBodyPart =<< listToMaybe children)))
convertToBodyPart (XmlElement "italic" _ children) =
    Just (Italic (fromMaybe (Text "")
    (convertToBodyPart =<< listToMaybe children)))
convertToBodyPart (XmlElement "code" _ children) =
    Just (Code (fromMaybe (Text "")
    (convertToBodyPart =<< listToMaybe children)))
convertToBodyPart (XmlElement "codeblock" _ children) =
    Just (CodeBlock (mapMaybe convertToBodyPart children))
convertToBodyPart (XmlElement "link" attrs children) =
    case lookupAttribute "url" attrs of
        Just url -> Just (Link url (mapMaybe convertToBodyPart children))
        Nothing  -> Nothing
convertToBodyPart (XmlElement "image" attrs children) =
    case lookupAttribute "url" attrs of
        Just url -> Just (Image url (mapMaybe convertToBodyPart children))
        Nothing  -> Nothing
convertToBodyPart (XmlElement "section" attrs children) =
    let title = lookupAttribute "title" attrs
        content = mapMaybe convertToBodyPart children
    in Just (Section title content)
convertToBodyPart (XmlElement "list" _ children) =
    Just (List (map (\x -> Item (fromMaybe (Text "") x))
    (map convertToBodyPart children)))
convertToBodyPart (XmlText text) = Just (Text text)
convertToBodyPart _ = Nothing

findBody :: [XmlValue] -> Maybe [BodyPart]
findBody imbrications =
    let bodyElement = findXmlElement "body" imbrications
    in case bodyElement of
        Just (XmlElement _ _ children) -> Just
            (mapMaybe convertToBodyPart children)
        _ -> Nothing

convertXmlToDocument :: XmlValue -> Maybe Document
convertXmlToDocument (XmlElement "document" _ children) = 
    let header = findHeader children
        body = findBody children
    in case (header, body) of
        (Just h, Just b) -> Just (Document h b)
        _ -> Nothing
convertXmlToDocument _ = Nothing

parseXmlToDocument :: String -> IO Document
parseXmlToDocument input = case parseXmlContent input of
        Nothing -> putStrLn "Error: Failed to parse XML content." >>
            exitWith(ExitFailure 84)
        Just (xmlValue, "") -> case convertXmlToDocument xmlValue of
            Nothing ->
                putStrLn "Error: Failed to convert XML to Document." >>
                exitWith (ExitFailure 84)
            Just document -> return document
        _-> putStrLn "Error: Invalid content" >> exitWith (ExitFailure 84)
