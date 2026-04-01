{-
-- EPITECH PROJECT, 2024
-- makefile
-- File description:
-- Haskell Learning
-}

module Xml_converter(documentToXML) where
import Document
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf, intercalate)

takeWhiler :: String -> String
takeWhiler [] = []
takeWhiler str = takeWhile (/= '>') str

dropWhiler :: String -> String
dropWhiler [] = []
dropWhiler str = drop 1 (dropWhile (/= '>') str)

goodInline :: String -> String
goodInline [] = []
goodInline (x:xs)
    | isClosingTag (x:xs) && isOpeningTag (dropWhiler xs) =
        x : (takeWhiler xs) ++ ">\n" ++ goodInline (dropWhiler xs)
    | otherwise = x : goodInline xs
  where
    isClosingTag str = "</" `isPrefixOf` str
    isOpeningTag str = "<" `isPrefixOf` str && not (isClosingTag str)

documentToXML :: Document -> String
documentToXML (Document hdr bodyParts) =
    "<document>" ++
    headerToXML hdr ++
    "<body>" ++
    goodInline body ++
    "</body>" ++
    "</document>"
    where
        body = concatMap bodyPartToXML bodyParts

headerToXML :: Header -> String
headerToXML (Header t a d) = 
    "<header title=\"" ++ escapeXML (fromMaybe "" t) ++ "\">" ++
    authorXML ++ dateXML ++
    "</header>\n"
    where
        authorXML = maybe "" (\auth -> 
            "<author>" ++ escapeXML auth ++ "</author>\n") a
        dateXML = maybe "" (\dt -> "<date>" ++ escapeXML dt ++ "</date>") d

escapeXML :: String -> String
escapeXML = concatMap escapeChar
    where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar c   = [c]

bodyPartToXML :: BodyPart -> String
bodyPartToXML part = case part of
    Text str       -> escapeXML str
    Bold bp        -> boldToXML bp;Italic bp      -> italicToXML bp
    Code bp        -> codeToXML bp
    Link url bp    -> linkToXML url bp
    Image url bp   -> imageToXML url bp
    Paragraph ps   -> paragraphToXML ps
    Section mTitle ps -> sectionToXML mTitle ps
    CodeBlock bp   -> codeBlockToXML bp
    List items     -> listToXML items

boldToXML :: BodyPart -> String
boldToXML = inlineTag "bold"

italicToXML :: BodyPart -> String
italicToXML = inlineTag "italic"

codeToXML :: BodyPart -> String
codeToXML = inlineTag "code"

inlineTag :: String -> BodyPart -> String
inlineTag tagName bp =
    "<" ++ tagName ++ ">" ++
    bodyPartToXML bp ++
    "</" ++ tagName ++ ">"

linkToXML :: String -> [BodyPart] -> String
linkToXML url bp = 
    "<link url=\"" ++ escapeXML url ++ "\">" 
    ++ concatMap bodyPartToXML bp ++ "</link>"

imageToXML :: String -> [BodyPart] -> String
imageToXML url bp = 
    "<image url=\"" ++ escapeXML url ++ "\">" 
    ++ concatMap bodyPartToXML bp ++ "</image>"

paragraphToXML :: [BodyPart] -> String
paragraphToXML parts = 
    "<paragraph>" ++ 
    concatMap bodyPartToXML parts ++ "</paragraph>"

sectionToXML :: Maybe String -> [BodyPart] -> String
sectionToXML mTitle part = 
    "<section" ++ maybe "" (\t -> " title=\"" ++ escapeXML t ++ "\"") mTitle 
    ++ ">" 
    ++ concatMap bodyPartToXML part ++ "</section>"

codeBlockToXML :: [BodyPart] -> String
codeBlockToXML bp =
    "<codeblock>" ++
    concatMap bodyPartToXML bp ++
    "</codeblock>"

listToXML :: [Item] -> String
listToXML items =
    "<list>" ++ concatMap itemToXML items ++ "</list>"

itemToXML :: Item -> String
itemToXML (Item bp) = bodyPartToXML bp
