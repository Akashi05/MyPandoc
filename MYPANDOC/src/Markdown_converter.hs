{-
-- EPITECH PROJECT, 2024
-- makefile
-- File description:
-- Haskell Learning
-}

module Markdown_converter(printMarkdown) where
import Document
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

--newtype Item = Item BodyPart deriving (Show, Eq)

printMarkdown :: Document -> String
printMarkdown (Document header content) =
    printHeader header ++ "\n\n" ++ printContent content 1

printHeader :: Header -> String
printHeader (Header title author date) =
    "---\n" ++
    "title: " ++ fromMaybe "" title ++ "\n" ++
    maybe "" (\a -> "author: " ++ a ++ "\n") author ++
    maybe "" (\d -> "date: " ++ d ++ "\n") date ++
    "---"

printContent :: [BodyPart] -> Int -> String
printContent [] _ = ""
printContent (x:xs) r = 
    let current = case x of
            Section _ _ -> printBodyPart x r ++ printContent xs (r + 1)
            _           -> printBodyPart x r ++ printContent xs r
    in current


printBodyPart :: BodyPart -> Int -> String
printBodyPart (Text str) _ = str
printBodyPart (Bold part) r = "**" ++ printBodyPart part r ++ "**"
printBodyPart (Italic part) r = "*" ++ printBodyPart part r ++ "*"
printBodyPart (Code part) r = "`" ++ printBodyPart part r ++ "`"
printBodyPart (Link url parts) r = 
    "[" ++ concatMap (`printBodyPart` r) parts ++ "](" ++ url ++ ")"
printBodyPart (Image url parts) r =
    "![" ++ concatMap (`printBodyPart` r) parts ++ "](" ++ url ++ ")"
printBodyPart (Paragraph parts) r = 
    let content = concatMap (`printBodyPart` r) parts
    in if null content then "" else content ++ "\n\n"
printBodyPart (CodeBlock parts) _ = 
    "```\n" ++ concatMap (`printBodyPart` 0) parts ++ "\n```\n\n"
printBodyPart (List items) r = 
    concatMap (\(Item part) -> "-" ++ printBodyPart part r) items
printBodyPart (Section title parts) r = 
    let header = printSection title r
        body = concatMap (`printBodyPart` (r + 1)) parts
    in header ++ (if null body then "" else "\n" ++ body)


printSection :: Maybe String -> Int -> String
printSection (Just title) r = replicate r '#' ++ " " ++ title ++ "\n"
printSection Nothing r = replicate r '#' ++ "\n"

