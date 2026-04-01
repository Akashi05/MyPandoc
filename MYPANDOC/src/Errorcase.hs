{-
-- EPITECH PROJECT, 2022
-- $PROJECT_NAME
-- File description:
-- $DESCRIPTION
-}

module Errorcase(myMain) where

import System.Exit
import System.IO
import Data.Char (isAlpha)
import System.Environment (getArgs)
import Control.Monad (when, unless)
import JsonToDoc (documentToJson, parseContentToDocument) -- JSON-related functions
import Markdown_converter (printMarkdown) -- Markdown-related functions
import Xml_converter (documentToXML) -- XML-related functions
import Document
import Control.Exception (SomeException, try)
import XmlParser    

check_file_existence :: String -> IO Bool
check_file_existence f = do
    re <- try (readFile f) :: IO (Either SomeException String)
    case re of
        Left _ -> return False
        Right _ -> return True


-- Configuration data structure
data Conf = Conf {
    inputfile :: Maybe String,
    outputformat :: Maybe String,
    outputfile :: Maybe String,
    inputformat :: Maybe String
} deriving Show

-- Utility function to convert Maybe String to String
maybeToString :: Maybe String -> String
maybeToString (Just x) = x
maybeToString Nothing  = ""

-- Extract the file extension from a filename
typeChecker :: String -> String
typeChecker str = dropWhile (/= '.') str

-- Parse the input file content into a Document
parseFile :: Conf -> IO Document
parseFile info = do
    content <- readFile (maybeToString (inputfile info))
    case typeChecker (maybeToString (inputfile info)) of
        ".json" -> parseContentToDocument content
        ".xml" -> parseXmlToDocument content
        _ -> putStrLn "Not Handled!" >> exitWith(ExitFailure 84)

-- Convert the Document to the desired output format
convertDocument :: Document -> String -> String
convertDocument doc format =
    case format of
        "json"   -> documentToJson doc
        "xml"    -> documentToXML doc
        "markdown"     -> printMarkdown doc
        _         -> ""

handleOutput :: Conf -> String -> IO ()
handleOutput info result =
    case outputfile info of
        Just outputFile -> writeFile outputFile result
        Nothing         -> putStrLn result
-- Main file converter function
fileConverter :: Conf -> String -> IO ()
fileConverter info outputFormat = do
    doc <- parseFile info
    let result = convertDocument doc outputFormat
    if null result
        then putStrLn "Error: Unsupported output format."
            >> exitWith (ExitFailure 84)
        else handleOutput info result

-- Validate input file and format
validateInput :: String -> String -> IO ()
validateInput inputfile outputformat = do
    fileExists <- check_file_existence inputfile
    unless fileExists (putStrLn "Error: Input file does not exist."
        >> exitWith (ExitFailure 84))
    unless (outputformat `elem` ["json", "xml", "markdown"])
        $ putStrLn "Error: Invalid output format."
        >> exitWith (ExitFailure 84)

-- Handle command-line arguments
getOpts :: Conf -> [String] -> Maybe Conf
getOpts defaultConf [] = Just defaultConf
getOpts defaultConf (flag:value:xs) =
    case flag of
        "-i" -> getOpts defaultConf {inputfile = Just value} xs
        "-f" -> getOpts defaultConf {outputformat = Just value} xs
        "-o" -> getOpts defaultConf {outputfile = Just value} xs
        "-e" -> getOpts defaultConf {inputformat = Just value} xs
        _    -> Nothing
getOpts _ _ = Nothing

-- Main orchestrator logic
theCase :: Conf -> IO ()
theCase info
    | inputfile info == Nothing || outputformat info == Nothing =
        putStrLn "Error: Missing mandatory options (-i and -f)." >>
        exitWith (ExitFailure 84)
    | otherwise =
        validateInput (maybeToString (inputfile info))
        (maybeToString (outputformat info))
        >> fileConverter info (maybeToString (outputformat info))

-- That will main args take
myGetline :: [String] -> IO ()
myGetline args | length args `mod` 2 /= 0 || length args > 8 =
        putStrLn "Error: Invalid number of arguments." >>
        exitWith (ExitFailure 84)
    | otherwise = 
        let struct = Conf Nothing Nothing Nothing Nothing
        in case getOpts struct args of
            Just conf -> theCase conf
            Nothing   ->
                putStrLn "Error: Invalid arguments." >>
                exitWith (ExitFailure 84)

-- Main
myMain :: IO ()
myMain = do
    args <- getArgs
    myGetline args