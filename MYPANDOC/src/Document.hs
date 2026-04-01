{-
-- EPITECH PROJECT, 2025
-- Pandoc
-- File description:
-- main
-}
module Document(Document(..), Header(..), BodyPart(..), Item(..)) where
data Document = Document {
    header :: Header
    , content :: [BodyPart]
} deriving (Show, Eq)

data Header = Header {
    title :: Maybe String
    , author :: Maybe String
    , date :: Maybe String
} deriving (Show, Eq)

data BodyPart
    = Text String
    | Bold BodyPart
    | Italic BodyPart
    | Code BodyPart
    | Link String [BodyPart]
    | Image String [BodyPart]
    | Paragraph [BodyPart]
    | Section (Maybe String) [BodyPart]
    | CodeBlock [BodyPart]
    | List [Item]
    deriving (Show, Eq)

newtype Item = Item BodyPart deriving (Show, Eq)

