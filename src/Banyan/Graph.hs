{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Banyan.Graph where

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

newtype Node = Node {unNode :: Text}
  deriving newtype (Eq, Ord, Show)

data Dot
  = Digraph Text [(Node, Node)]
  deriving (Eq, Show)

type Parser = M.Parsec Void Text

parseDot :: Text -> Either Text Dot
parseDot = do
  parse dotParser "graph.dot"
  where
    parse :: Parser a -> String -> Text -> Either Text a
    parse p fn =
      first (toText . M.errorBundlePretty)
        . M.parse (p <* M.eof) fn

dotParser :: Parser Dot
dotParser = do
  void $ lexeme $ M.string "digraph"
  graphName <- lexeme $ M.some M.alphaNumChar
  void $ lexeme $ M.string "{"
  edges <- M.many $ do
    from <- lexeme $ M.some M.alphaNumChar
    void $ lexeme $ M.string "->"
    to <- lexeme $ M.some M.alphaNumChar
    void $ lexeme . M.string $ ";"
    pure (Node (toText from), Node (toText to))
  void $ lexeme $ M.string "}"
  pure $ Digraph (toText graphName) edges

sc :: Parser ()
sc =
  L.space
    M.space1
    (L.skipLineComment "//")
    M.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc