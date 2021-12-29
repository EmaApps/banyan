{-# LANGUAGE DerivingStrategies #-}

module Banyan.Graph where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM
import Data.NanoID (NanoID (..))
import Data.Tree (Forest)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L

type NodeID = NanoID

data Dot
  = Digraph Text [(NodeID, NodeID)]
  deriving (Eq, Show)

buildGraph :: Dot -> AM.AdjacencyMap NodeID
buildGraph (Digraph _ es) =
  AM.edges es

toTree :: AM.AdjacencyMap NodeID -> Forest NodeID
toTree = AM.dfsForest

getDescendents :: NodeID -> AM.AdjacencyMap NodeID -> [NodeID]
getDescendents nid g =
  toList $ AM.postSet nid g

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
    -- TODO: Support `a -> {b, c}` syntax.
    to <- lexeme $ M.some M.alphaNumChar
    void $ lexeme . M.string $ ";"
    pure (NanoID (encodeUtf8 from), NanoID (encodeUtf8 to))
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
