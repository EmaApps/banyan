module Banyan.Route where

import Banyan.ID (NodeID, parseUUIDFileName)
import Banyan.Model
import Control.Lens.Operators ((^.))
import qualified Data.Map.Strict as Map
import Ema (Ema (..))

data Route
  = RIndex
  | RNode NodeID
  deriving (Show)

instance Ema Model (Either FilePath Route) where
  encodeRoute _model =
    either id $ \case
      RIndex -> "index.html"
      RNode uuid -> show uuid <> ".html"
  decodeRoute model = \case
    "index.html" -> Just $ Right RIndex
    (parseUUIDFileName ".html" -> Just uuid) ->
      Right (RNode uuid) <$ modelLookup uuid model
    fp -> do
      absPath <- Map.lookup fp (model ^. modelFiles)
      pure $ Left absPath
  allRoutes m =
    (Right <$> RIndex : (RNode <$> Map.keys (m ^. modelNodes)))
      <> fmap Left (Map.keys $ m ^. modelFiles)
