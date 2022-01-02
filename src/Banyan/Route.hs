module Banyan.Route where

import Banyan.ID (NodeID, parseIDFileName)
import Banyan.Model
import Control.Lens.Operators ((^.))
import qualified Data.Map.Strict as Map
import Ema (Ema (..))

data Route
  = RIndex
  | RNode NodeID
  deriving (Show, Eq, Ord)

instance Ema Model (Either FilePath Route) where
  encodeRoute _model =
    either id $ \case
      RIndex -> "index.html"
      RNode uuid -> show uuid <> ".html"
  decodeRoute model = \case
    "index.html" -> Just $ Right RIndex
    (parseIDFileName ".html" -> Just uuid) ->
      Right (RNode uuid) <$ modelLookup uuid model
    fp -> do
      guard $ Map.member fp (model ^. modelFiles)
      pure $ Left fp
  allRoutes m =
    -- TODO: Don't generate pages for leaf nodes (they are not linked to)
    (Right <$> RIndex : (RNode <$> Map.keys (m ^. modelNodes)))
      <> fmap Left (Map.keys $ m ^. modelFiles)
