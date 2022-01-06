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

data SiteRoute
  = SRHtml Route
  | SRStatic FilePath
  deriving (Show, Eq, Ord)

instance Ema Model SiteRoute where
  encodeRoute _model = \case
    SRStatic fp -> fp
    SRHtml r -> case r of
      RIndex -> "index.html"
      RNode uuid -> show uuid <> ".html"
  decodeRoute model = \case
    "index.html" -> pure $ SRHtml RIndex
    (parseIDFileName ".html" -> Just uuid) ->
      SRHtml (RNode uuid) <$ modelLookup uuid model
    fp -> do
      guard $ Map.member fp (model ^. modelFiles)
      pure $ SRStatic fp
  allRoutes m =
    -- TODO: Don't generate pages for leaf nodes (they are not linked to)
    (SRHtml <$> RIndex : (RNode <$> Map.keys (m ^. modelNodes)))
      <> fmap SRStatic (Map.keys $ m ^. modelFiles)
