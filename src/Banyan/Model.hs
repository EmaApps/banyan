module Banyan.Model where

import qualified Algebra.Graph.AdjacencyMap as AM
import Banyan.ID (NodeID, randomId)
import Banyan.Markdown (Meta, Pandoc)
import qualified Data.Map.Strict as Map

type Node = (Maybe Meta, Pandoc)

data Error = BadGraph Text | BadMarkdown Text
  deriving (Show, Eq, Ord)

data Model = Model
  { _modelNodes :: Map NodeID Node,
    _modelGraph :: AM.AdjacencyMap NodeID,
    _modelNextUUID :: NodeID,
    _modelErrors :: Map FilePath Error
  }
  deriving (Show)

modelDel :: NodeID -> Model -> Model
modelDel fp (Model m g n e) = Model (Map.delete fp m) g n e

modelAdd :: NodeID -> Node -> Model -> Model
modelAdd fp s (Model m g n e) = Model (Map.insert fp s m) g n e

modelLookup :: NodeID -> Model -> Maybe Node
modelLookup k (Model m _ _ _) = Map.lookup k m

modelResetNextUUID :: (MonadIO m, HasCallStack) => m (Model -> Model)
modelResetNextUUID = do
  rid <- liftIO randomId
  pure $ \(Model m g _ e) ->
    if Map.member rid m
      then error $ "NanoID collision: " <> show rid
      else Model m g rid e

modelSetGraph :: AM.AdjacencyMap NodeID -> Model -> Model
modelSetGraph g (Model m _ n e) = Model m g n e

modelAddError :: FilePath -> Error -> Model -> Model
modelAddError fp e (Model m g n es) = Model m g n (Map.insert fp e es)

modelClearError :: FilePath -> Model -> Model
modelClearError fp (Model m g n es) = Model m g n (Map.delete fp es)