module Banyan.Model where

import qualified Algebra.Graph.AdjacencyMap as AM
import Banyan.ID (NodeID, randomId)
import Banyan.Markdown (Meta, Pandoc)
import qualified Data.Map.Strict as Map

type Node = (Maybe Meta, Pandoc)

data Model = Model
  { _modelNodes :: Map NodeID Node,
    _modelGraph :: AM.AdjacencyMap NodeID,
    _modelNextUUID :: NodeID
  }
  deriving (Show)

modelDel :: NodeID -> Model -> Model
modelDel fp (Model m g n) = Model (Map.delete fp m) g n

modelAdd :: NodeID -> Node -> Model -> Model
modelAdd fp s (Model m g n) = Model (Map.insert fp s m) g n

modelLookup :: NodeID -> Model -> Maybe Node
modelLookup k (Model m _ _) = Map.lookup k m

modelResetNextUUID :: (MonadIO m, HasCallStack) => m (Model -> Model)
modelResetNextUUID = do
  rid <- liftIO randomId
  pure $ \(Model m g _) ->
    if Map.member rid m
      then error $ "NanoID collision: " <> show rid
      else Model m g rid

modelSetGraph :: AM.AdjacencyMap NodeID -> Model -> Model
modelSetGraph g (Model m _ n) = Model m g n
