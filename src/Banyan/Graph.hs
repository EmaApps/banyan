{-# LANGUAGE DerivingStrategies #-}

module Banyan.Graph where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM
import Data.NanoID (NanoID (..))
import Data.Tree (Forest)

type NodeID = NanoID

addNodeWithParent :: NodeID -> Maybe NodeID -> AM.AdjacencyMap NodeID -> AM.AdjacencyMap NodeID
addNodeWithParent nid mPid g =
  -- First remove old parent relationship, if it exists.
  let oldEs = AM.preSet nid g
      g' = foldl' (\h oldParent -> AM.removeEdge oldParent nid h) g oldEs
   in AM.overlays [g', AM.vertex nid, maybe AM.empty (`AM.edge` nid) mPid]

removeNode :: NodeID -> AM.AdjacencyMap NodeID -> AM.AdjacencyMap NodeID
removeNode =
  AM.removeVertex

toTree :: AM.AdjacencyMap NodeID -> Forest NodeID
toTree g = AM.bfsForest (getRoots g) g

getRoots :: AM.AdjacencyMap NodeID -> [NodeID]
getRoots g = filter isRoot $ AM.vertexList g
  where
    isRoot nid =
      null $ AM.preSet nid g

getDescendents :: NodeID -> AM.AdjacencyMap NodeID -> [NodeID]
getDescendents nid g =
  toList $ AM.postSet nid g
