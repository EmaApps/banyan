{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Banyan.Model where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Banyan.Graph as G
import Banyan.ID (NodeID, randomId)
import Banyan.Markdown (Meta (..), Pandoc)
import Banyan.Model.Hash
import Control.Lens.Combinators (view)
import Control.Lens.Operators ((%~), (.~), (^.))
import Control.Lens.TH (makeLenses)
import Data.Dependent.Sum
import qualified Data.Map.Strict as Map
import System.FilePath ((-<.>), (</>))
import UnliftIO.Directory (makeAbsolute)

type Node = (Maybe Meta, Pandoc)

data Error = BadGraph Text | BadMarkdown Text
  deriving (Show, Eq, Ord)

data Model = Model
  { _modelBaseDir :: FilePath,
    _modelNodes :: Map NodeID Node,
    _modelGraph :: AM.AdjacencyMap NodeID,
    _modelFiles :: Map FilePath (FileHash, FilePath),
    _modelNextID :: NodeID,
    _modelErrors :: Map FilePath Error
  }
  deriving (Show)

emptyModel :: MonadIO m => FilePath -> m Model
emptyModel baseDir' = do
  rid <- randomId
  baseDir <- makeAbsolute baseDir'
  pure $
    Model
      { _modelBaseDir = baseDir,
        _modelNodes = Map.empty,
        _modelGraph = AM.empty,
        _modelFiles = Map.empty,
        _modelNextID = rid,
        _modelErrors = Map.empty
      }

makeLenses ''Model

modelDel :: NodeID -> Model -> Model
modelDel nid =
  modelNodes
    %~ Map.delete nid
    >>> modelGraph
    %~ G.removeNode nid

-- | This will update if the node already exists.
modelAdd :: NodeID -> Node -> Model -> Model
modelAdd nid node =
  modelNodes
    %~ Map.insert nid node
    >>> modelGraph
    %~ G.addNodeWithParent nid mParent
  where
    mParent = do
      Meta {..} <- fst node
      parent

modelLookup :: NodeID -> Model -> Maybe Node
modelLookup k =
  Map.lookup k . view modelNodes

modelLookupFile :: FilePath -> Model -> Maybe (FileHash, FilePath)
modelLookupFile fp model = do
  Map.lookup fp $ model ^. modelFiles

-- | Return a hashed URL to file, where the hash changes if Haskell process was
-- restarted or the underyling file has been modified since last use.
modelFileUrl :: FilePath -> Model -> Maybe Text
modelFileUrl fp model = do
  (h, _) <- modelLookupFile fp model
  let q = "?" <> hashText h
  pure $ toText fp <> q

modelAddFile :: DSum HashMode Identity -> FilePath -> FilePath -> Model -> Model
modelAddFile hash fp absPath model =
  model & modelFiles %~ Map.insert fp (hash, absPath)

modelDelFile :: FilePath -> Model -> Model
modelDelFile fp =
  modelFiles
    %~ Map.delete fp

modelResetNextID :: (MonadIO m, HasCallStack) => m (Model -> Model)
modelResetNextID = do
  rid <- liftIO randomId
  pure $ \model -> case modelLookup rid model of
    Just _ -> error $ "NanoID collision: " <> show rid
    Nothing -> model & modelNextID .~ rid

modelAddError :: FilePath -> Error -> Model -> Model
modelAddError fp e =
  modelErrors %~ Map.insert fp e

modelClearError :: FilePath -> Model -> Model
modelClearError fp =
  modelErrors %~ Map.delete fp

-- | Path to the local Markdown file for a given NodeID.
modelNodePath :: NodeID -> Model -> FilePath
modelNodePath nid model =
  let fn = show nid -<.> ".md"
   in (model ^. modelBaseDir) </> fn

modelNodeEditUrlVSCode :: NodeID -> Model -> Text
modelNodeEditUrlVSCode nid model =
  "vscode://file" <> toText (modelNodePath nid model)