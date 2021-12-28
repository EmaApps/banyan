module Banyan.Model.Patch where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Banyan.Graph as G
import Banyan.ID (parseUUIDFileName)
import qualified Banyan.Markdown as Markdown
import Banyan.Model
import qualified Ema.Helper.FileSystem as EmaFS
import System.FilePattern (FilePattern)

data FileType = FTMd | FTDot | FTStatic
  deriving (Eq, Show, Ord)

watching :: [(FileType, FilePattern)]
watching =
  [ (FTDot, "*.dot"),
    (FTMd, "*.md"),
    (FTStatic, "*")
  ]

ignoring :: [FilePattern]
ignoring = mempty

patchModel ::
  MonadIO m =>
  -- | Type of file being patched.
  FileType ->
  -- | Relative path of the file.
  FilePath ->
  -- | Specific action on the file, along with its absolute path (if it still
  -- exists).
  EmaFS.FileAction FilePath ->
  m (Model -> Model)
patchModel ftype fp action =
  fmap (maybe id (. modelClearError fp)) . runMaybeT $ do
    case ftype of
      FTDot -> case action of
        EmaFS.Delete ->
          pure $ modelSetGraph AM.empty
        EmaFS.Refresh _ absPath -> do
          s <- liftIO $ readFileText absPath
          pure $
            G.parseDot s
              & either
                (modelAddError fp . BadGraph)
                (modelSetGraph . G.buildGraph)
      FTMd -> do
        uuid <- hoistMaybe $ parseUUIDFileName ".md" fp
        -- Reset the next id, because a .md file may have been added (or
        -- deleted).  Ideally we should do this only on additions (and possibly
        -- on deletion), and not no modifications.
        liftA2 (.) modelResetNextID $ case action of
          EmaFS.Refresh _ absPath -> do
            Markdown.parseMarkdown absPath
              <&> either
                (modelAddError fp . BadMarkdown)
                (modelAdd uuid)
          EmaFS.Delete -> do
            pure $ modelDel uuid
      FTStatic -> case action of
        EmaFS.Refresh _ absPath -> do
          pure $ modelAddFile fp absPath
        EmaFS.Delete ->
          pure $ modelDelFile fp
