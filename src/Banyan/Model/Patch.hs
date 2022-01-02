module Banyan.Model.Patch where

import Banyan.ID (parseIDFileName)
import qualified Banyan.Markdown as Markdown
import Banyan.Model
import Control.Lens.Operators ((^.))
import qualified Ema.Helper.FileSystem as EmaFS
import System.FilePath ((</>))
import System.FilePattern (FilePattern)

data FileType = FTMd | FTStatic
  deriving (Eq, Show, Ord)

watching :: [(FileType, FilePattern)]
watching =
  [ (FTMd, "*.md"),
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
      FTMd -> do
        uuid <- hoistMaybe $ parseIDFileName ".md" fp
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
        EmaFS.Refresh _ _ -> do
          pure $ \model -> modelAddFile fp (model ^. modelBaseDir </> fp) model
        EmaFS.Delete ->
          pure $ modelDelFile fp
