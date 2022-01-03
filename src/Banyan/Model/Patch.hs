module Banyan.Model.Patch where

import Banyan.ID (parseIDFileName)
import qualified Banyan.Markdown as Markdown
import Banyan.Model
import Banyan.Model.Hash
import Data.Dependent.Sum
import Data.Some (Some (Some), withSome)
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as EmaFS
import System.FilePath ((</>))
import System.FilePattern (FilePattern)

data FileType = FTMd | FTStatic (Some HashMode)
  deriving (Eq, Show, Ord)

watching :: Ema.CLI.Action -> [(FileType, FilePattern)]
watching emaAction =
  [ (FTMd, "*.md"),
    (FTStatic (Some HashContents), "*.css"),
    (FTStatic fastHashMode, "*")
  ]
  where
    fastHashMode = case emaAction of
      -- In live server, try not to hash unless necessary.
      Ema.CLI.Run -> Some HashUUID
      -- All static file URLs are hashed in generated site, to prevent stale
      -- browser cache.
      _ -> Some HashContents

ignoring :: [FilePattern]
ignoring = mempty

patchModel ::
  MonadIO m =>
  -- | Base directory
  -- TODO: This is hacky. Instead of Ema support monadic `Model -> m Model`?
  FilePath ->
  -- | Type of file being patched.
  FileType ->
  -- | Relative path of the file.
  FilePath ->
  -- | Specific action on the file, along with its absolute path (if it still
  -- exists).
  EmaFS.FileAction FilePath ->
  m (Model -> Model)
patchModel baseDir ftype fp action =
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
      FTStatic hashMode -> case action of
        EmaFS.Refresh _ _ -> do
          let absPath = baseDir </> fp
          hash :: FileHash <- withSome hashMode $ \m -> (m ==>) <$> computeHash m absPath
          pure $ modelAddFile hash fp absPath
        EmaFS.Delete ->
          pure $ modelDelFile fp
