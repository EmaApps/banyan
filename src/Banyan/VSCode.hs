{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | Integration with VSCode extension
module Banyan.VSCode
  ( VSCodeAction (..),
    Action (..),
    renderVSCodeAction,
  )
where

import Banyan.ID
import Control.Exception.Safe (MonadThrow)
import Data.Some (Some)
import qualified Ema.CLI
import NeatInterpolation (text)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Show
import Text.URI (URI)
import qualified Text.URI as URI
import qualified Text.URI.QQ as URIQQ

data VSCodeAction
  = VSCodeAction FilePath Action
  deriving (Eq)

instance Show VSCodeAction where
  show (VSCodeAction _ action) =
    case action of
      NewNode nid Nothing ->
        "Create new top-level node " <> Text.Show.show nid
      NewNode nid (Just pid) ->
        "Create new child-node " <> Text.Show.show nid <> " under " <> Text.Show.show pid
      EditNode nid ->
        "Edit node " <> Text.Show.show nid

data Action
  = NewNode NodeID (Maybe NodeID)
  | EditNode NodeID
  deriving (Eq, Show)

-- | Base URI to invoke the vscode-extension for banyan
extUri :: URI
extUri = [URIQQ.uri|vscode://srid.banyan/|]

baseDirParam :: MonadThrow m => Text -> m URI.QueryParam
baseDirParam baseUri = URI.QueryParam [URIQQ.queryKey|baseDir|] <$> URI.mkQueryValue baseUri

mkUri :: VSCodeAction -> URI
mkUri (VSCodeAction baseDir action) = case action of
  NewNode nid mParent ->
    extUri
      { URI.uriPath =
          (False,) <$> do
            nid' <- URI.mkPathPiece @Maybe (show nid)
            pure $ [URIQQ.pathPiece|new|] :| [nid'],
        URI.uriQuery =
          catMaybes
            [ mParent >>= \parent ->
                URI.QueryParam
                  <$> URI.mkQueryKey "parent" <*> URI.mkQueryValue (show parent),
              baseDirParam (toText baseDir)
            ]
      }
  EditNode nid ->
    extUri
      { URI.uriPath =
          (False,) <$> do
            nid' <- URI.mkPathPiece @Maybe (show nid)
            pure $ [URIQQ.pathPiece|edit|] :| [nid'],
        URI.uriQuery =
          catMaybes
            [baseDirParam (toText baseDir)]
      }

renderVSCodeAction :: Some Ema.CLI.Action -> VSCodeAction -> H.Html
renderVSCodeAction (Ema.CLI.isLiveServer -> True) action = do
  let uri = mkUri action
  H.a
    ! A.class_ ""
    ! A.href (H.toValue $ URI.render uri)
    ! A.title (show action)
    $ actionTitle
  where
    actionTitle :: H.Html
    actionTitle = case action of
      VSCodeAction _ (NewNode _ _) ->
        H.unsafeByteString . encodeUtf8 $
          [text| 
          <svg xmlns="http://www.w3.org/2000/svg" class="w-4 inline" viewBox="0 0 20 20" fill="currentColor">
  <path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm1-11a1 1 0 10-2 0v2H7a1 1 0 100 2h2v2a1 1 0 102 0v-2h2a1 1 0 100-2h-2V7z" clip-rule="evenodd" />
</svg>
          |]
      _ ->
        H.unsafeByteString . encodeUtf8 $
          [text|
        <svg xmlns="http://www.w3.org/2000/svg" class="w-4 inline" fill="none" viewBox="0 0 24 24" stroke="currentColor">
  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z" />
</svg>
        |]
renderVSCodeAction _ _ =
  mempty
