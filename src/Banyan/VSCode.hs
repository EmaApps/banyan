{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | Integration with VSCode extension
module Banyan.VSCode where

import Banyan.ID
import Text.URI (URI)
import qualified Text.URI as URI
import qualified Text.URI.QQ as URIQQ

data VSCodeAction
  = NewNode NodeID (Maybe NodeID)
  deriving (Eq, Show)

mkUri :: VSCodeAction -> URI
mkUri (NewNode nid mParent) =
  [URIQQ.uri|vscode://srid.banyan/new/|]
    { URI.uriPath =
        (False,) <$> do
          nid' <- URI.mkPathPiece @Maybe (show nid)
          pure $ [URIQQ.pathPiece|new|] :| [nid'],
      URI.uriQuery =
        maybeToList $
          mParent >>= \parent ->
            URI.QueryParam
              <$> URI.mkQueryKey "parent" <*> URI.mkQueryValue (show parent)
    }
