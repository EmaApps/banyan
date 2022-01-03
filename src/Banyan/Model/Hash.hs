{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Banyan.Model.Hash where

import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Digest.Pure.MD5 (md5)
import Data.GADT.Compare.TH
  ( DeriveGCompare (deriveGCompare),
    DeriveGEQ (deriveGEq),
  )
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID

data HashMode a where
  HashContents :: HashMode Text
  HashUUID :: HashMode UUID

computeHash :: MonadIO m => HashMode a -> FilePath -> m a
computeHash mode fp = do
  case mode of
    HashContents -> do
      s <- readFileBS fp
      pure $ show $ md5 $ toLazy s
    HashUUID -> liftIO UUID.nextRandom

type FileHash = DSum HashMode Identity

hashText :: FileHash -> Text
hashText = \case
  HashContents :=> Identity s -> s
  HashUUID :=> Identity uuid -> show uuid

$(deriveGShow ''HashMode)
$(deriveGEq ''HashMode)
$(deriveGCompare ''HashMode)
$(deriveArgDict ''HashMode)
