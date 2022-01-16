{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Banyan.Markdown
  ( Meta (..),
    Pandoc,
    parseMarkdown,
    renderPandoc,
  )
where

import Banyan.ID (NodeID)
import Data.Aeson (FromJSON (parseJSON))
import Data.Default (Default (..))
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Ema.Helper.Markdown as Markdown
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options
import Text.Pandoc.Writers.HTML (writeHtml5String)

-- | Creation time in UTC.
newtype CreatedTime = CreatedTime UTCTime
  deriving newtype (Show, Eq, Ord)

data Meta = Meta
  { title :: Maybe Text,
    -- | Date will be used to sort a list of `Meta`s.
    date :: Maybe CreatedTime,
    parent :: Maybe NodeID
  }
  deriving (Show, Generic, Eq, FromJSON)

instance Ord Meta where
  compare = compare `on` (Down . date)

instance FromJSON CreatedTime where
  parseJSON = parseCreatedTime <=< parseJSON

parseCreatedTime :: (MonadFail m, Alternative m) => Text -> m CreatedTime
parseCreatedTime (toString -> s) = do
  utct <- parseTimeM False defaultTimeLocale dateFormat s <|> iso8601ParseM s
  pure $ CreatedTime utct
  where
    dateFormat = "%Y-%m-%dT%H:%M:%S"

instance Default Meta where
  def = Meta Nothing Nothing Nothing

parseMarkdown :: MonadIO m => FilePath -> m (Either Text (Maybe Meta, Pandoc))
parseMarkdown fp = do
  s <- readFileText fp
  pure $ Markdown.parseMarkdownWithFrontMatter @Meta Markdown.fullMarkdownSpec fp s

-- Might have to rewrite this to use commonmark-hs, so I can specify wiki-link
-- rendering.
renderPandoc :: Pandoc -> H.Html
renderPandoc doc =
  H.div ! A.class_ "prose prose-headings:my-1 prose-h1:text-center prose-h1:bg-gray-50 prose-h1:mt-0 pb-1" $ do
    H.unsafeByteString . either (error . show) encodeUtf8 $
      runPure $ writeHtml5String writerSettings doc
  where
    writerSettings :: WriterOptions
    writerSettings = def {writerExtensions = exts}
    exts :: Extensions
    exts =
      mconcat
        [ extensionsFromList
            [ Ext_fenced_code_attributes,
              Ext_auto_identifiers,
              Ext_smart
            ],
          githubMarkdownExtensions
        ]
