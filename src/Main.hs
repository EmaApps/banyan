{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Main where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Banyan.Graph as G
import Banyan.ID
import qualified Banyan.Markdown as Markdown
import Banyan.Model
import Control.Exception (throw)
import qualified Data.Map.Strict as Map
import Ema (Ema (..))
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as EmaFS
import qualified Ema.Helper.Tailwind as Tailwind
import qualified Emanote
import qualified Emanote.Source.Loc as Loc
import NeatInterpolation (text)
import qualified Shower
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Route
  = RIndex
  | RNode NodeID
  deriving (Show)

newFileCli :: NodeID -> Text
newFileCli (show -> nid) =
  [text|
    echo "---\ndate: $(date -u +'%Y-%m-%dT%H:%M:%S')\n---\n\n" >${nid}.md; cat >>${nid}.md
  |]

instance Ema Model Route where
  encodeRoute _model =
    \case
      RIndex -> "index.html"
      RNode uuid -> show uuid <> ".html"
  decodeRoute model = \case
    "index.html" -> Just RIndex
    (parseUUIDFileName ".html" -> Just uuid) ->
      RNode uuid <$ modelLookup uuid model
    _ -> Nothing
  allRoutes (Model m _ _) =
    RIndex : (RNode <$> Map.keys m)

main :: IO ()
main = do
  Ema.runEma (\act m -> Ema.AssetGenerated Ema.Html . render act m) $ \_act model -> do
    let layers = Loc.userLayers (one "content")
    nextId <- liftIO randomId
    Emanote.emanate
      layers
      [ (FTDot, "*.dot"),
        (FTMd, "*.md")
      ]
      mempty
      model
      (Model Map.empty AM.empty nextId)
      patchModel

data Error = BadGraph Text | BadMarkdown Text
  deriving (Show, Exception)

data FileType = FTMd | FTDot
  deriving (Eq, Show, Ord)

patchModel :: MonadIO m => FileType -> FilePath -> EmaFS.FileAction (NonEmpty (Loc.Loc, FilePath)) -> m (Model -> Model)
patchModel ftype fp action = do
  print fp
  fmap (fromMaybe id) . runMaybeT $ do
    case ftype of
      FTDot ->
        case action of
          EmaFS.Delete -> error "not implemented"
          EmaFS.Refresh _ (Loc.locResolve . head -> absPath) -> do
            s <- liftIO $ readFileText absPath
            case traceShowId (G.parseDot s) of
              Left e ->
                -- FIXME: this doesn't show error (at least on macOS M1)
                throw $ BadGraph e
              Right (G.buildGraph -> g) ->
                pure $ modelSetGraph g
      FTMd -> do
        uuid <- hoistMaybe $ parseUUIDFileName ".md" fp
        liftA2 (.) modelResetNextUUID $ case action of
          EmaFS.Refresh _ (Loc.locResolve . head -> absPath) -> do
            eRes <- Markdown.parseMarkdown absPath
            case eRes of
              Left err -> throw $ BadMarkdown err
              Right v ->
                pure $ modelAdd uuid v
          EmaFS.Delete -> do
            pure $ modelDel uuid

render :: Ema.CLI.Action -> Model -> Route -> LByteString
render emaAction model r =
  Tailwind.layout emaAction (H.title "Emanote-like" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2" $ do
        H.div ! A.class_ "bg-gray-200 p-2 rounded text-center" $ do
          H.pre $ H.toHtml $ newFileCli $ _modelNextUUID model
        case r of
          RIndex -> do
            "You are on the index page. "
            forM_ (Map.keys $ _modelNodes model) $ \uuid ->
              H.li $ H.code $ routeElem (RNode uuid) $ show uuid
          RNode uuid -> do
            "You are on the node page: " <> show uuid
            case modelLookup uuid model of
              Nothing -> "Not found"
              Just (mMeta, pandoc) -> do
                H.div ! A.class_ "border-2 m-4 p-2" $ Markdown.renderPandoc pandoc
                H.div $ H.toHtml $ show @Text mMeta
            routeElem RIndex "Go to Index"
      H.div ! A.class_ "font-mono text-xs flex items-center justify-center" $ do
        H.pre $ H.toHtml (Shower.shower $ _modelGraph model)
  where
    routeElem r' w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')
