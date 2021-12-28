{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Main where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Banyan.Graph as G
import Banyan.ID (NodeID, parseUUIDFileName, randomId)
import qualified Banyan.ID as ID
import qualified Banyan.Markdown as Markdown
import Banyan.Model
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
import qualified System.Environment as Env
import qualified Test.Tasty as T
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

instance Ema Model (Either FilePath Route) where
  encodeRoute _model =
    either id $ \case
      RIndex -> "index.html"
      RNode uuid -> show uuid <> ".html"

  decodeRoute model = \case
    "banyan.svg" -> Just $ Left "content/banyan.svg" -- TODO: store static files in model, after lens refactor
    "index.html" -> Just $ Right RIndex
    (parseUUIDFileName ".html" -> Just uuid) ->
      Right (RNode uuid) <$ modelLookup uuid model
    _ -> Nothing
  allRoutes (Model m _ _ _) =
    (Right <$> RIndex : (RNode <$> Map.keys m))
      <> [Left "favicon.svg"]

main :: IO ()
main =
  Env.getArgs >>= \case
    "test" : testArgs -> Env.withArgs testArgs $ T.defaultMain spec
    _ -> main'

spec :: T.TestTree
spec =
  T.testGroup
    "Banyan"
    [ ID.spec
    ]

main' :: IO ()
main' =
  Ema.runEma render $ \_act model -> do
    nextId <- liftIO randomId
    let layers = Loc.userLayers (one "content")
        model0 = Model Map.empty AM.empty nextId mempty
    Emanote.emanate
      layers
      [ (FTDot, "*.dot"),
        (FTMd, "*.md")
      ]
      mempty
      model
      model0
      patchModel

data FileType = FTMd | FTDot
  deriving (Eq, Show, Ord)

patchModel :: MonadIO m => FileType -> FilePath -> EmaFS.FileAction (NonEmpty (Loc.Loc, FilePath)) -> m (Model -> Model)
patchModel ftype fp action =
  fmap (maybe id (. modelClearError fp)) . runMaybeT $ do
    case ftype of
      FTDot ->
        case action of
          EmaFS.Delete -> error "not implemented"
          EmaFS.Refresh _ (Loc.locResolve . head -> absPath) -> do
            s <- liftIO $ readFileText absPath
            case G.parseDot s of
              Left e ->
                pure $ modelAddError fp (BadGraph e)
              Right (G.buildGraph -> g) ->
                pure $ modelSetGraph g
      FTMd -> do
        uuid <- hoistMaybe $ parseUUIDFileName ".md" fp
        -- Reset the next id, because a .md file may have been added (or
        -- deleted).  Ideally we should do this only on additions (and possibly
        -- on deletion), and not no modifications.
        liftA2 (.) modelResetNextID $ case action of
          EmaFS.Refresh _ (Loc.locResolve . head -> absPath) -> do
            eRes <- Markdown.parseMarkdown absPath
            case eRes of
              Left err ->
                pure $ modelAddError fp (BadMarkdown err)
              Right v ->
                pure $ modelAdd uuid v
          EmaFS.Delete -> do
            pure $ modelDel uuid

render :: Ema.CLI.Action -> Model -> Either FilePath Route -> Ema.Asset LByteString
render act model = \case
  Left fp ->
    -- This instructs ema to treat this route "as is" (ie. a static file; no generation)
    -- The argument `fp` refers to the absolute path to the static file.
    Ema.AssetStatic fp
  Right r ->
    -- Generate a Html route; hot-reload is enabled.
    Ema.AssetGenerated Ema.Html $ renderHtml act model r

renderHtml :: Ema.CLI.Action -> Model -> Route -> LByteString
renderHtml emaAction model r =
  Tailwind.layout emaAction (H.title "Banyan" >> H.base ! A.href "/" >> H.link ! A.rel "shortcut icon" ! A.href "banyan.svg" ! A.type_ "image/svg") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2" $ do
        H.div ! A.class_ "bg-gray-200 p-2 rounded text-center" $
          H.pre $ H.toHtml $ newFileCli $ _modelNextUUID model
        case r of
          RIndex -> do
            "You are on the index page. "
            forM_ (Map.keys $ _modelNodes model) $ \uuid ->
              H.li $ H.code $ routeElem (Right $ RNode uuid) $ show uuid
          RNode uuid -> do
            "You are on the node page: " <> show uuid
            case modelLookup uuid model of
              Nothing -> "Not found"
              Just (mMeta, pandoc) -> do
                H.div ! A.class_ "border-2 m-4 p-2" $ Markdown.renderPandoc pandoc
                H.div $ H.toHtml $ show @Text mMeta
            routeElem (Right RIndex) "Go to Index"
      H.div ! A.class_ "font-mono text-xs flex items-center justify-center" $ do
        H.pre $ H.toHtml (Shower.shower $ G.toTree $ _modelGraph model)
        when (Map.size (_modelErrors model) > 0) $ do
          H.div ! A.class_ "text-red-200 p-2" $ do
            "Errors:"
            forM_ (Map.toList $ _modelErrors model) $ \(fp, err) ->
              H.div $ H.toHtml $ show @Text fp <> ": " <> show err
  where
    routeElem (r' :: Either FilePath Route) w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')
