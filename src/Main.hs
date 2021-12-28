{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Main where

import qualified Banyan.Graph as G
import Control.Exception
import qualified Data.Map.Strict as Map
import Data.NanoID (NanoID (NanoID))
import qualified Data.NanoID as NanoID
import qualified Data.Text as T
import Ema (Ema (..))
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as EmaFS
import qualified Ema.Helper.Tailwind as Tailwind
import qualified Emanote
import qualified Emanote.Source.Loc as Loc
import NeatInterpolation (text)
import qualified Shower
import System.FilePath (splitExtension)
import System.Random.MWC (createSystemRandom)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

type NodeID = NanoID

data Route
  = RIndex
  | RNode NodeID
  deriving (Show)

data Model = Model
  { _modelNodes :: Map NodeID Text,
    _modelGraph :: G.Dot,
    _modelNextUUID :: NodeID
  }
  deriving (Show)

modelDel :: NodeID -> Model -> Model
modelDel fp (Model m g n) = Model (Map.delete fp m) g n

modelAdd :: NodeID -> Text -> Model -> Model
modelAdd fp s (Model m g n) = Model (Map.insert fp s m) g n

modelLookup :: NodeID -> Model -> Maybe Text
modelLookup k (Model m _ _) = Map.lookup k m

modelResetNextUUID :: (MonadIO m, HasCallStack) => m (Model -> Model)
modelResetNextUUID = do
  rid <- liftIO randomId
  pure $ \(Model m g _) ->
    if Map.member rid m
      then error $ "NanoID collision: " <> show rid
      else Model m g rid

modelSetGraph :: G.Dot -> Model -> Model
modelSetGraph g (Model m _ n) = Model m g n

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

randomId :: IO NanoID
randomId = do
  createSystemRandom >>= NanoID.customNanoID NanoID.alphanumeric 13

parseUUIDFileName :: String -> FilePath -> Maybe NodeID
parseUUIDFileName ext fp = do
  let (toText -> baseName, fpExt) = splitExtension fp
  guard $ ext == fpExt
  guard $ not $ "/" `T.isInfixOf` baseName
  pure $ NanoID $ encodeUtf8 baseName -- FIXME: not the correct way

data FileType = FTMd | FTDot
  deriving (Eq, Show, Ord)

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
      (Model Map.empty (G.Digraph ".." mempty) nextId)
      patchModel

data BadGraph = BadGraph Text
  deriving (Show, Exception)

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
                -- TODO: this doesn't show error. FIXME>
                throw $ BadGraph e
              Right g ->
                pure $ modelSetGraph g
      FTMd -> do
        uuid <- hoistMaybe $ parseUUIDFileName ".md" fp
        liftA2 (.) modelResetNextUUID $ case action of
          EmaFS.Refresh _ (Loc.locResolve . head -> absPath) -> do
            s <- liftIO $ readFileText absPath
            pure $ modelAdd uuid s
          EmaFS.Delete -> do
            pure $ modelDel uuid

render :: Ema.CLI.Action -> Model -> Route -> LByteString
render emaAction model r =
  Tailwind.layout emaAction (H.title "Emanote-like" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2 text-center" $ do
        H.div ! A.class_ "bg-gray-200 p-2 rounded" $ do
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
              Just s -> H.pre ! A.class_ "border-2 m-4 p-2 overflow-auto" $ H.toHtml s
            routeElem RIndex "Go to Index"
      H.div ! A.class_ "font-mono text-xs flex items-center justify-center" $ do
        H.pre $ H.toHtml (Shower.shower $ _modelGraph model)
  where
    routeElem r' w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')
