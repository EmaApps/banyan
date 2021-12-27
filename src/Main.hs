{-# LANGUAGE TypeApplications #-}

-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Main where

import qualified Data.Map.Strict as Map
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import Ema (Ema (..))
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as EmaFS
import qualified Ema.Helper.Tailwind as Tailwind
import qualified Emanote
import qualified Emanote.Source.Loc as Loc
import System.FilePath (splitExtension)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Route
  = RIndex
  | RNode UUID
  deriving (Show)

data Model = Model
  { _modelNodes :: Map UUID Text,
    _modelNextUUID :: UUID
  }
  deriving (Show)

modelDel :: UUID -> Model -> Model
modelDel fp (Model m n) = Model (Map.delete fp m) n

modelAdd :: UUID -> Text -> Model -> Model
modelAdd fp s (Model m n) = Model (Map.insert fp s m) n

modelLookup :: UUID -> Model -> Maybe Text
modelLookup k (Model m _) = Map.lookup k m

modelResetNextUUID :: UUID -> Model -> Model
modelResetNextUUID uuid (Model m _) = do
  Model m uuid

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
  allRoutes (Model m _) =
    RIndex : (RNode <$> Map.keys m)

parseUUIDFileName :: String -> FilePath -> Maybe UUID
parseUUIDFileName ext fp = do
  let (baseName, fpExt) = splitExtension fp
  guard $ ext == fpExt
  UUID.fromString baseName

main :: IO ()
main = do
  Ema.runEma (\act m -> Ema.AssetGenerated Ema.Html . render act m) $ \_act model -> do
    let layers = Loc.userLayers (one "content")
    nextUUID <- liftIO UUID4.nextRandom
    Emanote.emanate
      layers
      (one ((), "*.md"))
      mempty
      model
      (Model mempty nextUUID)
      (const patchModel)

patchModel :: (Monad m, MonadIO m) => FilePath -> EmaFS.FileAction (NonEmpty (Loc.Loc, FilePath)) -> m (Model -> Model)
patchModel fp action =
  fmap (fromMaybe id) . runMaybeT $ do
    uuid <- hoistMaybe $ parseUUIDFileName ".md" fp
    nextUUID <- liftIO UUID4.nextRandom
    (modelResetNextUUID nextUUID .) <$> case action of
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
        case r of
          RIndex -> do
            "You are on the index page. "
            forM_ (Map.keys $ _modelNodes model) $ \uuid ->
              H.li $ routeElem (RNode uuid) $ show uuid
            H.pre $ H.toHtml (show @Text model)
          RNode uuid -> do
            "You are on the node page: " <> show uuid
            case modelLookup uuid model of
              Nothing -> "Not found"
              Just s -> H.pre ! A.class_ "border-2 m-4 p-2" $ H.toHtml s
            routeElem RIndex "Go to Index"
  where
    routeElem r' w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')
