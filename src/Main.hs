{-# LANGUAGE TypeApplications #-}

-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Main where

import qualified Data.Map.Strict as Map
import Ema (Ema (..))
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.FileSystem as EmaFS
import qualified Ema.Helper.Tailwind as Tailwind
import qualified Emanote
import qualified Emanote.Source.Loc as Loc
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data Route
  = Index
  | About
  deriving (Show, Enum, Bounded)

newtype Model = Model {unModel :: Map FilePath Text}
  deriving (Show)

modelDel :: FilePath -> Model -> Model
modelDel fp (Model m) = Model $ Map.delete fp m

modelAdd :: FilePath -> Text -> Model -> Model
modelAdd fp s (Model m) = Model $ Map.insert fp s m

instance Ema Model Route where
  encodeRoute _model =
    \case
      Index -> "index.html"
      About -> "about.html"
  decodeRoute _model = \case
    "index.html" -> Just Index
    "about.html" -> Just About
    _ -> Nothing

main :: IO ()
main = do
  Ema.runEma (\act m -> Ema.AssetGenerated Ema.Html . render act m) $ \_act model -> do
    let layers = Loc.userLayers (one "content")
    Emanote.emanate
      layers
      (one ((), "*.md"))
      mempty
      model
      (Model mempty)
      (const patchModel)

patchModel :: (Monad m, MonadIO m) => FilePath -> EmaFS.FileAction (NonEmpty (Loc.Loc, FilePath)) -> m (Model -> Model)
patchModel fp = \case
  EmaFS.Refresh _ (Loc.locResolve . head -> absPath) -> do
    s <- liftIO $ readFileText absPath
    pure $ modelAdd fp s
  EmaFS.Delete -> do
    pure $ modelDel fp

render :: Ema.CLI.Action -> Model -> Route -> LByteString
render emaAction model r =
  Tailwind.layout emaAction (H.title "Emanote-like" >> H.base ! A.href "/") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2 text-center" $ do
        case r of
          Index -> do
            H.pre $ H.toHtml (show @Text $ unModel model)
            "You are on the index page. "
            routeElem About "Go to About"
          About -> do
            "You are on the about page. "
            routeElem Index "Go to Index"
  where
    routeElem r' w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')
