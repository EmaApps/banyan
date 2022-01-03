-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Main where

import qualified Banyan.ID as ID
import Banyan.Model (Model)
import qualified Banyan.Model as Model
import qualified Banyan.Model.Patch as Patch
import Banyan.Route (Route)
import qualified Banyan.View as View
import Control.Lens.Operators ((^.))
import qualified Data.Map.Strict as Map
import qualified Ema
import qualified Ema.CLI
import qualified Emanote
import qualified Emanote.Source.Loc as Loc
import qualified System.Environment as Env
import qualified Test.Tasty as T

main :: IO ()
main =
  Env.getArgs >>= \case
    "test" : testArgs -> Env.withArgs testArgs $ T.defaultMain spec
    _ -> exe

spec :: T.TestTree
spec =
  T.testGroup
    "Banyan"
    [ ID.spec
    ]

contentDir :: FilePath
contentDir = "content"

exe :: IO ()
exe =
  Ema.runEma render $ \_act model -> do
    let layers = Loc.userLayers (one contentDir)
    model0 <- Model.emptyModel contentDir
    Emanote.emanate
      layers
      Patch.watching
      Patch.ignoring
      model
      model0
      (\a b -> Patch.patchModel a b . fmap (Loc.locResolve . head))

render :: Ema.CLI.Action -> Model -> Either FilePath Route -> Ema.Asset LByteString
render act model = \case
  Left fp ->
    -- This instructs ema to treat this route "as is" (ie. a static file; no generation)
    -- The argument `fp` refers to the absolute path to the static file.
    case Map.lookup fp (model ^. Model.modelFiles) of
      Nothing -> error "missing static file"
      Just (_, absPath) -> Ema.AssetStatic absPath
  Right r ->
    -- Generate a Html route; hot-reload is enabled.
    Ema.AssetGenerated Ema.Html $ View.renderHtml act model r
