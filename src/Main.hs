-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Main where

import qualified Banyan.ID as ID
import Banyan.Model (Model)
import qualified Banyan.Model as Model
import qualified Banyan.Model.Patch as Patch
import Banyan.Route
import Banyan.Tailwind (runTailwindJIT, runTailwindProduction)
import qualified Banyan.Tailwind as Tailwind
import qualified Banyan.View as View
import Control.Lens.Operators ((^.))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Ema
import qualified Ema.CLI
import qualified Emanote
import qualified Emanote.Source.Loc as Loc
import qualified Paths_banyan
import qualified System.Environment as Env
import System.FilePath ((</>))
import qualified Test.Tasty as T
import UnliftIO.Async (concurrently_)

main :: IO ()
main = do
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
exe = do
  model0 <- Model.emptyModel contentDir
  ema model0

ema :: Model -> IO ()
ema model0 = do
  dataDir <- liftIO Paths_banyan.getDataDir
  let defaultLayer = Loc.defaultLayer $ dataDir </> "default"
      layers = one defaultLayer <> Loc.userLayers (one contentDir)
  let tc = Tailwind.TailwindConfig
  -- HACK: this really should be done properly. ema's generate killing main thread is bad.
  ema' layers (tc [dataDir </> "src/**/*.hs"]) model0 >>= \case
    Just (Ema.CLI.Generate _) -> do
      -- We must generate tailwind css a second time, *after*, .html are generated in first run.
      -- And then generate the HTML itself, to update the url hash.
      putStrLn "ema gen: 2nd pass"
      -- FIXME: don't hardcode
      -- not necessary, but will be - if we dynamically build css classes.
      -- void $ ema' layers (tc ["./content/.ci/*.html"]) inputCssPath model0
      pure ()
    _ -> pure ()

ema' :: Set (Loc.Loc, FilePath) -> Tailwind.TailwindConfig -> Model -> IO (Maybe Ema.CLI.Action)
ema' layers tailwindConfig model0 = do
  Ema.runEma render $ \act model -> do
    let runEmanate =
          Emanote.emanate
            layers
            (Patch.watching act)
            Patch.ignoring
            model
            model0
            (\a b -> Patch.patchModel a b . fmap (Loc.locResolve . head))
    case act of
      Ema.CLI.Run ->
        concurrently_
          (runTailwindJIT tailwindConfig Tailwind.defaultCss $ model0 ^. Model.modelBaseDir)
          runEmanate
      Ema.CLI.Generate _ -> do
        runTailwindProduction tailwindConfig Tailwind.defaultCss $ model0 ^. Model.modelBaseDir
        runEmanate
    pure act

render :: Ema.CLI.Action -> Model -> SiteRoute -> Ema.Asset LByteString
render act model = \case
  SRStatic fp ->
    -- This instructs ema to treat this route "as is" (ie. a static file; no generation)
    -- The argument `fp` refers to the absolute path to the static file.
    case Map.lookup fp (model ^. Model.modelFiles) of
      Nothing -> error "missing static file"
      Just (_, absPath) -> Ema.AssetStatic absPath
  SRHtml r ->
    -- Generate a Html route; hot-reload is enabled.
    Ema.AssetGenerated Ema.Html $ View.renderHtml act model r
