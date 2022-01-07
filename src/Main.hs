-- | A very simple site with two routes, and HTML rendered using Blaze DSL
module Main where

import qualified Banyan.ID as ID
import Banyan.Model (Model)
import qualified Banyan.Model as Model
import qualified Banyan.Model.Patch as Patch
import Banyan.Route
import Banyan.Tailwind (runTailwindJIT, runTailwindProduction)
import qualified Banyan.View as View
import Control.Lens.Operators ((^.))
import Control.Monad.Logger (runStdoutLoggingT)
import qualified Data.Map.Strict as Map
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
  dataDir <- liftIO Paths_banyan.getDataDir
  let defaultLayer = Loc.defaultLayer dataDir
      layers = one defaultLayer <> Loc.userLayers (one contentDir)
      inputCssPath = dataDir </> "input.css"
      tailwindConfigPath = dataDir </> "tailwind.config.js"
  model0 <- Model.emptyModel contentDir
  let runEma = Ema.runEma render $ \act model -> do
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
              (runTailwindJIT tailwindConfigPath inputCssPath $ model0 ^. Model.modelBaseDir)
              runEmanate
          Ema.CLI.Generate _ -> do
            runTailwindProduction tailwindConfigPath inputCssPath $ model0 ^. Model.modelBaseDir
            runEmanate
  -- HACK: this really should be done properly. ema's generate killing main thread is bad.
  runEma >>= \case
    Ema.CLI.Cli (Ema.CLI.Generate _) -> do
      -- We must generate tailwind css a second time, *after*, .html are generated in first run.
      -- And then generate the HTML itself, to update the url hash.
      putStrLn "ema gen: 2nd pass"
      void runEma
    _ -> pure ()

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
