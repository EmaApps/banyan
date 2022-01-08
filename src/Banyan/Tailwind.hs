{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Banyan.Tailwind
  ( runTailwindJIT,
    runTailwindProduction,
    tailwindCssFilename,
    TailwindConfig (..),
    defaultCss,
  )
where

import Control.Monad.Logger (MonadLogger, logInfoN)
import Data.Aeson (encode)
import Data.ByteString (hPut)
import Deriving.Aeson
import NeatInterpolation (text)
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (hClose)
import System.Which (staticWhich)
import Text.Printf (printf)
import qualified Text.Show
import UnliftIO (MonadUnliftIO, finally)
import UnliftIO.Directory (removeFile)
import UnliftIO.Process (callProcess)
import UnliftIO.Temporary (withSystemTempFile)

-- | Haskell version of tailwind.config.js
--
-- Only the subset we care to define, as some fields (eg: plugins) are defined
-- with arbitrary JS code.
data TailwindConfig = TailwindConfig
  { -- | List of source patterns that reference CSS classes
    tailwindConfigContent :: [FilePath]
  }
  deriving (Generic)
  deriving
    (ToJSON)
    via CustomJSON
          '[ FieldLabelModifier
               '[StripPrefix "tailwindConfig", CamelToSnake]
           ]
          TailwindConfig

newtype Css = Css {unCss :: Text}

defaultCss :: Css
defaultCss =
  Css
    [text|
    @tailwind base;
    @tailwind components;
    @tailwind utilities;
    |]

instance Text.Show.Show TailwindConfig where
  show (decodeUtf8 . encode -> config) =
    -- Use `Object.assign` to merge JSON (produced in Haskell) with the rest of
    -- config (defined by raw JS; that cannot be JSON encoded)
    toString
      [text|
      module.exports =
        Object.assign(
          JSON.parse('${config}'),
          {
            theme: {
              extend: {},
            },
            plugins: [
              require('@tailwindcss/typography'),
              require('@tailwindcss/forms'),
              require('@tailwindcss/line-clamp'),
              require('@tailwindcss/aspect-ratio')
            ],
          })
      |]

tailwind :: FilePath
tailwind = $(staticWhich "tailwind")

tailwindCssFilename :: String
tailwindCssFilename = "tailwind-generated.css"

runTailwindJIT :: (MonadUnliftIO m, MonadLogger m) => TailwindConfig -> Css -> FilePath -> m ()
runTailwindJIT config input outputDir = do
  withTmpFile (show config) $ \configFile ->
    withTmpFile (unCss input) $ \inputFile ->
      callTailwind ["-c", configFile, "-i", inputFile, "-o", outputDir </> tailwindCssFilename, "-w"]
  error "Tailwind exited unexpectedly!"

runTailwindProduction :: (MonadUnliftIO m, MonadLogger m) => TailwindConfig -> Css -> FilePath -> m ()
runTailwindProduction config input outputDir =
  withTmpFile (show config) $ \configFile ->
    withTmpFile (unCss input) $ \inputFile ->
      callTailwind ["-c", configFile, "-i", inputFile, "-o", outputDir </> tailwindCssFilename, "--minify"]

withTmpFile :: MonadUnliftIO m => Text -> (FilePath -> m a) -> m a
withTmpFile s f = do
  withSystemTempFile "tailwind.config.js" $ \fp h -> do
    liftIO $ do
      print fp
      print s
      hPut h (encodeUtf8 s) >> hClose h
    f fp
      `finally` removeFile fp

callTailwind :: (MonadIO m, MonadLogger m) => [String] -> m ()
callTailwind args = do
  logInfoN $ "Running Tailwind compiler with args: " <> show args
  liftIO (doesFileExist tailwind) >>= \case
    True ->
      timeIt $ do
        callProcess tailwind args
    False ->
      error $ "Tailwind compiler not found at " <> toText tailwind

timeIt :: MonadIO m => m b -> m b
timeIt m = do
  t0 <- liftIO getCPUTime
  !x <- m
  t1 <- liftIO getCPUTime
  let diff :: Double = fromIntegral (t1 - t0) / (10 ^ (9 :: Integer))
  liftIO $ printf "Process duration: %0.3f ms\n" diff
  pure $! x
