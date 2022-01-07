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
  )
where

import Control.Monad.Logger (MonadLogger, logInfoN)
import Data.Aeson
import Deriving.Aeson
import NeatInterpolation
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (hClose, hPrint)
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

runTailwindJIT :: (MonadUnliftIO m, MonadLogger m) => TailwindConfig -> FilePath -> FilePath -> m ()
runTailwindJIT config input outputDir = do
  withConfig config $ \configFile ->
    callTailwind ["-c", configFile, "-i", input, "-o", outputDir </> tailwindCssFilename, "-w"]
  error "Tailwind exited unexpectedly!"

runTailwindProduction :: (MonadUnliftIO m, MonadLogger m) => TailwindConfig -> FilePath -> FilePath -> m ()
runTailwindProduction config input outputDir =
  withConfig config $ \configFile ->
    callTailwind ["-c", configFile, "-i", input, "-o", outputDir </> tailwindCssFilename, "--minify"]

withConfig :: MonadUnliftIO m => TailwindConfig -> (FilePath -> m a) -> m a
withConfig config f = do
  withSystemTempFile "tailwind.config.js" $ \fp h -> do
    liftIO $ do
      print fp
      print config
      hPrint h config >> hClose h
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
