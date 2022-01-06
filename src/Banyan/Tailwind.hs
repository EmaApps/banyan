{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Banyan.Tailwind (buildCss) where

import Control.Monad.Logger (MonadLogger, logInfoN)
import qualified Ema.CLI
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist)
import System.Process (readProcess)
import System.Which (staticWhich)
import Text.Printf (printf)

tailwind :: FilePath
tailwind = $(staticWhich "tailwind")

buildCss :: (MonadIO m, MonadLogger m, HasCallStack) => Ema.CLI.Action -> m Text
buildCss action = do
  logInfoN "Running Tailwind compiler to build style.css"
  let extraArgs =
        case action of
          Ema.CLI.Run -> []
          _ -> ["--minify"]
  liftIO (doesFileExist tailwind) >>= \case
    True ->
      timeIt . liftIO $ do
        toText <$> readProcess tailwind (["-i", "input.css"] <> extraArgs) ""
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
