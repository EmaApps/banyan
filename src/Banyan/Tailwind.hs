{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Banyan.Tailwind
  ( runTailwindJIT,
    runTailwindProduction,
    tailwindCssFilename,
  )
where

import Control.Monad.Logger (MonadLogger, logInfoN)
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Which (staticWhich)
import Text.Printf (printf)
import UnliftIO.Process (callProcess)

tailwind :: FilePath
tailwind = $(staticWhich "tailwind")

tailwindCssFilename :: String
tailwindCssFilename = "tailwind-generated.css"

runTailwindJIT :: (MonadIO m, MonadLogger m) => FilePath -> FilePath -> m ()
runTailwindJIT input outputDir = do
  callTailwind ["-i", input, "-o", outputDir </> tailwindCssFilename, "-w"]
  error "Tailwind exited unexpectedly!"

runTailwindProduction :: (MonadIO m, MonadLogger m) => FilePath -> FilePath -> m ()
runTailwindProduction input outputDir =
  callTailwind ["-i", input, "-o", outputDir </> tailwindCssFilename, "--minify"]

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
