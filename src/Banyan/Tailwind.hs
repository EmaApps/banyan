{-# LANGUAGE TemplateHaskell #-}

module Banyan.Tailwind (buildCss) where

import Control.Monad.Logger (MonadLogger, logInfoN)
import qualified Ema.CLI
import System.Process (readProcess)
import System.Which (staticWhich)

tailwind :: FilePath
tailwind = $(staticWhich "tailwind")

buildCss :: (MonadIO m, MonadLogger m) => Ema.CLI.Action -> m Text
buildCss action = do
  logInfoN "Running Tailwind compiler to build style.css"
  let extraArgs =
        case action of
          Ema.CLI.Run -> []
          _ -> ["--minify"]
  -- TODO: Time this, since we are not using JIT (-w) mode.
  liftIO $
    toText
      <$> readProcess
        tailwind
        -- FIXME: doin't want to write back to source, because that fails during gh-pages action (Docker!)
        (["-i", "input.css"] <> extraArgs)
        ""
