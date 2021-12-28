module Banyan.ID where

import Data.NanoID (NanoID (NanoID))
import qualified Data.NanoID as NanoID
import qualified Data.Text as T
import System.FilePath (splitExtension)
import System.Random.MWC (createSystemRandom)

-- | Like UUID but much shorter. Collisions are still (reasonably) rare.
type NodeID = NanoID

randomId :: IO NanoID
randomId = do
  g <- createSystemRandom
  rid <- NanoID.customNanoID NanoID.alphanumeric 13 g
  if any ((`T.isPrefixOf` show rid) . show) [0 :: Int .. 9]
    then randomId -- Avoid IDs beginning with numerals (have to be quoted in DOT)
    else pure rid

parseUUIDFileName :: String -> FilePath -> Maybe NodeID
parseUUIDFileName ext fp = do
  let (toText -> baseName, fpExt) = splitExtension fp
  guard $ ext == fpExt
  guard $ not $ "/" `T.isInfixOf` baseName
  pure $ NanoID $ encodeUtf8 baseName -- FIXME: not the correct way
