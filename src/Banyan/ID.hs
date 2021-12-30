module Banyan.ID where

import Data.NanoID (NanoID (NanoID))
import qualified Data.NanoID as NanoID
import qualified Data.Text as T
import Hedgehog (property, (===))
import System.FilePath (splitExtension)
import System.Random.MWC (createSystemRandom)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | Like UUID but much shorter. Collisions are still (reasonably) rare.
type NodeID = NanoID

randomId :: MonadIO m => m NanoID
randomId = do
  g <- liftIO createSystemRandom
  rid <- liftIO $ NanoID.customNanoID NanoID.alphanumeric 13 g
  if any ((`T.isPrefixOf` show rid) . show) [0 :: Int .. 9]
    then randomId -- Avoid IDs beginning with numerals (have to be quoted in DOT)
    else pure rid

parseIDFileName :: String -> FilePath -> Maybe NodeID
parseIDFileName ext fp = do
  let (toText -> baseName, fpExt) = splitExtension fp
  guard $ ext == fpExt
  guard $ not $ "/" `T.isInfixOf` baseName
  pure $ NanoID $ encodeUtf8 baseName -- FIXME: not the correct way

spec :: TestTree
spec =
  testGroup
    "NodeID"
    [ testProperty "no numerals as first char" . property $ do
        rid <- liftIO randomId
        any ((`T.isPrefixOf` show rid) . show) [0 :: Int .. 9]
          === False
    ]
