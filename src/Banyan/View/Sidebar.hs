module Banyan.View.Sidebar where

import qualified Banyan.Graph as G
import Banyan.ID (NodeID)
import qualified Banyan.Markdown as Markdown
import Banyan.Model
import Banyan.Route
import Banyan.View.Common
import Control.Lens.Operators ((^.))
import Data.Tree (Tree (Node))
import qualified Data.Tree as Tree
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

renderSidebar :: Model -> Route -> H.Html
renderSidebar model hereR = do
  let forest = G.toTree $ model ^. modelGraph
  H.div ! A.class_ "border-r-2 md:h-full pr-2 mr-2" $ do
    routeElemUnlessHere model hereR (Right RIndex) "Index"
    H.div ! A.class_ "font-mono" $ do
      forM_ forest $ \node ->
        renderNode model hereR True node

renderNode :: Model -> Route -> Bool -> Tree G.NodeID -> H.Html
renderNode model hereR emptyAllowed (Node nid children) = do
  when (emptyAllowed || not (null children)) $ do
    H.div ! A.class_ "pl-2" $ do
      nodeLink model hereR nid
      H.div $ do
        let childNodes = sortOn (fmap fst . flip modelLookup model . Tree.rootLabel) children
        forM_ childNodes $ \node ->
          renderNode model hereR False node

nodeLink :: Model -> Route -> NodeID -> H.Html
nodeLink model hereR nid =
  case modelLookup nid model of
    Nothing -> "error: no such node: " <> show nid
    Just (mMeta, _pandoc) -> do
      let nodeTitle = fromMaybe (show nid) $ Markdown.title =<< mMeta
      routeElemUnlessHere model hereR (nodeRoute nid) $ do
        H.toHtml nodeTitle

nodeRoute :: NodeID -> Either FilePath Route
nodeRoute = Right . RNode
