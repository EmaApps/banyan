{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Banyan.View where

import qualified Banyan.Graph as G
import Banyan.ID (NodeID)
import qualified Banyan.Markdown as Markdown
import Banyan.Model
import Banyan.Route
import qualified Banyan.View.Sidebar as Sidebar
import Control.Lens.Operators ((^.))
import qualified Data.Map.Strict as Map
import qualified Ema.CLI
import qualified Ema.Helper.Tailwind as Tailwind
import NeatInterpolation (text)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

renderHtml :: Ema.CLI.Action -> Model -> Route -> LByteString
renderHtml emaAction model r =
  Tailwind.layout emaAction (H.title "Banyan" >> H.base ! A.href "/" >> H.link ! A.rel "shortcut icon" ! A.href "banyan.svg" ! A.type_ "image/svg") $
    renderLayout
      model
      (topbar model r)
      (Sidebar.renderSidebar model r)
      $ case r of
        RIndex -> do
          "Check the sidebar"
        RNode nid -> do
          case modelLookup nid model of
            Nothing -> "Not found"
            Just (mMeta, pandoc) -> do
              -- TODO: this should be a breadcrumb
              let nodeTitle = fromMaybe (show nid) $ Markdown.title =<< mMeta
              H.header ! A.class_ "text-2xl font-bold" $ H.toHtml nodeTitle
              H.div ! A.class_ "my-2" $ do
                Markdown.renderPandoc pandoc
                editLink emaAction model nid
              let childNodes' = G.getDescendents nid $ model ^. modelGraph
                  -- TODO: DRY with sidebar
                  childNodes = sortOn (fmap fst . flip modelLookup model) childNodes'
              H.div ! A.class_ "my-2 " $ do
                forM_ childNodes $ \node ->
                  H.div ! A.class_ "border-2 p-2 my-2 bg-gray-50" $ do
                    case modelLookup node model of
                      Nothing -> do
                        "missing!"
                        show node
                      Just (childMMeta, childPandoc) -> do
                        H.div ! A.class_ "text-sm text-gray-500" $ do
                          let childTitle = fromMaybe (show node) $ Markdown.title =<< childMMeta
                          H.span ! A.class_ "font-mono" $ H.toHtml childTitle
                          " / "
                          let nodeDate = maybe "No date" show $ Markdown.date =<< childMMeta
                          H.toHtml @Text nodeDate
                        Markdown.renderPandoc childPandoc
                        editLink emaAction model node
              H.div ! A.class_ "font-mono text-xs text-gray-400 mt-8" $ H.toHtml $ show @Text mMeta

editLink :: Ema.CLI.Action -> Model -> NodeID -> H.Html
editLink Ema.CLI.Run model nid =
  H.a
    ! A.class_ "bg-blue-400 text-sm text-white p-1 my-2"
    ! A.href (H.toValue $ modelNodeEditUrlVSCode nid model)
    $ "Edit in VS Code"
editLink _ _ _ =
  mempty

topbar :: Model -> Route -> H.Html
topbar model r =
  H.pre ! A.class_ "overflow-auto py-2" $ do
    H.toHtml $
      newFileCli (_modelNextID model) $ case r of
        RIndex -> Nothing
        RNode nid -> Just nid
  where
    -- CLI for creating a new node, optionally under the given parent.
    newFileCli :: NodeID -> Maybe NodeID -> Text
    newFileCli (show -> nid) mPid =
      -- If there is parent node, inject `parent: ...` property into YAML header.
      let pidS :: Text = maybe "\\n" (\pid -> "\\nparent: " <> show pid) mPid
       in [text|
        echo "---\ndate: $(date -u +'%Y-%m-%dT%H:%M:%S')${pidS}\n---\n\n" >${nid}.md; cat >>${nid}.md
      |]

renderLayout :: Model -> H.Html -> H.Html -> H.Html -> H.Html
renderLayout model top sidebar main = do
  H.div ! A.class_ "container mx-auto" $ do
    H.div ! A.class_ "flex flex-col mt-2" $ do
      H.div ! A.id "top" ! A.class_ "bg-pink-100 border-2 border-pink-400 p-2 rounded text-center" $ top
      H.div ! A.class_ "flex flex-row pt-2" $ do
        H.div ! A.id "sidebar" $ sidebar
        H.div ! A.id "main" $ main
    renderFooter model

renderFooter :: Model -> H.Html
renderFooter model = do
  H.div ! A.class_ "font-mono text-xs flex items-center justify-center border-t-2 mt-4" $ do
    when (Map.size (_modelErrors model) > 0) $ do
      H.div ! A.class_ "text-red-200 p-2" $ do
        "Errors:"
        forM_ (Map.toList $ _modelErrors model) $ \(fp, err) ->
          H.div $ H.toHtml $ show @Text fp <> ": " <> show err
