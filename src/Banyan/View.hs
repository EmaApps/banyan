{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Banyan.View where

import Banyan.ID (NodeID)
import qualified Banyan.Markdown as Markdown
import Banyan.Model
import Banyan.Route
import Banyan.View.Common
import qualified Banyan.View.Sidebar as Sidebar
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
      (topbar model)
      (Sidebar.renderSidebar model r)
      $ case r of
        RIndex -> do
          "Every node: "
          forM_ (Map.keys $ _modelNodes model) $ \uuid ->
            H.li $ H.code $ routeElem model (Right $ RNode uuid) $ show uuid
        RNode nid -> do
          case modelLookup nid model of
            Nothing -> "Not found"
            Just (mMeta, pandoc) -> do
              -- TODO: this should be a breadcrumb
              H.header ! A.class_ "text-2xl font-bold" $ show nid
              H.div ! A.class_ "my-2" $ Markdown.renderPandoc pandoc
              H.div ! A.class_ "font-mono text-xs text-gray-400 mt-8" $ H.toHtml $ show @Text mMeta

topbar :: Model -> H.Html
topbar model =
  H.pre ! A.class_ "overflow-auto py-2" $ do
    H.toHtml $ newFileCli $ _modelNextID model
  where
    newFileCli :: NodeID -> Text
    newFileCli (show -> nid) =
      [text|
        echo "---\ndate: $(date -u +'%Y-%m-%dT%H:%M:%S')\n---\n\n" >${nid}.md; cat >>${nid}.md
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
