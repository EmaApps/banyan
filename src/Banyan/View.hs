{-# LANGUAGE TypeApplications #-}

module Banyan.View where

import qualified Banyan.Graph as G
import qualified Banyan.Markdown as Markdown
import Banyan.Model
import Banyan.Route
import qualified Banyan.VSCode as VSCode
import qualified Banyan.View.Sidebar as Sidebar
import Control.Lens.Operators ((^.))
import qualified Data.Map.Strict as Map
import qualified Ema.CLI
import qualified Ema.Helper.Tailwind as Tailwind
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

renderHtml :: Ema.CLI.Action -> Model -> Route -> LByteString
renderHtml emaAction model r =
  Tailwind.layout emaAction (H.title "Banyan" >> H.base ! A.href "/" >> H.link ! A.rel "shortcut icon" ! A.href "banyan.svg" ! A.type_ "image/svg") $
    renderLayout
      model
      (VSCode.renderVSCodeAction $ VSCode.NewNode (model ^. modelNextID) routeNid)
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
                VSCode.renderVSCodeAction $ VSCode.EditNode nid
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
                        VSCode.renderVSCodeAction $ VSCode.EditNode node
              H.div ! A.class_ "font-mono text-xs text-gray-400 mt-8" $ H.toHtml $ show @Text mMeta
  where
    routeNid = case r of
      RIndex -> Nothing
      RNode nid -> Just nid

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
