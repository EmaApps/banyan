{-# LANGUAGE TypeApplications #-}

module Banyan.View where

import qualified Banyan.Graph as G
import qualified Banyan.Markdown as Markdown
import Banyan.Model
import Banyan.Route (Route (..))
import qualified Banyan.VSCode as VSCode
import Banyan.View.Common (routeElem)
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
  Tailwind.layoutWith "en" "UTF-8" mempty (renderHead model) $
    renderLayout
      model
      (H.header ! A.class_ "flex items-center justify-center border-b-4 border-green-500" $ H.a ! A.href "https://github.com/srid/banyan" $ H.img ! A.class_ "w-8" ! A.src "/banyan.svg")
      (Sidebar.renderSidebar model r)
      $ case r of
        RIndex -> do
          let allNodes' = Map.keys $ model ^. modelNodes
              -- TODO: DRY with sidebar
              allNodes = sortOn (fmap fst . flip modelLookup model) allNodes'
          H.header "Timeline"
          -- TODO: breadcrumb
          VSCode.renderVSCodeAction emaAction $ mkVSCodeAction model $ VSCode.NewNode (model ^. modelNextID) routeNid
          renderListing emaAction model allNodes
        RNode nid -> do
          case modelLookup nid model of
            Nothing -> "Not found"
            Just (mMeta, pandoc) -> do
              -- TODO: this should be a breadcrumb
              let nodeTitle = fromMaybe (show nid) $ Markdown.title =<< mMeta
              H.header ! A.class_ "text-2xl font-bold" $ do
                H.toHtml nodeTitle
                VSCode.renderVSCodeAction emaAction $ mkVSCodeAction model $ VSCode.EditNode nid
              H.div ! A.class_ "my-2" $ do
                Markdown.renderPandoc pandoc
              let childNodes' = G.getDescendents nid $ model ^. modelGraph
                  -- TODO: DRY with sidebar
                  childNodes = sortOn (fmap fst . flip modelLookup model) childNodes'
              H.div ! A.class_ "my-2 " $ do
                VSCode.renderVSCodeAction emaAction $ mkVSCodeAction model $ VSCode.NewNode (model ^. modelNextID) routeNid
                renderListing emaAction model childNodes
              H.div ! A.class_ "font-mono text-xs text-gray-400 mt-8" $ H.toHtml $ show @Text mMeta
  where
    routeNid = case r of
      RIndex -> Nothing
      RNode nid -> Just nid

tailwindCssFilename :: String
tailwindCssFilename = "tailwind.css"

renderHead :: Model -> H.Html
renderHead model = do
  H.title "Banyan"
  H.base ! A.href "/"
  H.link ! A.rel "shortcut icon" ! A.href "banyan.svg" ! A.type_ "image/svg"
  let cssUrl = fromMaybe (error "style.css missing") $ modelFileUrl tailwindCssFilename model
  H.link ! A.rel "stylesheet" ! A.href (H.toValue cssUrl)

renderListing :: Ema.CLI.Action -> Model -> [G.NodeID] -> H.Html
renderListing emaAction model nodes = do
  forM_ nodes $ \node ->
    H.div ! A.class_ "rounded shadow p-2 my-2 bg-white max-w-prose" $ do
      case modelLookup node model of
        Nothing -> do
          "missing!"
          show node
        Just (childMMeta, childPandoc) -> do
          H.div ! A.class_ "text-sm text-gray-500" $ do
            let childTitle = fromMaybe (show node) $ Markdown.title =<< childMMeta
                grandChildren = G.getDescendents node $ model ^. modelGraph
            H.div ! A.class_ "font-mono text-xs flex flex-row items-center justify-between bg-gray-50 hover:bg-gray-100 p-1" $ do
              H.div $
                if null grandChildren
                  then H.toHtml childTitle
                  else do
                    routeElem model (Sidebar.nodeRoute node) $ do
                      H.toHtml childTitle
                    " (" <> show (length grandChildren) <> ")"
              let nodeDate = maybe "" show $ Markdown.date =<< childMMeta
              H.span ! A.class_ "opacity-60" $ H.toHtml @Text nodeDate
              H.div ! A.class_ "space-x-2" $ do
                VSCode.renderVSCodeAction emaAction $ mkVSCodeAction model $ VSCode.EditNode node
                VSCode.renderVSCodeAction emaAction $ mkVSCodeAction model $ VSCode.NewNode (model ^. modelNextID) (Just node)
          Markdown.renderPandoc childPandoc

mkVSCodeAction :: Model -> VSCode.Action -> VSCode.VSCodeAction
mkVSCodeAction model =
  VSCode.VSCodeAction (model ^. modelBaseDir)

renderLayout :: Model -> H.Html -> H.Html -> H.Html -> H.Html
renderLayout model top sidebar main = do
  H.body ! A.class_ "overflow-y-scroll bg-gray-200" $ do
    H.div ! A.class_ "container mx-auto max-w-screen-md" $ do
      H.div ! A.class_ "flex flex-col mt-2" $ do
        H.div ! A.id "top" ! A.class_ "border-2 p-1 rounded text-center print:hidden" $ top
        H.div ! A.class_ "flex flex-row pt-2" $ do
          H.div ! A.id "sidebar" ! A.class_ "print:hidden" $ sidebar
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
