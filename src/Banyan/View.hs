{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Banyan.View where

import qualified Banyan.Graph as G
import Banyan.ID (NodeID)
import qualified Banyan.Markdown as Markdown
import Banyan.Model
import Banyan.Route (Route (..))
import qualified Data.Map.Strict as Map
import qualified Ema
import qualified Ema.CLI
import qualified Ema.Helper.Tailwind as Tailwind
import NeatInterpolation (text)
import qualified Shower
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

renderHtml :: Ema.CLI.Action -> Model -> Route -> LByteString
renderHtml emaAction model r =
  Tailwind.layout emaAction (H.title "Banyan" >> H.base ! A.href "/" >> H.link ! A.rel "shortcut icon" ! A.href "banyan.svg" ! A.type_ "image/svg") $
    H.div ! A.class_ "container mx-auto" $ do
      H.div ! A.class_ "mt-8 p-2" $ do
        H.div ! A.class_ "bg-gray-200 p-2 rounded text-center" $
          H.pre $ H.toHtml $ newFileCli $ _modelNextID model
        case r of
          RIndex -> do
            "You are on the index page. "
            forM_ (Map.keys $ _modelNodes model) $ \uuid ->
              H.li $ H.code $ routeElem (Right $ RNode uuid) $ show uuid
          RNode uuid -> do
            "You are on the node page: " <> show uuid
            case modelLookup uuid model of
              Nothing -> "Not found"
              Just (mMeta, pandoc) -> do
                H.div ! A.class_ "border-2 m-4 p-2" $ Markdown.renderPandoc pandoc
                H.div $ H.toHtml $ show @Text mMeta
            routeElem (Right RIndex) "Go to Index"
      H.div ! A.class_ "font-mono text-xs flex items-center justify-center" $ do
        H.pre $ H.toHtml (Shower.shower $ G.toTree $ _modelGraph model)
        when (Map.size (_modelErrors model) > 0) $ do
          H.div ! A.class_ "text-red-200 p-2" $ do
            "Errors:"
            forM_ (Map.toList $ _modelErrors model) $ \(fp, err) ->
              H.div $ H.toHtml $ show @Text fp <> ": " <> show err
  where
    routeElem (r' :: Either FilePath Route) w =
      H.a ! A.class_ "text-red-500 hover:underline" ! routeHref r' $ w
    routeHref r' =
      A.href (fromString . toString $ Ema.routeUrl model r')
    newFileCli :: NodeID -> Text
    newFileCli (show -> nid) =
      [text|
        echo "---\ndate: $(date -u +'%Y-%m-%dT%H:%M:%S')\n---\n\n" >${nid}.md; cat >>${nid}.md
      |]
