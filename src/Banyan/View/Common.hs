module Banyan.View.Common where

import Banyan.Model
import Banyan.Route
import qualified Ema
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

routeElemUnlessHere :: Model -> Route -> SiteRoute -> H.Html -> H.Html
routeElemUnlessHere model hereR r' =
  if r' == SRHtml hereR
    then H.a ! A.class_ "font-semibold underline decoration-2 decoration-green-500 decoration-wavy"
    else routeElem model r'

routeElem :: Model -> SiteRoute -> H.Html -> H.Html
routeElem model r' =
  H.a ! A.class_ "text-green-600 hover:bg-green-500 hover:text-white transition-colors" ! routeHref model r'

routeHref :: Model -> SiteRoute -> H.Attribute
routeHref model r' =
  A.href (fromString . toString $ Ema.routeUrl model r')
