module Banyan.View.Common where

import Banyan.Model
import Banyan.Route
import qualified Ema
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

routeElemUnlessHere :: Model -> Route -> Either FilePath Route -> H.Html -> H.Html
routeElemUnlessHere model hereR (r' :: Either FilePath Route) =
  if r' == Right hereR
    then H.a ! A.class_ "font-semibold"
    else routeElem model r'

routeElem :: Model -> Either FilePath Route -> H.Html -> H.Html
routeElem model (r' :: Either FilePath Route) =
  H.a ! A.class_ "text-pink-500 hover:bg-pink-500 hover:text-white transition-colors" ! routeHref model r'

routeHref :: Model -> Either FilePath Route -> H.Attribute
routeHref model r' =
  A.href (fromString . toString $ Ema.routeUrl model r')
