module View.Page
  ( RenderedPage(..)
  , Render(..)
  , HTMLTemplate(..)
  ) where


import Data.Text (Text)
import Text.Blaze
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A

data RenderedPage = RenderedPage 
  { _rpTitle :: Maybe Text
  , _rpMarkup :: Markup
  }

class Render a where
  render :: a -> RenderedPage

newtype HTMLTemplate a = HTMLTemplate a

instance Render a => ToMarkup (HTMLTemplate a) where
  toMarkup (HTMLTemplate wrapped) =
    let RenderedPage mtitle markup = render wrapped
    in docTypeHtml $
      H.html ! lang "en" $ do
        H.head $ do
          H.title $ maybe "" toHtml mtitle
          meta ! charset "utf-8"
          meta ! name "viewport" ! content "width=device-width, initial-scale=1"
          link ! rel "stylesheet" ! href "https://cdn.rawgit.com/Chalarangelo/mini.css/v3.0.1/dist/mini-default.min.css"
          -- link ! rel "stylesheet" ! href "//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"
          -- link ! rel "stylesheet" ! href "//cdnjs.cloudflare.com/ajax/libs/normalize/5.0.0/normalize.css"
          -- link ! rel "stylesheet" ! href "//cdnjs.cloudflare.com/ajax/libs/milligram/1.3.0/milligram.css"
        H.body $
          H.main ! class_ "wrapper" $
            markup
