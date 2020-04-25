module Model.Types
  ( module Model.Types
  ) where

import Data.ByteString.Lazy (ByteString)

import Data.Map.Strict (Map)

import Data.Text (Text)

import Servant
import Servant.HTML.Blaze
import Servant.Multipart

import Text.Blaze
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A

import View.Page


-- API


type Index = Get '[HTML] (HTMLTemplate IndexPage)

type ProjectAPI
  = "project"
  :> Capture "projectName" ProjectName
  :> Get '[HTML] (HTMLTemplate ProjectPage)

type ProjectUploadFile
  = "project"
  :> Capture "projectName" ProjectName
  :> MultipartForm Mem UploadedFile
  :> Post '[HTML] (HTMLTemplate ProjectPage)

type ProjectAvenzamap
  = "project"
  :> Capture "projectName" ProjectName
  :> "MapCollection.avenzamaps"
  :> Get '[JSON] AvenzaMap

type MapFile
  = "project"
  :> Capture "projectName" ProjectName
  :> "file"
  :> Capture "fileName" MapFileName
  :> Get '[OctetStream] ByteString

type WebAPI
  = API
  :<|> Index
  :<|> ProjectAPI

type API
  = ProjectUploadFile
  :<|> ProjectAvenzamap
  :<|> MapFile

api :: Proxy API
api = Proxy

webApi :: Proxy WebAPI
webApi = Proxy

-- Pages


newtype ProjectName = ProjectName { unProjectName :: Text }
  deriving stock (Show, Eq)
  deriving newtype (Ord, FromHttpApiData, ToHttpApiData)

newtype MapFileName = MapFileName Text
  deriving stock (Show, Eq)
  deriving newtype (Ord, FromHttpApiData, ToHttpApiData)

-- Types

newtype IndexPage = IndexPage [ProjectName]

data ProjectPage = ProjectPage ProjectName [MapFileName]
    deriving stock (Show, Eq)

newtype UploadedFile = UploadedFile (FileData Mem)
  deriving stock (Eq, Show)

instance FromMultipart Mem UploadedFile where
  fromMultipart multipartData =
    UploadedFile <$> lookupFile "file" multipartData

type AvenzaMap = Text

type Projects = Map ProjectName ProjectFiles

type ProjectFiles = Map MapFileName UploadedFile

instance Render IndexPage where
  render (IndexPage projects) = RenderedPage (Just "Project Index") $ do
    p "Projects:"
    ul $ mapM_ linkProject projects
    where
      linkProject :: ProjectName -> Markup
      linkProject project@(ProjectName projectText) =
        let lnk = safeLink webApi (Proxy @ProjectAPI) project
        in li $
            a ! href (toValue $ toUrlPiece lnk) $ toMarkup projectText


instance Render ProjectPage where
  render (ProjectPage project fileNames) = 
    RenderedPage (Just $ "Project: " <> unProjectName project) $ 
      H.div ! class_ "container" $
        H.div ! class_ "row" $ do
          H.div ! class_ "col-md-6 col-sm-12" $ do
            h1 $ do
              text (unProjectName project)
              small $ linkForAvenzamap project
            p "Files:"
            ul $ mapM_ linkFile fileNames
          H.div ! class_ "col-md-6 col-sm-12" $
            H.div ! class_ "section" $
              H.form ! action "#" ! method "post" ! enctype "multipart/form-data" $
                fieldset $ do
                  H.legend ! class_ "doc" $ "Upload file:"
                  input ! type_ "file" ! name "file"
                  br
                  input ! class_ "button-primary tertiary small" ! type_ "submit" ! name "submit"
    where
      linkFile :: MapFileName -> Markup
      linkFile mapFile@(MapFileName mapFileText) =
        let lnk = safeLink webApi (Proxy @MapFile) project mapFile
        in li $ a ! href ("/" <> toValue (toUrlPiece lnk)) $ toMarkup mapFileText

      linkForAvenzamap :: ProjectName -> Markup
      linkForAvenzamap projectName =
        let lnk = safeLink webApi (Proxy @ProjectAvenzamap) projectName
        in a ! href ("/" <> toValue (toUrlPiece lnk)) $ "MapCollection.avenzamaps"
