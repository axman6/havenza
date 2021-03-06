module Model.Types
  ( module Model.Types
  ) where

import Data.Aeson

import Data.ByteString.Lazy (ByteString)

import Data.Map.Strict (Map, keys)

import Data.Text (Text)

import Servant
import Servant.HTML.Blaze
import Servant.Multipart

import Text.Blaze
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import View.Page


-- API


type Index = Get '[HTML] (HTMLTemplate IndexPage)

type CreateProject 
  = MultipartForm Mem NewProject
  :> Post '[HTML] NoContent

type ProjectAPI
  = "project"
  :> Capture "projectName" ProjectName
  :> Get '[HTML] (HTMLTemplate ProjectPage)

type ProjectUploadFile
  = "project"
  :> Capture "projectName" ProjectName
  :> MultipartForm Mem UploadedFiles
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
  :<|> CreateProject
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

data NewProject = NewProject ProjectName UploadedFiles
  deriving stock (Show, Eq)

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
  deriving stock (Show, Eq)

newtype UploadedFiles = UploadedFiles [FileData Mem]
  deriving stock (Eq, Show)

instance FromMultipart Mem NewProject where
  fromMultipart formData =
    NewProject
      <$> do ProjectName <$> lookupInput "projectName" formData
      <*> fromMultipart formData

instance FromMultipart Mem UploadedFiles where
  fromMultipart =
    pure . UploadedFiles . filter (\f -> fdInputName f == "files") . files
    -- UploadedFiles <$> lookupFile "file" multipartData

data AvenzaMap = AvenzaMap
  { _amName :: ProjectName
  , _amMaps :: ProjectFiles
  }

instance ToJSON AvenzaMap where
  toJSON (AvenzaMap projectName projectFiles) = object
    [ "Version" .= ("PDFMaps Maplist 1.1"::Text)
    , "Name" .= unProjectName projectName
    , "Collection" .= (getMapUrl <$> keys projectFiles )
    ] where
      getMapUrl :: MapFileName -> Value
      getMapUrl (MapFileName name) = 
        -- let lnk = safeLink webApi (Proxy @MapFile) projectName mapFileName
        -- in 
          object [ "Map" .= object [ "Path" .= ("file/" <> name) ] ]

type Projects = Map ProjectName ProjectFiles

type ProjectFiles = Map MapFileName UploadedFile

instance Render IndexPage where
  render (IndexPage projects) = RenderedPage (Just "Project Index") $
    H.div ! A.class_ "container" $
        H.div ! A.class_ "row" $ do
          H.div ! A.class_ "col-md-6 col-sm-12" $ do
            H.h1 $ H.text "Projects"
              -- H.small $ linkForAvenzamap project
            H.p "Projects:"
            H.ul $ mapM_ linkProject projects
          H.div ! A.class_ "col-md-6 col-sm-12" $
            H.div ! A.class_ "section" $
              H.form ! A.action "#" ! A.method "post" ! A.enctype "multipart/form-data" $
                H.fieldset $ do
                  H.legend ! A.class_ "doc" $ "New Collection:"
                  H.label ! A.for "projectName" $ "Collection Name"
                  H.input ! A.type_ "text" ! A.name "projectName"
                  H.input ! A.type_ "file" ! A.name "files" ! A.multiple ""
                  H.br
                  H.input ! A.class_ "button-primary tertiary small" ! A.type_ "submit" ! A.name "submit"

    where
      linkProject :: ProjectName -> Markup
      linkProject project@(ProjectName projectText) =
        let lnk = safeLink webApi (Proxy @ProjectAPI) project
        in H.li $ H.a ! A.href (toValue $ toUrlPiece lnk) $ toMarkup projectText


instance Render ProjectPage where
  render (ProjectPage project fileNames) = 
    RenderedPage (Just $ "Project: " <> unProjectName project) $ 
      H.div ! A.class_ "container" $
        H.div ! A.class_ "row" $ do
          H.div ! A.class_ "col-md-6 col-sm-12" $ do
            H.h1 $ do
              H.text (unProjectName project)
              H.small $ linkForAvenzamap project
            H.p "Files:"
            H.ul $ mapM_ linkFile fileNames
          H.div ! A.class_ "col-md-6 col-sm-12" $
            H.div ! A.class_ "section" $
              H.form ! A.action "#" ! A.method "post" ! A.enctype "multipart/form-data" $
                H.fieldset $ do
                  H.legend ! A.class_ "doc" $ "Upload file:"
                  H.input ! A.type_ "file" ! A.name "files" ! A.multiple ""
                  H.br
                  H.input ! A.class_ "button-primary tertiary small" ! A.type_ "submit" ! A.name "submit"
    where
      linkFile :: MapFileName -> Markup
      linkFile mapFile@(MapFileName mapFileText) =
        let lnk = safeLink webApi (Proxy @MapFile) project mapFile
        in H.li $ H.a ! A.href ("/" <> toValue (toUrlPiece lnk)) $ toMarkup mapFileText

      linkForAvenzamap :: ProjectName -> Markup
      linkForAvenzamap projectName =
        let lnk = safeLink webApi (Proxy @ProjectAvenzamap) projectName
        in H.a ! A.href ("/" <> toValue (toUrlPiece lnk)) $ "MapCollection.avenzamaps"
