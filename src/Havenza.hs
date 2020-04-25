{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

{- |
Copyright: (c) 2020 Alex Mason
SPDX-License-Identifier: MIT
Maintainer: Alex Mason <github@me.axman6.com>

A server for generating .avenzamaps documents from uploaded PDFs
-}

module Havenza
       ( API
       , WebAPI
       , runApp
       , emptyAppState
       , api
       , webApi
       , avenzaHandlers
       ) where

import Control.Monad ((>=>))
import Control.Monad.Except      (ExceptT(..))

import Data.ByteString.Lazy (ByteString)

import Data.IORef (IORef, atomicModifyIORef', readIORef)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Maybe (fromMaybe)

import Data.Text (Text)

import Polysemy
import Polysemy.Error

import Text.Blaze
import Text.Blaze.Html5            as H hiding (embed)
import Text.Blaze.Html5.Attributes as A hiding (id)

import Servant
import Servant.HTML.Blaze
import Servant.Multipart

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

newtype IndexPage = IndexPage [ProjectName]
newtype ProjectName = ProjectName { unProjectName :: Text }
  deriving stock (Show, Eq)
  deriving newtype (Ord, FromHttpApiData, ToHttpApiData)

data ProjectPage = ProjectPage ProjectName [MapFileName]
    deriving stock (Show, Eq)
newtype MapFileName = MapFileName Text
  deriving stock (Show, Eq)
  deriving newtype (Ord, FromHttpApiData, ToHttpApiData)

newtype HTMLTemplate a = HTMLTemplate a

newtype UploadedFile = UploadedFile (FileData Mem)
  deriving stock (Eq, Show)

instance FromMultipart Mem UploadedFile where
  fromMultipart multipartData =
    UploadedFile <$> lookupFile "file" multipartData

type AvenzaMap = Text

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

type App r = Sem (Project ': Error ServerError ': r)

type Projects = Map ProjectName ProjectFiles

emptyAppState :: Projects
emptyAppState = M.empty

type ProjectFiles = Map MapFileName UploadedFile

data Project m a where
  GetProjects      :: Project m Projects
  GetProject       :: ProjectName -> Project m ProjectFiles
  GetProjectFile   :: ProjectName -> MapFileName -> Project m (Maybe UploadedFile)
  AddFileToProject :: ProjectName -> UploadedFile -> Project m ProjectFiles

makeSem ''Project

runApp :: IORef Projects -> App '[Embed IO] a -> Handler a
runApp ref = Handler . ExceptT . runM . runError . interpretProjectIORef ref


interpretProjectIORef :: Member (Embed IO) r => IORef Projects -> Sem (Project ': r) a -> Sem r a
interpretProjectIORef ref = interpret $ \case
  GetProjects                               -> embed $ readIORef ref
  GetProject projectName                    -> embed $ getProject' projectName
  GetProjectFile projectName mapFileName    ->
     embed $ fmap (M.lookup projectName >=> M.lookup mapFileName) $ readIORef ref
  AddFileToProject projectName uploadedFile ->
     embed $ addFileToProject' projectName uploadedFile
  where
    getProject' :: ProjectName -> IO ProjectFiles
    getProject' projectName = do
      projectFiles <- atomicModifyIORef' ref $ \projects ->
        case M.lookup projectName projects of
          Nothing -> (M.insert projectName M.empty projects, M.empty)
          Just projectFiles -> (projects, projectFiles)
      print (projectName, projectFiles)
      pure projectFiles

    addFileToProject' :: ProjectName -> UploadedFile -> IO ProjectFiles
    addFileToProject' projectName uploadedFile@(UploadedFile FileData{..}) = do
      projectFiles <- atomicModifyIORef' ref $ \projects ->
        let currentProject = fromMaybe M.empty $ M.lookup projectName projects
            newFiles = M.insert (MapFileName fdFileName) uploadedFile currentProject
        in (M.insert projectName newFiles projects, newFiles)
      print (projectName, projectFiles)
      pure projectFiles


-- Handlers

avenzaHandlers :: ServerT WebAPI (App r)
avenzaHandlers =
  apiHandlers
  :<|> handleGetIndexPage
  :<|> handleGetProject
  where
    apiHandlers =
      handlePostProjectUploadFile
      :<|> handleGetProjectProjectAvenzamap
      :<|> handleGetMapFile

handlePostProjectUploadFile :: ProjectName -> UploadedFile -> App r (HTMLTemplate ProjectPage)
handlePostProjectUploadFile projectName uploadedFile = do
  projectFiles <- addFileToProject projectName uploadedFile
  pure $ HTMLTemplate $ ProjectPage projectName $ M.keys projectFiles

handleGetProjectProjectAvenzamap :: ProjectName -> App r Text
handleGetProjectProjectAvenzamap (ProjectName project) = pure $ "handleGetProjectProjectAvenzamap: " <> project

handleGetIndexPage :: App r (HTMLTemplate IndexPage)
handleGetIndexPage = HTMLTemplate . IndexPage . M.keys <$> getProjects

handleGetProject :: ProjectName -> App r (HTMLTemplate ProjectPage)
handleGetProject projectName =
  HTMLTemplate . ProjectPage projectName . M.keys <$> getProject projectName

handleGetMapFile :: ProjectName -> MapFileName -> App r ByteString
handleGetMapFile projectName mapFile = do
  getProjectFile projectName mapFile >>= \case
    Nothing -> throw $ err404 {errReasonPhrase = "File Not Found"}
    Just (UploadedFile fileData) -> pure $ fdPayload fileData

instance ToMarkup IndexPage where
  toMarkup (IndexPage projects) = do
    p "Projects:"
    ul $ mapM_ linkProject projects
    where
      linkProject :: ProjectName -> Markup
      linkProject project@(ProjectName projectText) =
        let lnk = safeLink webApi (Proxy @ProjectAPI) project
        in li $
            a ! href (toValue $ toUrlPiece lnk) $ toMarkup projectText

instance ToMarkup ProjectPage where
  toMarkup (ProjectPage project fileNames) =
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

instance ToMarkup a => ToMarkup (HTMLTemplate a) where
  toMarkup (HTMLTemplate wrapped) =
    docTypeHtml $
      H.html ! lang "en" $ do
        H.head $ do
          meta ! charset "utf-8"
          meta ! name "viewport" ! content "width=device-width, initial-scale=1"
          link ! rel "stylesheet" ! href "https://cdn.rawgit.com/Chalarangelo/mini.css/v3.0.1/dist/mini-default.min.css"
          -- link ! rel "stylesheet" ! href "//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"
          -- link ! rel "stylesheet" ! href "//cdnjs.cloudflare.com/ajax/libs/normalize/5.0.0/normalize.css"
          -- link ! rel "stylesheet" ! href "//cdnjs.cloudflare.com/ajax/libs/milligram/1.3.0/milligram.css"
        H.body $
          H.main ! class_ "wrapper" $
            toMarkup wrapped
