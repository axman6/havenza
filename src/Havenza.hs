{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Error.Class (MonadError(..))

import Data.ByteString.Lazy (ByteString)

import Data.IORef (IORef, atomicModifyIORef', readIORef)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Text (Text)

import Text.Blaze
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Servant
import Servant.HTML.Blaze
import Servant.Multipart

type Index = Get '[HTML] (HTMLTemplate IndexPage)

type Project 
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
  deriving newtype (FromHttpApiData, ToHttpApiData)

data ProjectPage = ProjectPage ProjectName [MapFileName] 
  deriving stock (Show, Eq)
newtype MapFileName = MapFileName Text
  deriving stock (Show, Eq)
  deriving newtype (Ord, FromHttpApiData, ToHttpApiData)

newtype HTMLTemplate a = HTMLTemplate a

data UploadedFile = UploadedFile (FileData Mem)
  deriving stock (Eq, Show)

instance FromMultipart Mem UploadedFile where
  fromMultipart multipartData =
    UploadedFile <$> lookupFile "file" multipartData

type AvenzaMap = Text

type WebAPI
  = API
  :<|> Index 
  :<|> Project

type API 
  = ProjectUploadFile
  :<|> ProjectAvenzamap
  :<|> MapFile

api :: Proxy API
api = Proxy

webApi :: Proxy WebAPI
webApi = Proxy

newtype App a = App { _runApp :: ReaderT (IORef AppState) Handler a }
  deriving newtype ( Functor, Applicative, Monad
                   , MonadReader (IORef AppState)
                   , MonadError ServerError
                   , MonadIO)

runApp :: IORef AppState -> App a -> Handler a
runApp ref = flip runReaderT ref . _runApp

data AppState = AppState
  {_asProjectMap :: Map ProjectName (Map MapFileName UploadedFile)
  } deriving stock (Show, Eq)

emptyAppState :: AppState
emptyAppState = AppState M.empty

-- Handlers

avenzaHandlers :: ServerT WebAPI App
avenzaHandlers =
  apiHandlers
  :<|> handleGetIndexPage
  :<|> handleGetProject
  where 
    apiHandlers =
      handlePostProjectUploadFile 
      :<|> handleGetProjectProjectAvenzamap
      :<|> handleGetMapFile

handlePostProjectUploadFile :: ProjectName -> UploadedFile -> App (HTMLTemplate ProjectPage)
handlePostProjectUploadFile projectName uploadedFile@(UploadedFile fileData) = do
  ref <- ask
  projectFiles <- liftIO $ atomicModifyIORef' ref $ \(AppState projects) -> 
    case M.lookup projectName projects of
      Nothing ->
        let newFiles = M.singleton (MapFileName $ fdFileName fileData) uploadedFile
        in (AppState $ M.insert projectName newFiles projects, newFiles)
      Just projectFiles -> 
        let newFiles = M.insert (MapFileName $ fdFileName fileData) uploadedFile projectFiles
        in (AppState $ M.insert projectName newFiles projects, newFiles)
  liftIO $ print (projectName, projectFiles)
  pure $ HTMLTemplate $ ProjectPage projectName $ M.keys projectFiles

handleGetProjectProjectAvenzamap :: ProjectName -> App Text
handleGetProjectProjectAvenzamap (ProjectName project) = pure $ "handleGetProjectProjectAvenzamap: " <> project

handleGetIndexPage :: App (HTMLTemplate IndexPage)
handleGetIndexPage = do
  ref <- ask
  AppState projects <- liftIO $ readIORef ref
  pure $ HTMLTemplate $ IndexPage $ M.keys projects

handleGetProject :: ProjectName -> App (HTMLTemplate ProjectPage)
handleGetProject projectName = do
  ref <- ask
  projectFiles <- liftIO $ atomicModifyIORef' ref $ \oldState@(AppState projects) -> 
    case M.lookup projectName projects of
      Nothing -> (AppState $ M.insert projectName M.empty projects, M.empty)
      Just projectFiles -> (oldState, projectFiles)
  liftIO $ print (projectName, projectFiles)
  pure $ HTMLTemplate $ ProjectPage projectName $ M.keys projectFiles

handleGetMapFile :: ProjectName -> MapFileName -> App ByteString
handleGetMapFile projectName mapFile = do
  ref <- ask
  AppState projects <- liftIO $ readIORef ref
  case M.lookup projectName projects >>= M.lookup mapFile of
    Nothing -> throwError $ err404 {errReasonPhrase = "File Not Found"}
    Just (UploadedFile fileData) -> pure $ fdPayload fileData

instance ToMarkup IndexPage where
  toMarkup (IndexPage projects) = do
    p "Projects:"
    ul $ mapM_ linkProject projects
    where
      linkProject :: ProjectName -> Markup
      linkProject project@(ProjectName projectText) =
        let lnk = safeLink webApi (Proxy @Project) project
        in li $ 
            a ! href (toValue $ toUrlPiece lnk) $ toMarkup projectText

instance ToMarkup ProjectPage where
  toMarkup (ProjectPage project fileNames) = do
    p "Files:"
    ul $ mapM_ linkFile fileNames
    H.form ! action "#" ! method "post" ! enctype "multipart/form-data" $ do
        p "Upload file:"
        input ! type_ "file" ! name "file"
        br
        input ! type_ "submit" ! name "submit"
    where
      linkFile :: MapFileName -> Markup
      linkFile mapFile@(MapFileName mapFileText) =
        let lnk = safeLink webApi (Proxy @MapFile) project mapFile
        in a ! href ("/" <> toValue (toUrlPiece lnk)) $ toMarkup mapFileText

instance ToMarkup a => ToMarkup (HTMLTemplate a) where
  toMarkup (HTMLTemplate wrapped) =
    H.html $ 
      H.body $ 
        toMarkup wrapped