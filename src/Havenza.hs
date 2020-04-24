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
       , api
       , webApi
       , avenzaAPIJS
       , avenzaHandlers
       ) where

import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Servant
import Servant.HTML.Blaze
import Servant.Multipart
import Servant.JS


type Index = Get '[HTML] (HTMLTemplate IndexPage)

type Project 
  = "project" 
  :> Capture "projectName" ProjectName
  :> Get '[HTML] (HTMLTemplate ProjectPage)

type ProjectUploadFile
  = "project" 
  :> Capture "projectName" ProjectName 
  :> MultipartForm Mem UploadedFile
  :> Post '[JSON] Status

type ProjectAvenzamap
  = "project" 
  :> Capture "projectName" ProjectName 
  :> "MapIndex.avenzamap"
  :> Get '[JSON] AvenzaMap

type MapFile
  = "project" 
  :> Capture "projectName" ProjectName 
  :> "file"
  :> Capture "fileName" MapFileName
  :> Get '[JSON] Text

newtype IndexPage = IndexPage [ProjectName]
newtype ProjectName = ProjectName { unProject :: Text }
  deriving stock (Show, Eq)
  deriving newtype (FromHttpApiData, ToHttpApiData)

data ProjectPage = ProjectPage ProjectName [MapFileName] 
  deriving stock (Show, Eq)
newtype MapFileName = MapFileName Text
  deriving stock (Show, Eq)
  deriving newtype (FromHttpApiData, ToHttpApiData)

newtype HTMLTemplate a = HTMLTemplate a

data UploadedFile = UploadedFile (FileData Mem)

instance FromMultipart Mem UploadedFile where
  fromMultipart multipartData =
    UploadedFile <$> lookupFile "file" multipartData

type Status = Text
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

avenzaAPIJS :: Text
avenzaAPIJS = jsForAPI api jquery 


-- Handlers

avenzaHandlers :: Server WebAPI
avenzaHandlers =
  apiHandlers
  :<|> handleGetIndexPage
  :<|> handleGetProject
  where 
    apiHandlers =
      handlePostProjectUploadFile 
      :<|> handleGetProjectProjectAvenzamap
      :<|> handleGetMapFile

handlePostProjectUploadFile :: ProjectName -> UploadedFile -> Handler Text
handlePostProjectUploadFile (ProjectName project) (UploadedFile fileData) = do
  let res = "handlePostProjectUploadFile: " <> project <> "\n" <> renderFile fileData
  pure res
  where 
    renderFile :: FileData Mem -> Text
    renderFile (FileData inputName fileName mimeType bs) =
      fileName <> " (from input: " <> inputName <> ", mimeType: " <> mimeType <> ")\n"
      <> T.pack (show bs)

handleGetProjectProjectAvenzamap :: ProjectName -> Handler Text
handleGetProjectProjectAvenzamap (ProjectName project) = pure $ "handleGetProjectProjectAvenzamap: " <> project

handleGetIndexPage :: Handler (HTMLTemplate IndexPage)
handleGetIndexPage = pure $ HTMLTemplate $ IndexPage [ProjectName "handleGetIndexPage", ProjectName "Another Project"]

handleGetProject :: ProjectName -> Handler (HTMLTemplate ProjectPage)
handleGetProject project@(ProjectName projectName) = 
  pure $ HTMLTemplate $ ProjectPage project [MapFileName $"handleGetProject: " <> projectName]

handleGetMapFile :: ProjectName -> MapFileName -> Handler Text
handleGetMapFile (ProjectName project) (MapFileName mapFileName) =
  pure $ "Project: " <> project <> "\nFileName: " <> mapFileName

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