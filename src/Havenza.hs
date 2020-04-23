{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Servant
import Servant.HTML.Blaze
import Servant.Multipart
import Servant.JS


type Index = Get '[HTML] IndexPage

type Project = "project" :> Capture "projectname" Text :> Get '[HTML] ProjectPage

type ProjectUploadFile
  = "project" 
  :> Capture "projectname" Text 
  :> MultipartForm Mem (MultipartData Mem)
  :> Post '[JSON] Status

type ProjectAvenzamap
  = "project" 
  :> Capture "projectname" Text 
  :> "MapIndex.avenzamap"
  :> Get '[JSON] AvenzaMap


type IndexPage = Text
type ProjectPage = Text
type Status = Text
type AvenzaMap = Text

type WebAPI
  = API
  :<|> Index 
  :<|> Project

type API 
  = ProjectUploadFile
  :<|> ProjectAvenzamap

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

handlePostProjectUploadFile :: Text -> MultipartData Mem -> Handler Text
handlePostProjectUploadFile project multipart = 
  pure $ "handlePostProjectUploadFile: " <> project <> "\n" <> renderMultipart multipart
  where 
    renderMultipart :: MultipartData Mem -> Text
    renderMultipart (MultipartData is fs) =
      T.unlines $ renderInputs is <> renderFiles fs
   
    renderInputs :: [Input] -> [Text]
    renderInputs = fmap $ \(Input name value) -> name <> ": " <> value

    renderFiles :: [FileData Mem] -> [Text]
    renderFiles = fmap $ \(FileData inputName fileName mimeType bs) ->
      fileName <> " (from input: " <> inputName <> ", mimeType: " <> mimeType <> ")\n"
      <> T.pack (show bs)

handleGetProjectProjectAvenzamap :: Text -> Handler Text
handleGetProjectProjectAvenzamap project = pure $ "handleGetProjectProjectAvenzamap: " <> project

handleGetIndexPage :: Handler IndexPage
handleGetIndexPage = pure "handleGetIndexPage"

handleGetProject :: Text -> Handler ProjectPage
handleGetProject project = pure $ "handleGetProject: " <> project
