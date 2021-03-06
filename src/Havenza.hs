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

import Data.ByteString.Lazy (ByteString)

import qualified Data.Map.Strict as M

import Polysemy.Error

import Servant
import Servant.Multipart

import Model.Types
import Effects.Project
import View.Page

emptyAppState :: Projects
emptyAppState = M.empty

-- Handlers

avenzaHandlers :: ServerT WebAPI (App r)
avenzaHandlers =
  apiHandlers
  :<|> handleGetIndexPage
  :<|> handlePostCreateProject
  :<|> handleGetProject
  where
    apiHandlers =
      handlePostProjectUploadFile
      :<|> handleGetProjectProjectAvenzamap
      :<|> handleGetMapFile

handlePostProjectUploadFile :: ProjectName -> UploadedFiles -> App r (HTMLTemplate ProjectPage)
handlePostProjectUploadFile projectName uploadedFiles = do
  projectFiles <- addFilesToProject projectName uploadedFiles
  pure $ HTMLTemplate $ ProjectPage projectName $ M.keys projectFiles

handleGetProjectProjectAvenzamap :: ProjectName -> App r AvenzaMap
handleGetProjectProjectAvenzamap projectName =
  AvenzaMap projectName <$> getProject projectName

handleGetIndexPage :: App r (HTMLTemplate IndexPage)
handleGetIndexPage = HTMLTemplate . IndexPage . M.keys <$> getProjects

handlePostCreateProject :: NewProject -> App r NoContent
handlePostCreateProject (NewProject projectName files) = do
  _ <- addFilesToProject projectName files
  let projectPath = toHeader $ safeLink webApi (Proxy @ProjectAPI) projectName
  throw $ err301 
    { errHeaders = 
      [("Location", "/" <> projectPath)] 
    }

handleGetProject :: ProjectName -> App r (HTMLTemplate ProjectPage)
handleGetProject projectName =
  HTMLTemplate . ProjectPage projectName . M.keys <$> getProject projectName

handleGetMapFile :: ProjectName -> MapFileName -> App r ByteString
handleGetMapFile projectName mapFile =
  getProjectFile projectName mapFile >>= \case
    Nothing -> throw $ err404 {errReasonPhrase = "File Not Found"}
    Just (UploadedFile fileData) -> pure $ fdPayload fileData

