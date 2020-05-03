{-# LANGUAGE TemplateHaskell            #-}

module Effects.Project 
  ( Project
  , getProjects
  , getProject
  , getProjectFile
  , addFilesToProject
  , App
  , runApp
  ) where

import Control.Monad ((>=>))
import Control.Monad.Except      (ExceptT(..))

import Data.IORef (IORef, atomicModifyIORef', readIORef)

import qualified Data.Map.Strict as M

import Data.Maybe (fromMaybe)

import Polysemy
import Polysemy.Error

import Servant
import Servant.Multipart

import Model.Types

type App r = Sem (Project ': Error ServerError ': r)

data Project m a where
  GetProjects       :: Project m Projects
  GetProject        :: ProjectName -> Project m ProjectFiles
  GetProjectFile    :: ProjectName -> MapFileName -> Project m (Maybe UploadedFile)
  AddFilesToProject :: ProjectName -> UploadedFiles -> Project m ProjectFiles

makeSem ''Project

runApp :: IORef Projects -> App '[Embed IO] a -> Handler a
runApp ref = Handler . ExceptT . runM . runError . interpretProjectIORef ref


interpretProjectIORef :: Member (Embed IO) r => IORef Projects -> Sem (Project ': r) a -> Sem r a
interpretProjectIORef ref = interpret $ \case
  GetProjects                               -> embed $ readIORef ref
  GetProject projectName                    -> embed $ getProject' projectName
  GetProjectFile projectName mapFileName    ->
     embed $ fmap (M.lookup projectName >=> M.lookup mapFileName) $ readIORef ref
  AddFilesToProject projectName uploadedFiles ->
     embed $ addFilesToProject' projectName uploadedFiles
  where
    getProject' :: ProjectName -> IO ProjectFiles
    getProject' projectName =
      atomicModifyIORef' ref $ \projects ->
        case M.lookup projectName projects of
          Nothing -> (M.insert projectName M.empty projects, M.empty)
          Just projectFiles -> (projects, projectFiles)

    addFilesToProject' :: ProjectName -> UploadedFiles -> IO ProjectFiles
    addFilesToProject' projectName (UploadedFiles files) =
      atomicModifyIORef' ref $ \projects ->
        let currentProject = fromMaybe M.empty $ M.lookup projectName projects
            newFiles = foldr (\file@FileData{..} -> M.insert (MapFileName fdFileName) (UploadedFile file))
                             currentProject files
        in (M.insert projectName newFiles projects, newFiles)
