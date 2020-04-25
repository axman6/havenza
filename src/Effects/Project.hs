{-# LANGUAGE TemplateHaskell            #-}

module Effects.Project 
  ( Project
  , getProjects
  , getProject
  , getProjectFile
  , addFileToProject
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
      pure projectFiles

    addFileToProject' :: ProjectName -> UploadedFile -> IO ProjectFiles
    addFileToProject' projectName uploadedFile@(UploadedFile FileData{..}) = do
      projectFiles <- atomicModifyIORef' ref $ \projects ->
        let currentProject = fromMaybe M.empty $ M.lookup projectName projects
            newFiles = M.insert (MapFileName fdFileName) uploadedFile currentProject
        in (M.insert projectName newFiles projects, newFiles)
      pure projectFiles
