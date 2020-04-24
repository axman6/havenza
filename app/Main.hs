module Main (main) where

import Data.IORef (newIORef)

import Servant.Server
import Network.Wai.Handler.Warp (run)

import Havenza


main :: IO ()
main = do
  ref <- newIORef emptyAppState
  run 8080 $ serve webApi $ 
    hoistServer webApi (runApp ref) avenzaHandlers
