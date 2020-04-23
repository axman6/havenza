module Main (main) where

import Havenza

import Servant.Server
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8080 (serve webApi avenzaHandlers)
