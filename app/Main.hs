module Main (main) where

import Data.IORef (newIORef)
import Data.Proxy (Proxy(..))
import Data.Default.Class

import Servant.Multipart
import Servant.Server
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Parse(clearMaxRequestNumFiles, defaultParseRequestBodyOptions)

import Havenza

context :: Context '[MultipartOptions Mem]
context = multiOptions :. EmptyContext where
  multiOptions = (defaultMultipartOptions Proxy)
    { generalOptions = clearMaxRequestNumFiles defaultParseRequestBodyOptions }

main :: IO ()
main = do
  ref <- newIORef emptyAppState
  run 8080 $ 
    logStdout $
    gzip def $
    serveWithContext webApi context $ 
    hoistServer webApi (runApp ref) avenzaHandlers
