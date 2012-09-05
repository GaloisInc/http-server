--------------------------------------------------------------------------------
-- |
-- Module      : Network.HTTP.Server
-- Copyright   : (c) Galois, Inc. 2007, 2008
-- License     : BSD3
--
-- Maintainer  : diatchki@galois.com
-- Stability   : provisional
-- Portability :
--

module Network.HTTP.Server
  ( server, serverWith, Handler
  , Config(..), defaultConfig
  , Request(..), Response(..), RequestMethod(..)
  , module Network.HTTP.Headers
  , module Network.HTTP.Server.Response
  ) where

import Network.HTTP.Server.Utils
import Network.HTTP.Server.Logger
import Network.HTTP.Server.Response

import Prelude hiding (catch)
#ifdef _OS_UNIX
import qualified System.Posix.Signals as P
                        (installHandler,sigPIPE,Handler(Ignore))
#endif
import Network.Socket
import Network.BSD
import Network.HTTP
import Network.HTTP.Headers   -- for re-export above
import Network.URI
import Network.URL
import Control.Concurrent(forkIO)
import Control.Exception(catch,SomeException)
import Data.Maybe(fromMaybe)


-- XXX:  Add timeouts to close lingering connections

-- | Server configuration.
data Config = Config
  { srvLog  :: Logger       -- ^ Server reports what's going on here.
  , srvHost :: HostName     -- ^ Host name to bind to.
  , srvPort :: PortNumber   -- ^ Port to listen on.
  }


-- | Some default options for a server:
-- no logging output, listen on \"localhost:8000\".
defaultConfig :: Config
defaultConfig = Config
  { srvLog  = quietLogger
  , srvHost = "localhost"
  , srvPort = 8000
  }





-- Note: See http://www.haskell.org/ghc/docs/latest/html/libraries/network-2.1.0.0/Network.html#10 for details ob the sigPIPE handler.
server_init :: Config -> IO Socket
server_init conf = withSocketsDo $
  do
#ifdef _OS_UNIX
     _ <- P.installHandler P.sigPIPE P.Ignore Nothing
#endif
     let host_name = srvHost conf
         lg        = srvLog conf
         port_num  = srvPort conf
     hst  <- getHostByName host_name
     s    <- socket (hostFamily hst) Stream =<< getProtocolNumber "TCP"
     setSocketOption s ReuseAddr 1      -- socket level 1 (?)
     case hostAddresses hst of
       h : _ -> bindSocket s (SockAddrInet port_num h)
       _     -> ioError (userError ("Could not resolve host address for: "
                                                          ++ host_name))
     listen s sOMAXCONN

     logInfo lg 0 ("Listening on " ++ host_name ++ ":" ++ show port_num)
     return s

-- | Handlers invoked to process requests.
-- The type parameter is for the type of the payload in the body.
-- It is a variation on string of some sort (e.g., String, ByteString, etc.)
type Handler a = SockAddr -> URL -> Request a -> IO (Response a)

-- | Start a server with the default configuration, and the given handler.
--  Requests are handled in separate threads.
server :: HStream a => Handler a -> IO ()
server = serverWith defaultConfig

-- | Start a server with the given configuration and handler.
--  Requests are handled in separate threads.
serverWith :: HStream a => Config -> Handler a -> IO ()
serverWith conf handler = withSocketsDo $
  do s <- server_init conf
     loop s `catch` \e ->
       logError lg ("Unexpected (0): " ++ show (e :: SomeException))
     sClose s
  where
  lg = srvLog conf

  loop s = do (client_sock,sock_addr) <- accept s
              _ <- forkIO (client client_sock sock_addr)
              loop s

  -- get_request :: HandleStream a -> IO (Maybe (URL, Request a))
  get_request sock =
    do mbreq <- receiveHTTP sock
       case mbreq of
         Left err  -> logError lg (show err) >> return Nothing
         Right req ->
           let url_txt = show (rqURI req)
           in case importURL url_txt of
                Just url -> return (Just (url,req))
                Nothing  ->
                  do logError lg ("Invalid URL: " ++ url_txt)
                     return Nothing


  client sock addr =
    do let client_host = ppHostAddr addr
       let portnum = portFromSockAddr addr
       let client_addr = ppSockAddr addr ""    
       logInfo lg 0 ("Accepted connection from " ++ client_addr)
       conn <- socketConnection client_host portnum sock
       setStreamHooks conn nullHooks { hook_close =
          logInfo lg 0 ("Closing  connection to " ++ client_addr)
             }
       client_interact addr conn

  -- client_interact :: SockAddr -> HandleStream a -> IO ()
  client_interact addr conn =
    do mbreq <- get_request conn
       resp <- case mbreq of
                 Just (url,req) -> do
                      auth <- authorityToAuth `fmap` getAuth req
                      handler addr url req { rqURI = (rqURI req) {
                                                  uriScheme = "http:"
                                                , uriAuthority = Just auth
                                                }
                                           }
                    `catch` \e ->
                        do logError lg ("Unexpected (1): "
                                        ++ show (e :: SomeException))
                           return (err_response InternalServerError)
                 Nothing -> return (err_response BadRequest)

       let resp1 = fromMaybe resp
                 $ do (_,rq)  <- mbreq
                      "close" <- findHeader HdrConnection rq
                      return (insertHeaderIfMissing HdrConnection "close" resp)
           closing  = case findHeader HdrConnection resp1 of
                        Just "close" -> True
                        _            -> False
{-
           msg_len  = length (rspBody resp1)
           resp2    = insertHeaderIfMissing HdrContentLength
                                                          (show msg_len) resp1
-}
           resp2    = resp1
           resp3    = insertHeaderIfMissing HdrServer
                          "Haskell HTTP Server" resp2

       respondHTTP conn resp3
       if closing
          then do close conn
                  logInfo lg 0 ("Closed connection to " ++ ppSockAddr addr "")
          else client_interact addr conn


authorityToAuth :: URIAuthority -> URIAuth
authorityToAuth a =
  URIAuth { uriUserInfo = info
          , uriRegName  = Network.HTTP.host a
          , uriPort     = maybe "" ((':':).show) (Network.HTTP.port a)
          }
  where
  info = case (user a, password a) of
           (Just x, Just y) -> x ++ ":" ++ y
           (Just x, Nothing) -> x
           _ -> ""
