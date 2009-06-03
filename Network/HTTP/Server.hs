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
  ( server, Handler
  , module HTTP.Exports
  , module Network.HTTP.Server.Response
  ) where

-- Re-exported for convenince
import Network.HTTP.Headers as HTTP.Exports
import Network.HTTP as HTTP.Exports
      (Request(..), Response(..), RequestMethod(..))

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
import Network.URI
import Network.URL
import Control.Concurrent(forkIO)
import Control.Exception(catch,SomeException)
import Data.Maybe(fromMaybe)


-- XXX:  Add timeouts to close lingering connections

-- Note: See http://www.haskell.org/ghc/docs/latest/html/libraries/network-2.1.0.0/Network.html#10 for details ob the sigPIPE handler.
server_init :: Logger -> HostName -> PortNumber -> IO Socket
server_init lg host_name port_num = withSocketsDo $
  do
#ifdef _OS_UNIX
     P.installHandler P.sigPIPE P.Ignore Nothing
#endif
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
type Handler = SockAddr -> URL -> Request_String -> IO Response_String

-- | Start a server.  Parameters:
--
--   * The logger is used to report what's going on
--     (see "Network.HTTP.Server.Logger")
--
--   * The host name to bind to (often \"localhost\")
--
--   * The port to listen on
--
--   * What do with requests. These are executed in separete threads.
server :: Logger -> HostName -> PortNumber -> Handler -> IO ()
server lg host_name port_num handler = withSocketsDo $
  do s <- server_init lg host_name port_num
     loop s `catch` \e ->
       logError lg ("Unexpected (0): " ++ show (e :: SomeException))
     sClose s
  where
  loop s = do (client_sock,sock_addr) <- accept s
              forkIO (client client_sock sock_addr)
              loop s

  get_request :: HandleStream String -> IO (Maybe (URL, Request_String))
  get_request sock =
    do mbreq <- receiveHTTP sock
       case mbreq of
         Left err  -> logError lg (show err) >> return Nothing
         Right req ->
           let url_txt = show (rqURI req)
           in case importURL url_txt of
                Just url -> return (Just (url,req))
                Nothing  -> do logError lg ("Invalid URL: " ++ url_txt)
                               return Nothing


  client sock addr =
    do let name = ppSockAddr addr ""
       logInfo lg 0 ("Accepted connection from " ++ name)
       logInfo lg 0 "Accepted connection"
       conn <- socketConnection name sock    -- XXX: name?
       setStreamHooks conn nullHooks { hook_close = sClose sock }
       client_interact addr conn


  client_interact :: SockAddr -> HandleStream String -> IO ()
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
                           return (err_response InternalServerError
                                      (reason InternalServerError))
                 Nothing -> return (err_response BadRequest
                                          (reason BadRequest))

       let resp1 = fromMaybe resp
                 $ do (_,rq)  <- mbreq
                      "close" <- findHeader HdrConnection rq
                      return (insertHeaderIfMissing HdrConnection "close" resp)
           closing  = case findHeader HdrConnection resp1 of
                        Just "close" -> True
                        _            -> False
           msg_len  = length (rspBody resp1)
           resp2    = insertHeaderIfMissing HdrContentLength
                                                          (show msg_len) resp1
           resp3    = insertHeaderIfMissing HdrServer "ML Wiki" resp2

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


