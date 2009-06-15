import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.HTTP.Server.HtmlForm as Form
import Network.URL as URL
import Text.JSON
import Text.JSON.String(runGetJSON)
import Text.XHtml
import Codec.Binary.UTF8.String
import Control.Exception(try,SomeException)
import System.FilePath
import Data.List(isPrefixOf)

main :: IO ()
main = serverWith defaultConfig { srvLog = stdLogger, srvPort = 8888 }
     $ \_ url request ->

  case rqMethod request of

    GET ->
      do let ext = takeExtension (url_path url)
         mb_txt <- try (readFile (url_path url))
         case mb_txt of
           Right a -> return $ if ext == ".html"
                                  then sendHTML OK (primHtml a)
                                  else sendScript OK a
           Left e -> return $ sendHTML NotFound $
               thehtml $ concatHtml
                 [ thead noHtml
                 , body $ concatHtml
                    [ toHtml "I could not find "
                    , toHtml $ exportURL url { url_type = HostRelative }
                    , toHtml ", so I made this with XHTML combinators. "
                    , toHtml $ hotlink "example.html" (toHtml "Try this instead.")
                    ]
                 ]

                 where _hack :: SomeException
                       _hack = e   -- to specify the type


    POST ->
      return $
      case findHeader HdrContentType request of
        Just ty
          | "application/x-www-form-urlencoded" `isPrefixOf` ty ->
          case URL.importParams txt of
            Just fields -> sendHTML OK $
              toHtml "You posted a URL encoded form:" +++ br +++
              toHtml (show fields) +++ br +++
              hotlink "example.html" (toHtml "back")
            Nothing -> sendHTML BadRequest $
              toHtml "Could not understand URL encoded form data"

          | "multipart/form-data" `isPrefixOf` ty ->
          case Form.fromRequest request of
            Just fields -> sendHTML OK $
              toHtml "You posted a multipart form:" +++ br +++
              toHtml (show (Form.toList fields)) +++ br +++
              hotlink "example.html" (toHtml "back")
            Nothing -> sendHTML BadRequest $
              toHtml "Could not understand multipart form data"

          | "application/json" `isPrefixOf` ty ->
          case runGetJSON readJSValue txt of
            Right val -> sendJSON OK $
              JSObject $ toJSObject [("success", JSString $ toJSString "hello")]
            Left err -> sendJSON BadRequest $
              JSObject $ toJSObject [("error", JSString $ toJSString err)]

        x -> sendHTML BadRequest $
             toHtml $ "I don't know how to deal with POSTed content" ++
                      " of type " ++ show x

        -- we assume UTF8 encoding
        where txt = decodeString (rqBody request)

    _ -> return $ sendHTML BadRequest $ toHtml "I don't understand"



sendText       :: StatusCode -> String -> Response String
sendText s v    = insertHeader HdrContentLength (show (length txt))
                $ insertHeader HdrContentEncoding "UTF-8"
                $ insertHeader HdrContentEncoding "text/plain"
                $ (respond s :: Response String) { rspBody = txt }
  where txt       = encodeString v

sendJSON       :: StatusCode -> JSValue -> Response String
sendJSON s v    = insertHeader HdrContentType "application/json"
                $ sendText s (showJSValue v "")

sendHTML       :: StatusCode -> Html -> Response String
sendHTML s v    = insertHeader HdrContentType "text/html"
                $ sendText s (renderHtml v)

sendScript     :: StatusCode -> String -> Response String
sendScript s v  = insertHeader HdrContentType "application/x-javascript"
                $ sendText s v
