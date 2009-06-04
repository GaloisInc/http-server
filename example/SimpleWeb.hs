import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL
import Text.JSON
import Text.JSON.String(runGetJSON)
import Text.XHtml
import Codec.Binary.UTF8.String
import Control.Exception(try,SomeException)
import System.FilePath


jsonHandler :: (JSValue -> IO JSValue) -> String -> IO (Response String)
jsonHandler h txt =
  case runGetJSON readJSValue txt of
    Right val -> sendJSON OK `fmap` h val
    Left err  -> return $ sendJSON NotFound
                        $ JSObject $ toJSObject [("error", JSString $ toJSString err)]


sendText     :: StatusCode -> String -> Response String
sendText s v  = insertHeader HdrContentLength (show (length txt))
              $ insertHeader HdrContentEncoding "UTF-8"
              $ insertHeader HdrContentEncoding "text/plain"
              $ (respond s :: Response String) { rspBody = txt }
  where txt   = encodeString v

sendJSON     :: StatusCode -> JSValue -> Response String
sendJSON s v  = insertHeader HdrContentType "application/json"
              $ sendText s (showJSValue v "")

sendHTML     :: StatusCode -> Html -> Response String
sendHTML s v  = insertHeader HdrContentType "text/html"
              $ sendText s (renderHtml v)

sendScript   :: StatusCode -> String -> Response String
sendScript s v  = insertHeader HdrContentType "application/x-javascript"
                $ sendText s v

main :: IO ()
main = server stdLogger "localhost" 8888 $ \_ url request ->
  print request >>
  case rqMethod request of
    GET ->
      case takeExtension (url_path url) of
        ".js" ->
          do mb_txt <- try (readFile (url_path url))
             case mb_txt of
               Right a -> return $ sendScript OK a
               Left e  -> return $ sendHTML NotFound
                                 $ toHtml "Static content not found"
                 where _hack :: SomeException
                       _hack = e   -- to specify the type
        ".html" -> return $ sendHTML OK $
               thehtml $ concatHtml
                 [ thead $ concatHtml
                         $ map addScript
                            [ "jquery-1.3.2.min.js", "myscript.js" ]
                 , body $ concatHtml
                    [ toHtml "Hello"
                    ]
                 ]
        _ -> return $ sendHTML NotFound $ toHtml "Not found"
    POST -> jsonHandler jsonExample $ decodeString $ rqBody request
    _ -> return $ sendHTML NotFound $ toHtml "I don't understand"

  where addScript x = script noHtml ! [ thetype "text/javascript", src x ]

jsonExample  :: JSValue -> IO JSValue
jsonExample v =
  do putStrLn "received:"
     putStrLn (showJSValue v "")
     return $ JSObject $ toJSObject [("success", JSString $ toJSString "hello")]

