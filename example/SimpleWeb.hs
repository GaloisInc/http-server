import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.HTTP.Server.HtmlForm as Form
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
main = serverWith defaultConfig { srvLog = stdLogger, srvPort = 8888 }
     $ \_ url request ->
  print request >>
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
                    [ toHtml "I could not find the file, "
                    , toHtml "so I made this with xhtml combinators. "
                    , toHtml $ hotlink "example.html" (toHtml "Try this.")
                    ]
                 ]

                 where _hack :: SomeException
                       _hack = e   -- to specify the type


    POST ->
      case Form.fromRequest request of
        Just fields ->
          return $ sendHTML OK $
          toHtml "You posted:" +++ br +++
          toHtml (show (Form.toList fields)) +++ br +++
          hotlink "example.html" (toHtml "back")

        Nothing ->
          do putStrLn "JSON"
             jsonHandler jsonExample $ decodeString $ rqBody request

    _ -> return $ sendHTML NotFound $ toHtml "I don't understand"

jsonExample  :: JSValue -> IO JSValue
jsonExample v =
  do putStrLn "received:"
     putStrLn (showJSValue v "")
     return $ JSObject $ toJSObject [("success", JSString $ toJSString "hello")]

