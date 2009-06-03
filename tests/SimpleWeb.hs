import Network.HTTP.Server
import Network.HTTP.Server.Logger

main = server stdLogger "tangra.galois.com" 8888 $
  \_ _ r -> do putStrLn "BODY:"
               putStrLn (rqBody r)
               return (Response (2,0,0) "OK" []
                    $ unlines 
                    [ "<html>"
                    , "<head>"
                    , "<script type='text/javascript' src='http://tangra/jquery-1.3.2.min.js'></script>"
                    , "<script type='text/javascript'>"
                    , "$(document).ready(function(){"
                    , "  jQuery.ajax({url:'/', data: '{}', type: 'POST', dataType: 'text/json' });"
                    , "});"
                    , "</script>"
                    , "</head>"
                    , "<body>"
                    , "</body>"
                    ])
