--------------------------------------------------------------------------------
-- |
-- Module      : Network.HTTP.Server.Logger
-- Copyright   : (c) Galois, Inc. 2007, 2008
-- License     : BSD3
--
-- Maintainer  : diatchki@galois.com
-- Stability   : provisional
-- Portability :
--

module Network.HTTP.Server.Logger
  ( Logger(..)
  , stdLogger, quietLogger, utf8Logger
  , LogItem(..), LogType(..)
  , showLogItem, readLogItem, filterLog
  ) where

import System.IO (Handle,stdout,stderr,hFlush)
import qualified System.IO.UTF8 as UTF8 (hPutStrLn)

-- | A type used by the server to report various events.
-- Useful for debugging.
data Logger
 = Logger
     { logInfo     :: Int -> String -> IO ()
     , logDebug    :: String -> IO ()
     , logError    :: String -> IO ()
     , logWarning  :: String -> IO ()
     , getLog      :: Maybe Int         -- limit
                   -> (LogType -> Bool) -- which items
                   -> IO [LogItem]
     }

notSaved           :: Maybe Int -> (LogType -> Bool) -> IO [LogItem]
notSaved l p        = return $ filterLog l p
                              [LogItem Warning "Not saving the log"]

-- | A logger that uses the standard output and standard error.
-- Text is UTF8 encoded.
stdLogger          :: Logger
stdLogger           = utf8Logger stdout stderr

-- | A logger that does not report anything.
quietLogger        :: Logger
quietLogger =
  Logger
    { logInfo    = \ _ _  -> return ()
    , logDebug   = \_     -> return ()
    , logError   = \_     -> return ()
    , logWarning = \_     -> return ()
    , getLog     = notSaved
    }

-- | A logger that uses the given handles for output and errors.
utf8Logger :: Handle -> Handle -> Logger
utf8Logger h hErr =
  Logger
    { logInfo    = \ _lev s -> logUTF8 h (LogItem (Info _lev) s)
    , logDebug   = logUTF8 h . LogItem Debug
    , logError   = logUTF8 hErr . LogItem Error
    , logWarning = logUTF8 hErr . LogItem Warning
    , getLog     = notSaved
    }

logUTF8 :: Handle -> LogItem -> IO ()
logUTF8 h i = UTF8.hPutStrLn h (showLogItem i) >> hFlush h


data LogType  = Error | Warning | Debug | Info Int  deriving Show
data LogItem  = LogItem { item_type :: LogType, item_data :: String }

showLogItem :: LogItem -> String
showLogItem (LogItem t txt) = show t ++ ": " ++ txt

readLogItem :: String -> Maybe LogItem
readLogItem l =
  case break (':' ==) l of
    ("Error",_:txt)   -> Just $ LogItem Error txt
    ("Warning",_:txt) -> Just $ LogItem Warning txt
    ("Debug",_:txt)   -> Just $ LogItem Debug txt
    ('I':'n':'f':'o':' ':lvl,_:txt) ->
       case reads lvl of
         [(n,"")] -> Just $ LogItem (Info n) txt
         _        -> Nothing
    _ -> Nothing

-- NOTE: always reads the whole file!
filterLog :: Maybe Int -> (LogType -> Bool) -> [LogItem] -> [LogItem]
filterLog limit choose ls = case limit of
                              Just n -> take n allItems
                              _ -> allItems
  where allItems = filter (choose . item_type) ls
