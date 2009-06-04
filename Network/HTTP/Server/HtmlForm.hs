--------------------------------------------------------------------------------
-- |
-- Module      : Network.HTTP.Server.HtmlForm
-- Copyright   : (c) Galois, Inc. 2007, 2008
-- License     : BSD3
--
-- Maintainer  : diatchki@galois.com
-- Stability   : provisional
-- Portability :
--

module Network.HTTP.Server.HtmlForm
  ( FormFields
  , fieldNames, hasField
  , lookupString, lookupRead
  , toList
  , fromRequest
  ) where

import Codec.MIME.Parse
import Codec.MIME.Type
import Network.HTTP
import Codec.Binary.UTF8.String as UTF8
import Data.Char(isSpace)

-- | An abstraction of a map mapping form fields to their values.
newtype FormFields = FF [(String,String)] deriving Show

-- | The names of the fields that were posted.
fieldNames :: FormFields -> [String]
fieldNames (FF xs) = map fst xs

-- | Do we have the given field?
hasField :: FormFields -> String -> Bool
hasField (FF xs) x = x `elem` map fst xs

-- | Lookup a field value as a string.
lookupString :: FormFields -> String -> Maybe String
lookupString (FF xs) x = (drop_r . UTF8.decodeString) `fmap` lookup x xs
  where drop_r ('\r' : '\n' : cs) = '\n' : drop_r cs
        drop_r (c:cs) = c : drop_r cs
        drop_r [] = []

-- | Lookup a field value and try to parse it.
lookupRead :: Read a => FormFields -> String -> Maybe a
lookupRead xs x        = do y <- lookupString xs x
                            case reads y of
                              [(n,cs)] | all isSpace cs -> return n
                              _ -> Nothing

-- | The fields as pairs of strings.
toList :: FormFields -> [(String,String)]
toList (FF xs) = xs

-- | Try to parse the body of a request.
fromRequest :: Request String -> Maybe FormFields
fromRequest r = let mv = mime_request r
                in case mimeType (mime_val_type mv) of
                     Multipart FormData -> Just (FF (toMap mv))
                     _                  -> Nothing

  where toMap mv = case mime_val_content mv of
                     Multi vs -> concatMap toMap vs
                     Single v -> [ (UTF8.decodeString k, v)
                                              | k <- keys (mime_val_disp mv) ]

        -- XXX: should we check the type?
        keys (Just x) = [ k | Name k <- dispParams x ]
        keys Nothing  = []

mime_request :: Request String -> MIMEValue
mime_request req
  = let hdrs = map (\ (Header a b) -> (show a, b)) (rqHeaders req)
        body = rqBody req
    in parseMIMEBody hdrs body

