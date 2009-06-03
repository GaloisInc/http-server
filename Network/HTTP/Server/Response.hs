--------------------------------------------------------------------------------
-- |
-- Module      : Network.HTTP.Server.Response
-- Copyright   : (c) Galois, Inc. 2007, 2008
-- License     : BSD3
--
-- Maintainer  : diatchki@galois.com
-- Stability   : provisional
-- Portability :
--

module Network.HTTP.Server.Response where

import Network.HTTP

-- | A list of status code.  This not yet complete.
data StatusCode
  = OK
  | SeeOther
  | BadRequest
  | Forbidden
  | NotFound
  | Found
  | Conflict
  | InternalServerError
  | NotImplemented



-- | Make a simple response with the given staus and body.
-- Intended to be used for (bad) erros.
-- Adds a "close" header.
err_response :: StatusCode -> a -> Response a
err_response code b = insertHeader HdrConnection "close" (respond code b)

-- | Make a simple response with the given staus and body.
-- No headers.
respond :: StatusCode -> a -> Response a
respond code b = Response
  { rspCode     = statusCodeTriplet code
  , rspReason   = reason code
  , rspHeaders  = []
  , rspBody     = b
  }

-- | A brief description of what happend.
reason :: StatusCode -> String
reason code = case code of
  OK                  -> "OK"
  SeeOther            -> "See other"
  BadRequest          -> "Bad request"
  Conflict            -> "Conflict"
  Forbidden           -> "Forbidden"
  NotFound            -> "Not found"
  Found               -> "Found"
  InternalServerError -> "Internal server error"
  NotImplemented      -> "Not implemented"

statusCodeTriplet :: StatusCode -> (Int,Int,Int)
statusCodeTriplet x = case x of
  OK                  -> (2,0,0)
  Found               -> (3,0,2)
  SeeOther            -> (3,0,3)
  BadRequest          -> (4,0,0)
  Forbidden           -> (4,0,3)
  NotFound            -> (4,0,4)
  Conflict            -> (4,0,9)
  InternalServerError -> (5,0,0)
  NotImplemented      -> (5,0,1)


