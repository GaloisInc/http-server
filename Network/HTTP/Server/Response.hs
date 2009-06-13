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
import Network.BufferType

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



-- | Make a simple response with the given status and body.
-- Intended to be used for (bad) errors.
-- Adds a "close" header.
err_response :: BufferType a => StatusCode -> Response a
err_response = insertHeader HdrConnection "close" . respond

-- | Make a simple response with the given status and body.
-- No headers or body.
respond :: BufferType a => StatusCode -> Response a
respond code = Response
  { rspCode     = statusCodeTriplet code
  , rspReason   = reason code
  , rspHeaders  = []
  , rspBody     = buf_empty bufferOps
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
