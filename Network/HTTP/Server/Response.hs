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

import Network.HTTP hiding (Continue)
import Network.BufferType

-- | HTTP/1.1 status codes
data StatusCode
  = Continue -- ^ 100
  | SwitchingProtocols -- ^ 101
  | OK -- ^ 200
  | Created -- ^ 201
  | Accepted -- ^ 202
  | NonAuthoritativeInformation -- ^ 203  
  | NoContent -- ^ 204
  | ResetContent -- ^ 205
  | PartialContent -- ^ 206
  | MultipleChoices -- ^ 300
  | MovedPermanently -- ^ 301
  | Found  -- ^ 302
  | SeeOther -- ^ 303
  | NotModified -- ^ 304
  | UseProxy -- ^ 305
  | TemporaryRedirect -- ^ 307
  | BadRequest -- ^ 400
  | Unauthorized -- 401
  | PaymentRequired -- ^ 402
  | Forbidden -- ^ 403
  | NotFound -- ^ 404
  | MethodNotAllowed -- ^ 405
  | NotAcceptable -- ^ 406
  | ProxyAuthenticationRequired -- ^ 407
  | RequestTimeout -- ^ 408
  | Conflict -- ^ 409
  | Gone -- ^ 410
  | LengthRequired -- ^ 411
  | PreconditionFailed -- ^ 412
  | RequestEntityTooLarge -- ^ 413
  | RequestURITooLong -- ^ 414
  | UnsupportedMediaType -- ^ 415
  | RequestedRangeNotSatisfiable -- ^ 416
  | ExpectationFailed -- ^ 417
  | InternalServerError -- ^ 500
  | NotImplemented -- ^ 501
  | BadGateway -- ^ 502
  | ServiceUnavailable -- ^ 503
  | GatewayTimeout -- ^ 504
  | HTTPVersionNotSupported -- ^ 505

-- | Make a simple response with the given status and body.
-- Intended to be used for (bad) errors.
-- Adds a "close" header.
err_response :: BufferType a => StatusCode -> Response a
err_response code = insertHeader HdrConnection "close" (respond code)

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
  Continue                     -> "Continue"
  SwitchingProtocols           -> "Switching protocols"
  OK                           -> "OK"
  Created                      -> "Created"
  Accepted                     -> "Accepted"
  NonAuthoritativeInformation  -> "Non authoritative information"
  NoContent                    -> "No content"
  ResetContent                 -> "Reset content"
  PartialContent               -> "Partial content"
  MultipleChoices              -> "Multiple choices"
  MovedPermanently             -> "Moved permanently"
  Found                        -> "Found"
  SeeOther                     -> "See other"
  NotModified                  -> "Not modified"
  UseProxy                     -> "Use proxy"
  TemporaryRedirect            -> "Temporary redirect"
  BadRequest                   -> "Bad request"
  Unauthorized                 -> "Unauthorized"
  PaymentRequired              -> "Payment required"
  Forbidden                    -> "Forbidden"
  NotFound                     -> "Not found"
  MethodNotAllowed             -> "Method not allowed"
  NotAcceptable                -> "Not acceptable"
  ProxyAuthenticationRequired  -> "Proxy authentication required"
  RequestTimeout               -> "Request timeout"
  Conflict                     -> "Conflict"
  Gone                         -> "Gone"
  LengthRequired               -> "Length required"
  PreconditionFailed           -> "Precondition failed"
  RequestEntityTooLarge        -> "Request entity too large"
  RequestURITooLong            -> "Request URI too long"
  UnsupportedMediaType         -> "Unsupported media type"
  RequestedRangeNotSatisfiable -> "Requested range not satisfiable"
  ExpectationFailed            -> "Expectation failed"
  InternalServerError          -> "Internal server error"
  NotImplemented               -> "Not implemented"
  BadGateway                   -> "Bad gateweay"
  ServiceUnavailable           -> "Service unavailable"
  GatewayTimeout               -> "Gateway timeout"
  HTTPVersionNotSupported      -> "HTTP version not supported"

statusCodeTriplet :: StatusCode -> (Int,Int,Int)
statusCodeTriplet x = case x of
  Continue                     -> (1,0,0)
  SwitchingProtocols           -> (1,0,1)
  OK                           -> (2,0,0)
  Created                      -> (2,0,1)
  Accepted                     -> (2,0,2)
  NonAuthoritativeInformation  -> (2,0,3)  
  NoContent                    -> (2,0,4)
  ResetContent                 -> (2,0,5)
  PartialContent               -> (2,0,6)
  MultipleChoices              -> (3,0,0)
  MovedPermanently             -> (3,0,1)
  Found                        -> (3,0,2)
  SeeOther                     -> (3,0,3)
  NotModified                  -> (3,0,4)
  UseProxy                     -> (3,0,5)
  TemporaryRedirect            -> (3,0,7)
  BadRequest                   -> (4,0,0)
  Unauthorized                 -> (4,0,1)
  PaymentRequired              -> (4,0,2)
  Forbidden                    -> (4,0,3)
  NotFound                     -> (4,0,4)
  MethodNotAllowed             -> (4,0,5)
  NotAcceptable                -> (4,0,6)
  ProxyAuthenticationRequired  -> (4,0,7)
  RequestTimeout               -> (4,0,8)
  Conflict                     -> (4,0,9)
  Gone                         -> (4,1,0)
  LengthRequired               -> (4,1,1)
  PreconditionFailed           -> (4,1,2)  
  RequestEntityTooLarge        -> (4,1,3)
  RequestURITooLong            -> (4,1,4)
  UnsupportedMediaType         -> (4,1,5)
  RequestedRangeNotSatisfiable -> (4,1,6)
  ExpectationFailed            -> (4,1,7)
  InternalServerError          -> (5,0,0)
  NotImplemented               -> (5,0,1)
  BadGateway                   -> (5,0,2)
  ServiceUnavailable           -> (5,0,3)
  GatewayTimeout               -> (5,0,4)
  HTTPVersionNotSupported      -> (5,0,5)
