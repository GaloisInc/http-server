--------------------------------------------------------------------------------
-- |
-- Module      : Network.HTTP.Server.Utils
-- Copyright   : (c) Galois, Inc. 2007, 2008
-- License     : BSD3
--
-- Maintainer  : diatcki@galois.com
-- Stability   : provisional
-- Portability :
--

module Network.HTTP.Server.Utils where

import Network.Socket
import Data.Word
import Data.Bits
#ifdef _OS_UNIX
import Numeric
#endif

byte :: (Bits a, Integral a) => a -> Int -> Word8
byte w n = fromIntegral (w `shiftR` (n * 8))

word :: (Bits a, Integral a) => a -> Int -> Word16
word w n = fromIntegral (w `shiftR` (n * 16))

ppHostAddress :: HostAddress -> ShowS
ppHostAddress w = shows (byte w 0) . showChar '.' .
                  shows (byte w 1) . showChar '.' .
                  shows (byte w 2) . showChar '.' .
                  shows (byte w 3)

#if _OS_UNIX
-- XXX: Are the words in the correct order?
ppHostAddress6 :: HostAddress6 -> ShowS
ppHostAddress6 (w1,w2,w3,w4) =
  showChar '[' . showHex (word w1 0) .
  showChar ':' . showHex (word w1 1) .
  showChar ':' . showHex (word w2 0) .
  showChar ':' . showHex (word w2 1) .
  showChar ':' . showHex (word w3 0) .
  showChar ':' . showHex (word w3 1) .
  showChar ':' . showHex (word w4 0) .
  showChar ':' . showHex (word w4 1) .
  showChar ']'
#endif


ppSockAddr :: SockAddr -> ShowS
ppSockAddr (SockAddrInet port addr) = ppHostAddress addr
                                    . showChar ':' . shows port
#ifdef _OS_UNIX
ppSockAddr (SockAddrInet6 port _ addr _) =
  ppHostAddress6 addr . showChar ':' . shows port
ppSockAddr (SockAddrUnix sock) = showString "unix/" . showString sock
#endif
