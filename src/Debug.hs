{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad             (forever)
import Data.Function             (($))
import Data.List                 (head)
import Data.Maybe                (Maybe (Just, Nothing))
import Network.Socket            (AddrInfoFlag (AI_PASSIVE), Socket,
                                  SocketType (Stream), accept, addrAddress,
                                  addrFamily, addrFlags, bind, close,
                                  defaultHints, defaultProtocol, getAddrInfo,
                                  listen, socket, withSocketsDo)
import Network.Socket.ByteString (recv, sendAll)
import System.IO                 (IO, print)

logAndEcho :: Socket -> IO ()
logAndEcho sock               = forever $ do
  (s, _)      <- accept sock
  printAndKickback s
  close s
  where printAndKickback conn = do
          msg <- recv conn 1024
          print msg
          sendAll conn msg

main :: IO ()
main                                                      = withSocketsDo $ do
  addrInfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "79")
  let serverAddr                                          = head addrInfos
  sock      <- socket (addrFamily serverAddr) Stream defaultProtocol
  bind sock (addrAddress serverAddr)
  listen sock 1
  logAndEcho sock
  close sock
