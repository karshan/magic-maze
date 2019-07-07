{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
module Util where

import Protolude
import Prelude (error)
import System.Random (randomRIO, randomIO)
import Network.WebSockets.Connection (acceptRequest, PendingConnection(pendingOptions), ConnectionOptions(connectionOnPong), Connection)

impossible = error "impossible"

(!!) :: [a] -> Int -> Maybe a
[] !! _ = Nothing
(x:xs) !! 0 = Just x
(x:xs) !! n = xs !! (n - 1)

randomList :: [a] -> IO (Maybe a)
randomList xs = (xs !!) . (`mod` length xs) <$> randomIO

genName :: IO Text
genName = do
    let rand :: [a] -> IO (Maybe a)
        rand l = (l !!) <$> randomRIO (0, length l - 1)
        v = [ "a", "e", "i", "o" , "u", "aa", "ee", "oo" ]
        c = [ "b", "c", "ch", "d", "f", "ph", "g", "h", "gh", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "th", "v", "w", "x", "y", "z" ]
    toS . concat . catMaybes <$> mapM rand [ c, v, c, v, c, v, c ]

-- Accept WebSocket request with custom onPong handler
acceptRequest' :: PendingConnection -> IO () -> IO Connection
acceptRequest' pendingConn onPong =
    acceptRequest $
        pendingConn { 
            pendingOptions = (pendingOptions pendingConn) {
                connectionOnPong = onPong
            }
        }
