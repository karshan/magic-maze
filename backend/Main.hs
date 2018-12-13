{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets.Connection
import           Protolude
import           System.Environment (getArgs)

main :: IO ()
main = do
    Just port <- (readMaybe <=< head) <$> getArgs
    run port (websocketsOr defaultConnectionOptions wsApp backupApp)
    where
        wsApp pendingConn = do
            conn <- acceptRequest pendingConn
            sendTextData conn ("Hello, client!" :: Text)

        backupApp :: Application
        backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"
