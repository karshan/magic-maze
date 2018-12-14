{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Application.Static
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
            forkPingThread conn 30
            sendTextData conn ("Hello, client!" :: Text)
            forever $ do
                a <- receiveData conn
                putText a

        redirSlash :: Request -> Request
        redirSlash request = if null (pathInfo request) then request { pathInfo = ["index.html"] } else request

        backupApp :: Application
        backupApp request f =
            staticApp (defaultWebAppSettings "static") (redirSlash request) f
