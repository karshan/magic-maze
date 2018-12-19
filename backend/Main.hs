{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import           Data.Map (Map)
import qualified Data.Map as Map
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets.Connection
import           Protolude
import Prelude (String)
import           System.Environment (getArgs)
import           System.Random (randomIO)

import Types


type ServerState = Map Text Connection

addClient :: MVar ServerState -> Connection -> IO Text
addClient mv conn = do
    name <- toS <$> replicateM 6 (chr . (`mod` 26) <$> randomIO)
    modifyMVar_ mv (return . Map.insert name conn)
    return name

removeClient :: MVar ServerState -> Text -> IO ()
removeClient mv name = modifyMVar_ mv (return . Map.delete name)

broadcast :: MVar ServerState -> Text -> IO ()
broadcast mv msg = do
    connMap <- readMVar mv
    mapM_ (flip sendTextData msg) connMap

main :: IO ()
main = do
    Just port <- (readMaybe <=< head) <$> getArgs
    state <- newMVar Map.empty
    run port (websocketsOr defaultConnectionOptions (wsApp state) backupApp)
    where
        wsApp state pendingConn = do
            conn <- acceptRequest pendingConn
            name <- addClient state conn
            flip finally (removeClient state name) $ do
                forkPingThread conn 30
                forever $ do
                    a <- receiveData conn
                    putText a
                    print $ (eitherDecode (toS a) :: Either String Command)
                    broadcast state a

        redirSlash :: Request -> Request
        redirSlash request = if null (pathInfo request) then request { pathInfo = ["index.html"] } else request

        backupApp :: Application
        backupApp request f =
            staticApp (defaultWebAppSettings "static") (redirSlash request) f
