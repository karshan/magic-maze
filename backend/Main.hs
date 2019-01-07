{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
module Main where

import           Control.Lens
import           Control.Lens.TH (makeFieldsNoPrefix)
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
import GameData

data ServerState = ServerState {
    _connections :: Map Text Connection,
    _gameState :: ServerGameState
  }

makeFieldsNoPrefix ''ServerState

numClients :: MVar ServerState -> IO Int
numClients = map (length . view connections) . readMVar

addClient :: MVar ServerState -> Connection -> IO Text
addClient mv conn = do
    name <- toS <$> replicateM 6 (chr . (`mod` 26) <$> randomIO)
    modifyMVar_ mv (return . over connections (Map.insert name conn))
    return name

removeClient :: MVar ServerState -> Text -> IO ()
removeClient mv name = modifyMVar_ mv (return . over connections (Map.delete name))

broadcast :: MVar ServerState -> Text -> IO ()
broadcast mv msg = do
    connMap <- view connections <$> readMVar mv
    mapM_ (flip sendTextData msg) connMap

main :: IO ()
main = do
    Just port <- (readMaybe <=< head) <$> getArgs
    state <- newMVar $ ServerState Map.empty initialState
    run port (websocketsOr defaultConnectionOptions (wsApp state) backupApp)
    where
        wsApp state pendingConn = do
            conn <- acceptRequest pendingConn
            name <- addClient state conn
            gs <- view gameState <$> readMVar state
            sendTextData conn (encode (SetState gs))
            flip finally (removeClient state name) $ do
                forkPingThread conn 30
                forever $ do
                    a <- receiveData conn
                    let command = (eitherDecode (toS a) :: Either String Command)
                    case command of
                        (Right (SetState s)) -> putText "updated gameState" >> modifyMVar_ state (\curs -> return (curs & gameState .~ s))
                        _ -> print command
                    n <- numClients state
                    putText $ "numClients: " <> show n
                    broadcast state a

        redirSlash :: Request -> Request
        redirSlash request = if null (pathInfo request) then request { pathInfo = ["index.html"] } else request

        backupApp :: Application
        backupApp request f =
            staticApp (defaultWebAppSettings "static") (redirSlash request) f
