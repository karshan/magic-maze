{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
module Main where

import           Control.Lens
import           Control.Lens.TH                (makeFieldsNoPrefix)
import           Control.Monad.Random
import           Data.Aeson
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets.Connection
import           Prelude                        (String)
import           Protolude
import           System.Environment             (getArgs)
import           System.Random                  (randomIO)

import           GameData
import           GameLogic
import           Types

data ServerState = ServerState {
    _connections :: Map Text Connection,
    _gameState   :: ServerGameState
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
    putText $ "broadcasting " <> msg
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
            sendTextData conn (encode (SSetState gs))
            flip finally (removeClient state name) $ do
                forkPingThread conn 30
                forever $ do
                    (a :: Text) <- receiveData conn
                    let eCommand = (eitherDecode (toS a) :: Either String C2SCommand)
                    either print
                        (\command -> do
                            print command
                            mCommandToSend <- modifyMVar state
                                (\curState -> do
                                    (newGameState, mCommandToSend) <-
                                            evalRandIO (evalCommand command (curState ^. gameState))
                                    return (curState & gameState .~ newGameState, mCommandToSend))
                            maybe (return ()) (broadcast state . toS . encode) mCommandToSend)
                        eCommand

        redirSlash :: Request -> Request
        redirSlash request = if null (pathInfo request) then request { pathInfo = ["index.html"] } else request

        backupApp :: Application
        backupApp request f =
            staticApp (defaultWebAppSettings "static") (redirSlash request) f
