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
import qualified Data.Text                      as T
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets             (requestPath)
import           Network.WebSockets.Connection
import           Prelude                        (String)
import           Protolude
import           System.Environment             (getArgs)
import           System.Random                  (randomIO)

import           GameData
import           GameLogic
import qualified Lenses                         as L
import           Types

type ServerState = Map Text (MVar RoomState)
data RoomState = RoomState {
    _connections :: Map Text Connection,
    _gameState   :: ServerGameState
  }

makeFieldsNoPrefix ''RoomState

-- TODO check if randomName already exists ?
addClient :: MVar RoomState -> Connection -> IO Text
addClient mv conn = do
    name <- toS <$> replicateM 6 (chr . (`mod` 26) <$> randomIO)
    modifyMVar_ mv (return . over connections (Map.insert name conn))
    return name

removeClient :: MVar RoomState -> Text -> IO ()
removeClient mv name = modifyMVar_ mv (return . over connections (Map.delete name))

broadcast :: MVar RoomState -> Text -> IO ()
broadcast mv msg = do
    putText $ "broadcasting " <> msg
    mapM_ (flip sendTextData msg) . view connections =<< readMVar mv

tickThread :: MVar RoomState -> IO ()
tickThread roomStateMV = forever $ do
    threadDelay 1000000
    modifyMVar_ roomStateMV
        (\roomState ->
            if roomState ^. gameState.L.timer <= 1 then
                return $ roomState & gameState.L.timer .~ 0 & gameState.L.status .~ Lost
            else
                return $ roomState & gameState.L.timer %~ (\x -> x - 1))

-- FIXME keepAlive ping, and SetState when client comes to life after being dead
main :: IO ()
main = do
    Just port <- (readMaybe <=< head) <$> getArgs
    serverStateMV <- newMVar Map.empty
    run port (websocketsOr defaultConnectionOptions (wsApp serverStateMV) backupApp)
    where
        wsApp :: MVar ServerState -> PendingConnection -> IO ()
        wsApp serverStateMV pendingConn = do
            let path = toS $ requestPath (pendingRequest pendingConn)
            roomStateMV <- modifyMVar serverStateMV
                (\serverState ->
                    maybe
                        (do
                            newRoomStateMV <- newMVar (RoomState Map.empty initialState)
                            void $ forkIO (tickThread newRoomStateMV)
                            return (Map.insert path newRoomStateMV serverState, newRoomStateMV))
                        (\existingRoomStateMV -> return (serverState, existingRoomStateMV))
                        (Map.lookup path serverState))
            conn <- acceptRequest pendingConn
            name <- addClient roomStateMV conn
            gameStateAtConn <- view gameState <$> readMVar roomStateMV
            sendTextData conn (encode (SSetState gameStateAtConn))
            flip finally (removeClient roomStateMV name) $ do
                forkPingThread conn 30
                forever $ do
                    (a :: Text) <- receiveData conn
                    let eCommand = (eitherDecode (toS a) :: Either String C2SCommand)
                    print eCommand
                    status <- view (gameState.L.status) <$> readMVar roomStateMV
                    if status == Lost || status == Won then
                        return ()
                    else
                        either (const $ return ())
                            (\command -> do
                                mCommandToSend <- modifyMVar roomStateMV
                                    (\roomState -> do
                                        (newGameState, mCommandToSend) <-
                                                evalRandIO (evalCommand command (roomState ^. gameState))
                                        return (roomState & gameState .~ newGameState, mCommandToSend))
                                maybe (return ()) (broadcast roomStateMV . toS . encode) mCommandToSend)
                            eCommand
        redir req = if "." `T.isInfixOf` T.concat (pathInfo req) then req else req { pathInfo = [ "index.html" ] }
        backupApp :: Application
        backupApp request f =
            staticApp (defaultWebAppSettings "static") (redir request) f
