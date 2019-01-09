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
import           Network.WebSockets (requestPath)
import           Prelude                        (String)
import           Protolude
import           System.Environment             (getArgs)
import           System.Random                  (randomIO)

import           GameData
import           GameLogic
import           Types
import qualified Data.Text as T

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

main :: IO ()
main = do
    Just port <- (readMaybe <=< head) <$> getArgs
    serverStateMV <- newMVar Map.empty
    run port (websocketsOr defaultConnectionOptions (wsApp serverStateMV) backupApp)
    -- FIXME thread that decrements gameState.timer everysecond after the game starts
    where
        wsApp :: MVar ServerState -> PendingConnection -> IO ()
        wsApp serverStateMV pendingConn = do
            let path = toS $ requestPath (pendingRequest pendingConn)
            roomStateMV <- modifyMVar serverStateMV
                (\serverState ->
                    maybe
                        (do
                            newRoomStateMV <- newMVar (RoomState Map.empty initialState)
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
                    either print
                        (\command -> do
                            print command
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
