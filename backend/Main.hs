{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE PartialTypeSignatures  #-}
module Main where

import           Control.Lens
import           Control.Lens.TH                (makeFieldsNoPrefix)
import           Control.Monad.Random
import           Data.Aeson
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Set                       ((\\))
import qualified Data.Set                       as Set
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
import           System.Random                  (randomRIO, randomIO)
import           WaiAppStatic.Types

import           GameData
import           GameLogic
import qualified Lenses                         as L
import           Types
import Data.Time.Clock
import Util

-- TODO check if randomName already exists ?
-- TODO remainingDirs more lens foo cleanup
-- TODO last return line, TupleSections ?
addClient :: MVar RoomState -> PendingConnection -> IO (Maybe (Text, Dir), Connection)
addClient mv pendingConn = do
    name <- genName
    let updatePongTime :: RoomState -> IO RoomState
        updatePongTime roomState = do
            t <- getCurrentTime
            return $ roomState & L.connections %~ (Map.adjust (L.lastPong .~ t) name)
    conn <- acceptRequest' pendingConn (modifyMVar_ mv updatePongTime)
    mDir <- modifyMVar mv
        (\roomState ->
            if length (roomState ^. L.connections) >= 4 then
                return (roomState, Nothing)
            else do
                let remainingDirs = Set.fromList [ N, E, S, W ] \\
                      Set.fromList (map (view L.allowedDir) $ roomState ^. L.connections ^.. each)
                dir <- fromMaybe impossible <$> randomList (Set.toList remainingDirs)
                curTime <- getCurrentTime
                let newConnState = ConnectionState conn dir curTime
                return $ (roomState & L.connections %~ (Map.insert name newConnState), Just dir))
    return $ ((\dir -> (name, dir)) <$> mDir, conn)

removeClient :: MVar RoomState -> Text -> IO ()
removeClient mv name = modifyMVar_ mv (return . over L.connections (Map.delete name))

broadcast :: MVar RoomState -> Text -> IO ()
broadcast mv msg = do
    putText $ "broadcasting " <> msg
    mapM_ (flip sendTextData msg . view L.connection) . view L.connections =<< readMVar mv

broadcastExcept :: Text -> MVar RoomState -> Text -> IO ()
broadcastExcept name mv msg = do
    putText $ "broadcasting " <> msg
    mapM_ (flip sendTextData msg . view L.connection) .
        Map.filterWithKey (\k _ -> k /= name) . view L.connections =<< readMVar mv

tickThread :: MVar RoomState -> IO ()
tickThread roomStateMV = forever $ do
    threadDelay 1000000
    modifyMVar_ roomStateMV
        (\roomState ->
            if roomState ^. L.gameState.L.timer <= 1 then
                return $ roomState & L.gameState.L.timer .~ 0 & L.gameState.L.status .~ Lost
            else
                return $ roomState & L.gameState.L.timer %~ (\x -> x - 1))

-- broadcast the list of currently connected clients
broadcastClients :: MVar RoomState -> IO ()
broadcastClients roomStateMV = do
    let msg :: RoomState -> Text -> LByteString
        msg rState name = encode . SSetClients . map (view L.allowedDir) .
            Map.filterWithKey (\k _ -> k /= name) $ (rState ^. L.connections)
    roomState <- readMVar roomStateMV
    mapM_ (\(n, connState) -> sendTextData (connState ^. L.connection) $ msg roomState n)
        (Map.toList $ roomState ^. L.connections)

-- FIXME keepAlive ping, and SetState when client comes to life after being dead
-- FIXME kill rooms after game is won/lost
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
            (mNameDir, conn) <- addClient roomStateMV pendingConn
            maybe
                (sendTextData conn (encode SRoomFull) >> sendClose conn ("" :: Text))
                (\(name, allowedDir) -> do
                    sendTextData conn (encode $ SSetAllowedDir allowedDir)
                    broadcastClients roomStateMV
                    gameStateAtConn <- view L.gameState <$> readMVar roomStateMV
                    sendTextData conn (encode (SSetState gameStateAtConn))
                    (flip finally
                        (do
                            removeClient roomStateMV name
                            broadcastClients roomStateMV) $ do
                        forkPingThread conn 30
                        forever $ do
                            (a :: Text) <- receiveData conn
                            let eCommand = (eitherDecode (toS a) :: Either String C2SCommand)
                            print eCommand
                            status <- view (L.gameState.L.status) <$> readMVar roomStateMV
                            if status == Lost || status == Won then
                                sendClose conn ("" :: Text)
                            else
                                either (const $ return ())
                                    (\command -> do
                                        mCommandToSend <- modifyMVar roomStateMV
                                            (\roomState -> do
                                                (newGameState, mCommandToSend) <-
                                                        evalRandIO (evalCommand command allowedDir (roomState ^. L.gameState))
                                                return (roomState & L.gameState .~ newGameState, mCommandToSend))
                                        maybe (return ())
                                            (\commandToSend ->
                                                let brdcst = case commandToSend of
                                                        SPlayerMove _ _ -> broadcastExcept name
                                                        _ -> broadcast
                                                in brdcst roomStateMV $ toS $ encode commandToSend)
                                            mCommandToSend)
                                    eCommand))
                mNameDir
        redir req = if "." `T.isInfixOf` T.concat (pathInfo req) then req else req { pathInfo = [ "index.html" ] }
        backupApp :: Application
        backupApp request f =
            staticApp ((defaultWebAppSettings "static") { ssMaxAge = MaxAgeSeconds 0 }) (redir request) f
