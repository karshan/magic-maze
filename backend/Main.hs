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
import           Data.CaseInsensitive           as CI (mk)
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
import           Prelude                        (String, lookup)
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
    (mDir, shouldStartGame) <- modifyMVar mv
        (\roomState ->
            if length (roomState ^. L.connections) >= 4 then
                return (roomState, (Nothing, False))
            else do
                let remainingDirs = Set.fromList [ N, E, S, W ] \\
                      Set.fromList (map (view L.allowedDir) $ roomState ^. L.connections ^.. each)
                dir <- fromMaybe impossible <$> randomList (Set.toList remainingDirs)
                curTime <- getCurrentTime
                let newConnState = ConnectionState conn dir curTime
                let shouldStartGame = length (roomState ^. L.connections) == 3 && roomState ^.L.gameState ^. L.status == Waiting
                let startGame x = if shouldStartGame then Started else x
                return $ (roomState & L.connections %~ (Map.insert name newConnState) & L.gameState.L.status %~ startGame, (Just dir, shouldStartGame)))
    if shouldStartGame then do
        void $ forkIO (tickThread mv)
        roomState <- readMVar mv
        broadcast mv (toS $ encode $ SSetState (roomState ^. L.gameState)) -- FIXME s/SSetState/SStarted/g
        return $ ((\dir -> (name, dir)) <$> mDir, conn)
    else
        return $ ((\dir -> (name, dir)) <$> mDir, conn)

removeClient :: MVar RoomState -> Text -> IO Bool -- 0 clients remain
removeClient mv name =
    modifyMVar mv
        (\roomState ->
            return
                (over L.connections (Map.delete name) roomState,
                    length (roomState ^. L.connections) == 1))

broadcast :: MVar RoomState -> Text -> IO ()
broadcast mv msg =
    mapM_ (flip sendTextData msg . view L.connection) . view L.connections =<< readMVar mv

broadcastExcept :: Text -> MVar RoomState -> Text -> IO ()
broadcastExcept name mv msg = do
    mapM_ (flip sendTextData msg . view L.connection) .
        Map.filterWithKey (\k _ -> k /= name) . view L.connections =<< readMVar mv

tickThread :: MVar RoomState -> IO ()
tickThread roomStateMV = do
    threadDelay 1000000
    gameOver <- modifyMVar roomStateMV
        (\roomState ->
            if roomState ^. L.gameState.L.timer <= 1 then
                return (roomState & L.gameState.L.timer .~ 0 & L.gameState.L.status .~ Lost, True)
            else
                return (roomState & L.gameState.L.timer %~ (\x -> x - 1), False))
    if gameOver == False then
        tickThread roomStateMV
    else
        return ()

-- broadcast the list of currently connected clients
broadcastClients :: MVar RoomState -> IO ()
broadcastClients roomStateMV = do
    let msg :: RoomState -> Text -> LByteString
        msg rState name = encode . SSetClients . map (view L.allowedDir) .
            Map.filterWithKey (\k _ -> k /= name) $ (rState ^. L.connections)
    roomState <- readMVar roomStateMV
    mapM_ (\(n, connState) -> sendTextData (connState ^. L.connection) $ msg roomState n)
        (Map.toList $ roomState ^. L.connections)

websocketsOr' opts app backup req sendResponse =
    case websocketsApp opts (app req) req of
        Nothing -> backup req sendResponse
        Just res -> sendResponse res

-- FIXME keepAlive ping, and SetState when client comes to life after being dead
-- FIXME kill rooms after game is won/lost
main :: IO ()
main = do
    Just port <- (readMaybe <=< head) <$> getArgs
    serverStateMV <- newMVar Map.empty
    putText $ "magic-maze-backend listening on " <> show port
    run port (websocketsOr' defaultConnectionOptions (wsApp serverStateMV) (backupApp serverStateMV))

roomInfo :: Text -> RequestHeaders -> MVar RoomState -> IO Text
roomInfo path headers roomStateMV = do
    t <- getCurrentTime
    rs <- readMVar roomStateMV
    return $ "[" <> show t <> "] " <> path <>
        " (" <> show (Prelude.lookup (CI.mk $ toS ("X-Forwarded-For" :: Text)) headers) <> "): " <>
        (show $ length $ rs ^. L.connections) <> " players " <>
        "(" <> show (rs ^. L.gameState ^. L.status) <> ")"

wsApp :: MVar ServerState -> Request -> PendingConnection -> IO ()
wsApp serverStateMV req pendingConn = do
    let path = toS $ requestPath (pendingRequest pendingConn)

    -- Get existing roomStateMV or create a new one
    roomStateMV <- modifyMVar serverStateMV
        (\serverState ->
            maybe
                (do
                    newRoomStateMV <- newMVar (RoomState Map.empty initialState)
                    return (Map.insert path newRoomStateMV serverState, newRoomStateMV))
                (\existingRoomStateMV -> return (serverState, existingRoomStateMV))
                (Map.lookup path serverState))
    (mNameDir, conn) <- addClient roomStateMV pendingConn
    mNameDir & maybe (sendTextData conn (encode SRoomFull) >> sendClose conn ("" :: Text))
        (\(name, allowedDir) -> do
            putText =<< roomInfo path (requestHeaders req) roomStateMV
            sendTextData conn (encode $ SSetAllowedDir allowedDir)
            broadcastClients roomStateMV
            modifyMVar_ roomStateMV (\rs -> return (rs & L.gameState.L.maze.L.wepacq .~ (rs ^. L.gameState.L.status == WeaponsAcquired)))
            sendTextData conn . encode . SSetState =<< view L.gameState <$> readMVar roomStateMV
            (flip finally
                (do
                    allLeft <- removeClient roomStateMV name
                    putText =<< roomInfo path (requestHeaders req) roomStateMV
                    if allLeft then
                        modifyMVar_ serverStateMV (return . Map.delete path)
                    else
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
                        either (const $ return ()) -- TODO force client code refresh?
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

backupApp :: MVar ServerState -> Application
backupApp serverStateMV request f =
    if pathInfo request == [] then do
        ss <- readMVar serverStateMV
        let numRooms = length (Map.keys ss)
        let createButton :: LByteString = "<div style=\"padding-bottom: 1em\"><span id=\"create\" onclick=\"window.location.pathname = '/' + Math.random().toString(36).substring(2, 8)\">create</span></div>"
        let header :: LByteString = "<style>body { font-family: \"Lucida Console\", Monaco, monospace }\n#create { color: blue; cursor: pointer; }</style>" <> createButton
        content :: [Text] <- catMaybes <$> sequence
                (Map.elems (Map.mapWithKey
                    (\r roomStateMV -> do
                        rs <- readMVar roomStateMV
                        if rs ^. L.gameState.L.status == Waiting ||
                           (rs ^. L.gameState.L.status == Started &&
                                length (rs ^. L.connections) < 4) then
                            return $ Just $ "<div><a href=\"" <> r <> "\">" <> r <> "</a> " <> show (length (rs ^. L.connections)) <>  "/4</div>"
                        else
                            return Nothing)
                    ss))
        f $ responseLBS status200 [("Content-Type", "text/html")] (header <> "<div>" <> (show (length content)) <> " open game(s)" <> "</div>" <> toS (T.concat content))
    else
        staticApp ((defaultWebAppSettings "static") { ssMaxAge = MaxAgeSeconds 0 }) (redir request) f
            where
                redir req = if "." `T.isInfixOf` T.concat (pathInfo req) then req else req { pathInfo = [ "index.html" ] }
