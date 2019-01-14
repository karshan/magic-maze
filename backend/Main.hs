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

type ServerState = Map Text (MVar RoomState)
data ConnectionState = ConnectionState {
    _connection :: Connection,
    _allowedDir :: Dir,
    _lastPong   :: UTCTime
  }
data RoomState = RoomState {
    _connections :: Map Text ConnectionState,
    _gameState   :: ServerGameState
  }

makeFieldsNoPrefix ''ConnectionState
makeFieldsNoPrefix ''RoomState

genName :: IO Text
genName = do
    let rand :: [a] -> IO (Maybe a)
        rand l = (l !!) <$> randomRIO (0, length l - 1)
        v = [ "a", "e", "i", "o" , "u", "aa", "ee", "oo" ]
        c = [ "b", "c", "ch", "d", "f", "ph", "g", "h", "gh", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "th", "v", "w", "x", "y", "z" ]
    toS . concat . catMaybes <$> mapM rand [ c, v, c, v, c, v, c ]

-- TODO check if randomName already exists ?
addClient :: MVar RoomState -> PendingConnection -> IO (Text, Dir, Connection)
addClient mv pendingConn = do
    name <- genName
    let updatePongTime :: RoomState -> IO RoomState
        updatePongTime roomState = do
            t <- getCurrentTime
            return $ roomState & connections %~ (Map.adjust (lastPong .~ t) name)
    conn <- acceptRequest (pendingConn { pendingOptions = 
                (pendingOptions pendingConn) 
                { connectionOnPong = modifyMVar_ mv updatePongTime } })
    dir <- modifyMVar mv 
        (\roomState -> do
            let remainingDirs = Set.fromList [ N, E, S, W ] \\ Set.fromList (map (view allowedDir) $ Map.elems $ roomState ^. connections)
            randomDir <- fromMaybe N . ([N, E, S, W] !!) . (`mod` 4) <$> randomIO
            dir <- fromMaybe randomDir . ((Set.toList remainingDirs) !!) . (`mod` length remainingDirs) <$> randomIO
            curTime <- getCurrentTime
            return $ (roomState & connections %~ (Map.insert name (ConnectionState conn dir curTime)), dir))
    return (name, dir, conn)

removeClient :: MVar RoomState -> Text -> IO ()
removeClient mv name = modifyMVar_ mv (return . over connections (Map.delete name))

broadcast :: MVar RoomState -> Text -> IO ()
broadcast mv msg = do
    putText $ "broadcasting " <> msg
    mapM_ (flip sendTextData msg . view connection) . view connections =<< readMVar mv

broadcastExcept :: Text -> MVar RoomState -> Text -> IO ()
broadcastExcept name mv msg = do
    putText $ "broadcasting " <> msg
    mapM_ (flip sendTextData msg . view connection) . Map.filterWithKey (\k _ -> k /= name) . view connections =<< readMVar mv

tickThread :: MVar RoomState -> IO ()
tickThread roomStateMV = forever $ do
    threadDelay 1000000
    modifyMVar_ roomStateMV
        (\roomState ->
            if roomState ^. gameState.L.timer <= 1 then
                return $ roomState & gameState.L.timer .~ 0 & gameState.L.status .~ Lost
            else
                return $ roomState & gameState.L.timer %~ (\x -> x - 1))

broadcastClients :: MVar RoomState -> IO ()
broadcastClients roomStateMV = do
    -- let encodeClients r name = toS . encode . SSetClients . Map.filterWithKey (\k _ -> k /= name) $ (r ^. allowedDirs)
    roomState <- readMVar roomStateMV
    mapM_ (\(n, connState) ->
                sendTextData (connState ^. connection) $
                    encode $ SSetClients $ map (view allowedDir) $ Map.filterWithKey (\k _ -> k /= n) $ roomState ^. connections) 
        (Map.toList $ roomState ^. connections)

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
            (name, allowedDir, conn) <- addClient roomStateMV pendingConn
            sendTextData conn (encode $ SSetAllowedDir allowedDir)
            broadcastClients roomStateMV
            gameStateAtConn <- view gameState <$> readMVar roomStateMV
            sendTextData conn (encode (SSetState gameStateAtConn))
            flip finally 
                (do
                    removeClient roomStateMV name
                    broadcastClients roomStateMV) $ do
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
                                                evalRandIO (evalCommand command allowedDir (roomState ^. gameState))
                                        return (roomState & gameState .~ newGameState, mCommandToSend))
                                maybe (return ()) 
                                    (\commandToSend ->
                                        let brdcst = case commandToSend of
                                                SPlayerMove _ _ -> broadcastExcept name
                                                _ -> broadcast
                                        in brdcst roomStateMV $ toS $ encode commandToSend)
                                    mCommandToSend)
                            eCommand
        redir req = if "." `T.isInfixOf` T.concat (pathInfo req) then req else req { pathInfo = [ "index.html" ] }
        backupApp :: Application
        backupApp request f =
            staticApp ((defaultWebAppSettings "static") { ssMaxAge = MaxAgeSeconds 0 }) (redir request) f
