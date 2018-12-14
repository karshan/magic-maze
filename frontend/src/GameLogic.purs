module GameLogic where

import Prelude
import Data.Argonaut
import Data.Either
import Data.FoldableWithIndex
import Data.Map as Map
import Data.Maybe
import Data.Maybe.First
import Data.Monoid
import Data.Newtype
import Data.Tuple
import Effect
import Effect.Console (log)
import Signal.Channel (Channel)
import Signal.Channel (send) as Chan
import Signal.DOM
import Web.Socket.WebSocket as WS

import Types
import Isometric
import GFX as GFX

initialState :: GameState
initialState =
    let noWalls = { right: false, down: false }
        downWall = { right: false, down: true }
        rightWall = { right: true, down: false }
        rdWall = { right: true, down: true }
    in
        {
            maze: {
                cells: Map.fromFoldable [
                    Tuple (MapPoint { x: 0, y: 0 }) { walls: rightWall, walkable: true },
                    Tuple (MapPoint { x: 1, y: 0 }) { walls: noWalls, walkable: true },
                    Tuple (MapPoint { x: 2, y: 0 }) { walls: downWall, walkable: true },
                    Tuple (MapPoint { x: 3, y: 0 }) { walls: rightWall, walkable: true },
                    Tuple (MapPoint { x: 0, y: 1 }) { walls: rightWall, walkable: true },
                    Tuple (MapPoint { x: 1, y: 1 }) { walls: noWalls, walkable: true },
                    Tuple (MapPoint { x: 2, y: 1 }) { walls: rightWall, walkable: true },
                    Tuple (MapPoint { x: 3, y: 1 }) { walls: rightWall, walkable: true },
                    Tuple (MapPoint { x: 0, y: 2 }) { walls: noWalls, walkable: true },
                    Tuple (MapPoint { x: 1, y: 2 }) { walls: downWall, walkable: true },
                    Tuple (MapPoint { x: 2, y: 2 }) { walls: rdWall, walkable: true },
                    Tuple (MapPoint { x: 3, y: 2 }) { walls: noWalls, walkable: true },
                    Tuple (MapPoint { x: 0, y: 3 }) { walls: downWall, walkable: true },
                    Tuple (MapPoint { x: 1, y: 3 }) { walls: noWalls, walkable: true },
                    Tuple (MapPoint { x: 2, y: 3 }) { walls: rdWall, walkable: true },
                    Tuple (MapPoint { x: 3, y: 3 }) { walls: noWalls, walkable: false }
                ],
                borders: {
                    left: 0,
                    up: 0,
                    right: 3,
                    down: 3
                }
            },
            players: Map.fromFoldable [
                Tuple Red (MapPoint { x: 1, y: 1 }),
                Tuple Yellow (MapPoint { x: 2, y: 1 }),
                Tuple Green (MapPoint { x: 1, y: 2 }),
                Tuple Purple (MapPoint { x: 2, y: 2  })
            ],
            dragging: Nothing
        }

maybeStartDrag :: MouseInputs -> PlayerPositions -> Maybe DragState
maybeStartDrag i players =
  unwrap $
   foldlWithIndex
     (\playerColor accum position ->
         accum <>
           map ({ playerColor: playerColor, dragPoint: _ })
             (First $ evalBBox (GFX.playerBBox i.dims position) (toScreenPoint i.mousePos)))
     (First Nothing)
     players

-- TODO data ClientToServerCommand = PlayerMove _ _ | ...
dropPlayer :: MouseInputs -> DragState -> { playerColor :: PlayerColor, playerPosition :: MapPoint }
dropPlayer i { playerColor, dragPoint } =
  let playerPosition = screenToMap i.dims
                (ScreenPoint $ toPoint i.mousePos - dragPoint + GFX.playerCenterT)
   in { playerColor, playerPosition }

data DragCommand =
    StartDrag
  | EndDrag DragState

gameLogicPure :: MouseInputs -> GameState -> { nextGameState :: GameState, msgToSend :: Maybe Command }
gameLogicPure mouseInputs gameState =
  let dragCommand = unwrap $ (do
        dragState <- First gameState.dragging
        guard (not mouseInputs.mousePressed) (pure (EndDrag dragState))) <>
        guard (isNothing gameState.dragging && mouseInputs.mousePressed) (pure StartDrag)
      mkOut = { nextGameState: _, msgToSend: _ }
  in case dragCommand of
        Nothing -> mkOut gameState Nothing
        Just StartDrag -> mkOut (gameState { dragging = maybeStartDrag mouseInputs gameState.players }) Nothing
        Just (EndDrag dragState) ->
          let { playerColor, playerPosition } = dropPlayer mouseInputs dragState
          in
            mkOut
              (gameState { 
                players = Map.update (const $ Just playerPosition) playerColor gameState.players,
                dragging = Nothing 
              }) 
              (Just $ PlayerMove playerColor playerPosition)

gameLogic :: Inputs -> GameState -> Effect GameState
gameLogic inputs gameState =
  case inputs of
    Mouse mouseInputs -> do
      let { nextGameState, msgToSend } = gameLogicPure mouseInputs gameState
      either log
        (\{ ws, m } -> WS.sendString ws m)
        (do
            ws <- note "WebSocket not open" mouseInputs.ws
            let m = stringify (encodeJson msgToSend)
            pure { ws, m })
      pure nextGameState
    ServerMsg msg -> log (show msg) *> pure gameState
