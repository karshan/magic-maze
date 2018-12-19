module GameLogic where

import Control.Monad.Except
import Data.Array
import Data.Either
import Data.Foldable
import Data.FoldableWithIndex
import Data.Maybe
import Data.Maybe.First
import Data.Monoid
import Data.Newtype
import Data.Tuple
import Effect
import Foreign.Generic
import Isometric
import Prelude
import Signal.DOM
import Tiles
import Types

import Data.Map as Map
import Effect.Console (log)
import GFX as GFX
import Signal.Channel (Channel)
import Signal.Channel (send) as Chan
import Web.Socket.WebSocket as WS

initialState :: GameState
initialState =
    let noWalls = { right: false, down: false }
        downWall = { right: false, down: true }
        rightWall = { right: true, down: false }
        rdWall = { right: true, down: true }
    in {
      maze: initialTile,
      players: Map.fromFoldable [
        Tuple Red (MapPoint { x: 1, y: 1 }),
        Tuple Yellow (MapPoint { x: 2, y: 1 }),
        Tuple Green (MapPoint { x: 1, y: 2 }),
        Tuple Purple (MapPoint { x: 2, y: 2  })
      ],
      dragging: Nothing
    }

moveMapPoint :: MapPoint -> Dir -> MapPoint
moveMapPoint (MapPoint { x,  y }) dir =
  case dir of
    N -> MapPoint { x, y: y - 1 }
    S -> MapPoint { x, y: y + 1 }
    W -> MapPoint { x: x - 1, y }
    E -> MapPoint { x: x + 1, y }

blockedByWall :: Maze -> MapPoint -> MapPoint -> Dir -> Boolean
blockedByWall maze (MapPoint { x: cx, y: cy }) (MapPoint { x: tx, y: ty }) dir =
  case dir of
    N -> any (\y -> maybe true (_.walls.down) $ Map.lookup (MapPoint { x: cx, y }) maze.cells) (ty..(cy - 1))
    S -> any (\y -> maybe true (_.walls.down) $ Map.lookup (MapPoint { x: cx, y }) maze.cells) (cy..(ty - 1))
    W -> any (\x -> maybe true (_.walls.right) $ Map.lookup (MapPoint { x, y: cy }) maze.cells) (tx..(cx - 1))
    E -> any (\x -> maybe true (_.walls.right) $ Map.lookup (MapPoint { x, y: cy }) maze.cells) (cx..(tx - 1))

getDirection :: MapPoint -> MapPoint -> Maybe Dir
getDirection (MapPoint { x: cx, y: cy }) (MapPoint { x: tx, y: ty }) =
  if cx == tx then
    if ty > cy then
      Just S
    else if ty < cy then
      Just N
    else
      Nothing
  else if cy == ty then
    if tx > cx then
      Just E
    else if tx < cx then
      Just W
    else
      Nothing
  else
    Nothing

evalCommand :: Command -> GameState -> GameState
evalCommand (PlayerMove pCol targetPos) gs = maybe gs identity (do
  currentPos <- Map.lookup pCol gs.players
  targetCell <- Map.lookup targetPos gs.maze.cells
  guard (targetCell.special /= (Just STUnwalkable)) (pure unit)
  guard (not $ any (_ == targetPos) gs.players) (pure unit)
  dir <- getDirection currentPos targetPos
  guard (not $ blockedByWall gs.maze currentPos targetPos dir) (pure unit)
  pure $ gs { players = Map.update (const $ Just targetPos) pCol gs.players })

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
dropPlayer :: MouseInputs -> DragState -> Command
dropPlayer i { playerColor, dragPoint } =
  let playerPosition = screenToMap i.dims
                (ScreenPoint $ toPoint i.mousePos - dragPoint + GFX.playerCenterT)
   in PlayerMove playerColor playerPosition

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
          let command = dropPlayer mouseInputs dragState
          in
            mkOut
              (evalCommand command gameState {
                dragging = Nothing 
              }) 
              (Just $ command)

gameLogic :: Inputs -> GameState -> Effect GameState
gameLogic inputs gameState =
  case inputs of
    Mouse mouseInputs -> do
      let { nextGameState, msgToSend } = gameLogicPure mouseInputs gameState
      either (maybe (pure unit) log)
        (\{ ws, m } -> WS.sendString ws m)
        (do
            ws <- note (Just "WebSocket not open") mouseInputs.ws
            m <- note Nothing msgToSend
            pure { ws: ws, m: genericEncodeJSON defaultOptions m })
      pure nextGameState
    ServerMsg mMsg -> do
      -- TODO error logging
      let (decodedMsg :: Maybe Command) =
            hush <<< runExcept <<< genericDecodeJSON defaultOptions =<< mMsg
      log (show decodedMsg)
      maybe (pure gameState)
        (pure <<< flip evalCommand gameState)
        decodedMsg
