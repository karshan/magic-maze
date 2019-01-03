module GameLogic where

import Control.Monad.Except
import Control.Monad.State
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
import Graphics.Drawing
import Isometric
import Prelude
import Signal.DOM
import Tiles
import Types

import Data.Map as Map
import Effect.Console (log)
import GFX as GFX
import GFX.Cell (evalExploreBBox)
import Signal.Channel (Channel)
import Signal.Channel (send) as Chan
import Web.Socket.WebSocket as WS

initialState :: GameState
initialState = {
      maze: initialTile,
      players: Map.fromFoldable [
        Tuple Red (MapPoint { x: 1, y: 1 }),
        Tuple Yellow (MapPoint { x: 2, y: 1 }),
        Tuple Green (MapPoint { x: 1, y: 2 }),
        Tuple Purple (MapPoint { x: 2, y: 2  })
      ],
      dragging: Nothing,
      renderOffset: { x: 1715.0, y: 840.0 } -- TODO calculate from offscreenDims
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
  -- FIXME guard (not $ blockedByPlayer gs.maze gs.players currentPos targetPos dir)
  pure $ gs { players = Map.update (const $ Just targetPos) pCol gs.players })
evalCommand (Explore mp dir) gs =
  maybe gs (gs { maze = _ }) $ mergeTiles gs.maze 0 mp dir

-- TODO evalBBox in descending order of player y coordinate
maybeStartDrag :: RealMouseInputs -> PlayerPositions -> Maybe DragState
maybeStartDrag i players =
  unwrap $
   foldlWithIndex
     (\playerColor accum position ->
         accum <>
           map ({ playerColor: playerColor, dragPoint: _ })
             (First $ evalBBox (GFX.playerBBox i.offscreenDims position) (ScreenPoint i.realMousePos)))
     (First Nothing)
     players

dropPlayer :: RealMouseInputs -> DragState -> Command
dropPlayer i { playerColor, dragPoint } =
  let playerPosition = screenToMap i.offscreenDims
                (ScreenPoint $ i.realMousePos - dragPoint + GFX.playerCenterT)
   in PlayerMove playerColor playerPosition

data DragCommand =
    StartDrag
  | EndDrag DragState

-- FIXME explore and drag can occur on the same mouse press
-- only one should occur on one mousepress
gameLogicState :: MouseInputs -> State GameState (Maybe Command)
gameLogicState mouseInputs = do
  renderOffset <- _.renderOffset <$> get
  let realMouseI = { offscreenDims: mouseInputs.offscreenDims, ws: mouseInputs.ws, mousePressed: mouseInputs.mousePressed, realMousePos: toPoint mouseInputs.mousePos + renderOffset }
  explore <- handleExplore realMouseI
  drag <- handleDrag realMouseI
  pure $ unwrap $ First drag <> First explore

handleExplore :: RealMouseInputs -> State GameState (Maybe Command)
handleExplore mouseInputs =
  if mouseInputs.mousePressed then do
    gameState <- get 
    pure $ unwrap $ forAllCells gameState.maze 
              (\x y cell ->
                  case cell.special of
                       (Just (STExplore color dir)) ->
                          -- FIXME only expore if neighboring cell is empty
                          if Map.lookup color gameState.players == Just (MapPoint { x, y }) then
                            let mp = MapPoint { x, y }
                                m = First $ evalExploreBBox mouseInputs.offscreenDims dir mp
                                      (ScreenPoint mouseInputs.realMousePos)
                            in const (Explore mp dir) <$> m
                          else
                            mempty
                       _ -> mempty)
  else
    pure Nothing

handleDrag :: RealMouseInputs -> State GameState (Maybe Command)
handleDrag mouseInputs = do
  gameState <- get
  let dragCommand = unwrap $ (do
        dragState <- First gameState.dragging
        guard (not mouseInputs.mousePressed) (pure (EndDrag dragState))) <>
        guard (isNothing gameState.dragging && mouseInputs.mousePressed) (pure StartDrag)
  case dragCommand of
        Nothing -> pure Nothing
        Just StartDrag -> do
          put (gameState { dragging = maybeStartDrag mouseInputs gameState.players })
          pure Nothing
        Just (EndDrag dragState) -> do
          let command = dropPlayer mouseInputs dragState
          put (evalCommand command gameState {
                dragging = Nothing 
              }) 
          pure (Just command)

gameLogic :: Channel Maze -> Inputs -> GameState -> Effect GameState
gameLogic rerenderChan inputs gameState =
  case inputs of
    Mouse mouseInputs -> do
      let (Tuple msgToSend nextGameState) = runState (gameLogicState mouseInputs) gameState
      either (maybe (pure unit) log)
        (\{ ws, m } -> WS.sendString ws m)
        (do
            ws <- note (Just "WebSocket not open") mouseInputs.ws
            m <- note Nothing msgToSend
            pure { ws: ws, m: genericEncodeJSON defaultOptions m })
      pure nextGameState
    Keyboard arrowKeys -> do
      let mul = 5.0
          xLeft = if arrowKeys.left then (-1.0) else 0.0
          xRight = if arrowKeys.right then 1.0 else 0.0
          yUp = if arrowKeys.up then (-1.0) else 0.0
          yDown = if arrowKeys.down then 1.0 else 0.0
          cx = gameState.renderOffset.x
          cy = gameState.renderOffset.y
      pure $ gameState { renderOffset = { x: cx + mul * (xLeft + xRight), y: cy + mul * (yUp + yDown) } }
    ServerMsg mMsg -> do
      -- TODO error logging
      let (decodedMsg :: Maybe Command) =
            hush <<< runExcept <<< genericDecodeJSON defaultOptions =<< mMsg
      log (show decodedMsg)
      maybe (pure gameState)
        (\cmd ->
            let newState = evalCommand cmd gameState
            in Chan.send rerenderChan newState.maze *> pure newState)
        decodedMsg
