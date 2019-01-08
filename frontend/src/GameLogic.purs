module GameLogic where

import Control.Monad.Except (runExcept)
import Control.Monad.State (State, get, put, runState)
import Data.Array ((..), (!!), deleteAt)
import Data.Either (either, note)
import Data.Foldable (any, foldMap, null)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Lens ((^.))
import Data.Map as Map
import Data.Maybe (Maybe (..), fromMaybe, isNothing, maybe)
import Data.Maybe.First (First (..))
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Tuple (Tuple (..))
import Effect (Effect)
import Effect.Console (log)
import Foreign (F, ForeignError (..), fail, renderForeignError)
import Foreign.Generic (defaultOptions, genericDecodeJSON, genericEncodeJSON)
import GFX as GFX
import GFX.Cell (evalExploreBBox)
import Graphics.Drawing (Point)
import Isometric (evalBBox, mapToScreen, screenToMap)
import Prelude
import Signal.Channel (Channel)
import Signal.Channel (send) as Chan
import Signal.DOM (DimensionPair)
import Tiles (mergeTiles)
import Types (C2SCommand(..), Cell, Dir(..), DirMap(..), DragState, Escalator(..), GameState, Inputs(..), MapPoint(..), Maze(..), MouseInputs, PlayerPositions, RealMouseInputs, S2CCommand(..), ScreenPoint(..), SpecialTile(..), borders, cells, down, escalators, forAllCells, right, special, toPoint, walls)
import Web.Socket.WebSocket as WS
import Web.UIEvent.WheelEvent (deltaX, deltaY)

initialState :: GameState
initialState = {
      maze: Maze { cells: Map.empty, borders: DirMap { left: 0, right: 0, up: 0, down: 0 }, escalators: [] },
      tiles: [],
      players: Map.empty,
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
    N -> any (\y -> maybe true (\(c :: Cell) -> c^.walls^.down) $ Map.lookup (MapPoint { x: cx, y }) (maze^.cells))
            (ty..(cy - 1))
    S -> any (\y -> maybe true (\(c :: Cell) -> c^.walls^.down) $ Map.lookup (MapPoint { x: cx, y }) (maze^.cells))
            (cy..(ty - 1))
    W -> any (\x -> maybe true (\(c :: Cell) -> c^.walls^.right) $ Map.lookup (MapPoint { x, y: cy }) (maze^.cells))
            (tx..(cx - 1))
    E -> any (\x -> maybe true (\(c :: Cell) -> c^.walls^.right) $ Map.lookup (MapPoint { x, y: cy }) (maze^.cells))
            (cx..(tx - 1))

blockedByPlayer :: Maze -> PlayerPositions -> MapPoint -> MapPoint -> Dir -> Boolean
blockedByPlayer maze players (MapPoint { x: cx, y: cy }) (MapPoint { x: tx, y: ty }) dir =
  case dir of
    N -> any (\y -> not $ null $ Map.filter (_ == MapPoint { x: cx, y }) players) (ty..(cy - 1))
    S -> any (\y -> not $ null $ Map.filter (_ == MapPoint { x: cx, y }) players) ((cy + 1)..ty)
    W -> any (\x -> not $ null $ Map.filter (_ == MapPoint { x, y: cy }) players) (tx..(cx - 1))
    E -> any (\x -> not $ null $ Map.filter (_ == MapPoint { x, y: cy }) players) ((cx + 1)..tx)

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

isEscalator :: Array Escalator -> MapPoint -> MapPoint -> Boolean
isEscalator escalators mp1 mp2 = Set.member (Set.fromFoldable [ mp1, mp2 ]) $
  Set.map (\(Escalator a b) -> Set.fromFoldable [ a, b ]) (Set.fromFoldable escalators)

evalClientCommand :: C2SCommand -> GameState -> GameState
evalClientCommand (CPlayerMove pCol _ targetPos) gs = maybe gs identity $ do
  currentPos <- Map.lookup pCol gs.players
  targetCell <- Map.lookup targetPos (gs.maze^.cells)
  guard (targetCell^.special /= (Just STUnwalkable)) (pure unit)
  guard (not $ any (_ == targetPos) gs.players) (pure unit)
  if isEscalator (gs.maze^.escalators) currentPos targetPos || targetCell^.special == Just (STWarp pCol) then
    pure $ gs { players = Map.update (const $ Just targetPos) pCol gs.players }
    else do
      dir <- getDirection currentPos targetPos
      guard (not $ blockedByWall gs.maze currentPos targetPos dir) (pure unit)
      guard (not $ blockedByPlayer gs.maze gs.players currentPos targetPos dir) (pure unit)
      pure $ gs { players = Map.update (const $ Just targetPos) pCol gs.players }
evalClientCommand (CExplore mp dir) gs = gs

-- FIXME implement
evalServerCommand :: S2CCommand -> GameState -> GameState
evalServerCommand (SExplore nextTile mp dir) gs =
  maybe gs (gs { maze = _, tiles = fromMaybe [] (deleteAt nextTile gs.tiles) })
    ((gs.tiles !! nextTile) >>= (\newTile -> mergeTiles gs.maze newTile mp dir))
evalServerCommand _ gs = gs

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

dropPlayer :: PlayerPositions -> RealMouseInputs -> DragState -> Maybe C2SCommand
dropPlayer players i { playerColor, dragPoint } = do
  let targetPos = screenToMap i.offscreenDims
                (ScreenPoint $ i.realMousePos - dragPoint + GFX.playerCenterT)
  curPos <- Map.lookup playerColor players
  if curPos == targetPos then
    Nothing
   else
    Just $ CPlayerMove playerColor curPos targetPos

data DragCommand =
    StartDrag
  | EndDrag DragState

-- TODO explore and drag can occur on the same mouse press
-- only one should occur on one mousepress
gameLogicState :: MouseInputs -> State GameState (Maybe C2SCommand)
gameLogicState mouseInputs = do
  renderOffset <- _.renderOffset <$> get
  let realMouseI = { offscreenDims: mouseInputs.offscreenDims, ws: mouseInputs.ws, mousePressed: mouseInputs.mousePressed, realMousePos: toPoint mouseInputs.mousePos + renderOffset }
  explore <- handleExplore realMouseI
  drag <- handleDrag realMouseI
  pure $ unwrap $ First drag <> First explore

handleExplore :: RealMouseInputs -> State GameState (Maybe C2SCommand)
handleExplore mouseInputs =
  if mouseInputs.mousePressed then do
    gameState <- get
    let mCommand = unwrap $ forAllCells gameState.maze
              (\x y cell ->
                  case cell^.special of
                       (Just (STExplore color dir)) ->
                          -- TODO only explore if neighboring cell is empty (actually exploring is still prevented by mergeTiles)
                          if Map.lookup color gameState.players == Just (MapPoint { x, y }) then
                            let mp = MapPoint { x, y }
                                m = First $ evalExploreBBox mouseInputs.offscreenDims dir mp
                                      (ScreenPoint mouseInputs.realMousePos)
                            in const (CExplore mp dir) <$> m
                          else
                            mempty
                       _ -> mempty)
    maybe (pure unit) (\command -> put (evalClientCommand command gameState)) mCommand
    pure mCommand
  else
    pure Nothing

handleDrag :: RealMouseInputs -> State GameState (Maybe C2SCommand)
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
          let mCommand = dropPlayer gameState.players mouseInputs dragState
          maybe
            (put (gameState { dragging = Nothing }) *> pure Nothing)
            (\command ->
                put (evalClientCommand command gameState {
                      dragging = Nothing
                    }) *> pure (Just command))
            mCommand

clipRenderOffset :: DimensionPair -> DirMap Int -> Point -> Point
clipRenderOffset offscreenDims (DirMap { up, down, left, right }) { x: curX, y: curY } =
  let mp x y = MapPoint { x, y }
      clip a lower upper = if a < lower then lower else if a > upper then upper else a
      sLeft = _.x $ unwrap $ mapToScreen offscreenDims (mp left down)
      sRight = _.x $ unwrap $ mapToScreen offscreenDims (mp right up)
      sUp = _.y $ unwrap $ mapToScreen offscreenDims (mp left up)
      sDown = _.y $ unwrap $ mapToScreen offscreenDims (mp right down)
   in { x: clip curX (sLeft - 500.0) (sRight - 100.0), y: clip curY (sUp - 500.0) (sDown - 100.0) }

gameLogic :: Channel Maze -> Inputs -> GameState -> Effect GameState
gameLogic rerenderChan inputs gameState = do
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
          wx = fromMaybe 0.0 (deltaX <$> arrowKeys.mouseWheel)
          wy = fromMaybe 0.0 (deltaY <$> arrowKeys.mouseWheel)
          -- FIXME prevent scrolling too far away from the existing maze.
      pure $ gameState { renderOffset = clipRenderOffset arrowKeys.offscreenDims (gameState.maze^.borders)
                { x: cx + mul * (xLeft + xRight) + wx, y: cy + mul * (yUp + yDown) + wy } }
    ServerMsg mMsg -> do
      -- TODO error logging
      let (decodedMsg :: F S2CCommand) =
            genericDecodeJSON defaultOptions =<< (maybe (fail (ForeignError "nothing")) pure mMsg)
      -- log (show decodedMsg)
      either (\e -> log (foldMap renderForeignError e) *> pure gameState)
        (\cmd ->
            let newState = evalServerCommand cmd gameState
            in Chan.send rerenderChan newState.maze *> pure newState)
        (runExcept decodedMsg)
