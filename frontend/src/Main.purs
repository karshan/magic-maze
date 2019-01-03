module Main where

import Isometric
import Prelude
import Signal.DOM
import Signal.Effect
import Types

import Color (Color, white, rgb, rgba, black)
import DOM (onDOMContentLoaded)
import Data.Array ((..))
import Data.Either (hush)
import Data.Foldable (class Foldable, foldMap)
import Data.FoldableWithIndex (foldWithIndexM, foldMapWithIndex, foldlWithIndex)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber, round, floor)
import Data.Map (Map, member, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, isNothing, isJust)
import Data.Maybe.First (First(..))
import Data.Monoid (guard)
import Data.Newtype (wrap, unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import GFX as GFX
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, drawImage, getCanvasElementById, getContext2D, setCanvasHeight, setCanvasWidth, tryLoadImage, getImageData, ImageData, putImageDataFull, clearRect, canvasElementToImageSource)
import Graphics.Drawing
import Graphics.Drawing as D
import Graphics.Drawing.Font as D
import Signal (Signal, foldp, sampleOn, runSignal, constant, map2, merge)
import Signal.Channel (channel, send, subscribe)
import Signal.WebSocket (create) as WS
import Web.DOM.Document
import Web.HTML
import Web.HTML.Window
import Web.HTML.HTMLDocument
import Unsafe.Coerce -- TODO move to purescript-canvas

import GameLogic
import GFX.Cell (drawCell, drawCellWall, drawCellExplore)

translate' :: Point -> Drawing -> Drawing
translate' { x, y } = translate x y

keycodes :: DirMap Int
keycodes = {
        left: 37,
        up: 38,
        right: 39,
        down: 40
    }

drawPlayer :: DimensionPair -> MapPoint -> Maybe Point -> Point -> CanvasImageSource -> Drawing
drawPlayer offscreenDims playerPosition mDragPoint realMouse canvasImage =
    (maybe
        (GFX.playerCell <<< mapToScreenD offscreenDims playerPosition)
        (\dragPoint -> translate' (realMouse - dragPoint))
        mDragPoint)
    (image canvasImage)

drawEscalator :: DimensionPair -> MapPoint -> MapPoint -> Drawing
drawEscalator offscreenDims mp1 mp2 = 
  let mp2p = unwrap <<< mapToScreen offscreenDims
  in translate 0.0 (tileHalfHeight - 2.0) $ 
       outlined (lineWidth 4.0 <> outlineColor (rgba 120 120 120 1.0)) $
         path $ map mp2p [ mp1, mp2 ]

renderMaze :: Context2D -> { maze :: Maze, offscreenDims :: DimensionPair, assets :: Assets } -> Effect Unit
renderMaze ctx { maze, offscreenDims, assets } = do
  D.render ctx (filled (fillColor (rgba 0 0 0 0.0)) (rectangle 0.0 0.0 (toNumber offscreenDims.w) (toNumber offscreenDims.h)))
  let cells = forAllCells maze (drawCell offscreenDims maze.cells)
  let exploreCells =
        forAllCells maze
          (\x y cell ->
            case cell.special of
                 Just (STExplore col dir) ->
                   drawCellExplore dir offscreenDims x y (maybe mempty image (lookup (AExplore col) assets))
                 _ -> mempty)
  let walls = forAllCells maze (drawCellWall offscreenDims maze.cells)
  let escalators = foldMap (\(Tuple mp1 mp2) -> drawEscalator offscreenDims mp1 mp2) maze.escalators
  D.render ctx (cells <> exploreCells <> walls <> escalators)

renderText :: Number -> Number -> Color -> String -> Drawing
renderText x y c s = D.text (D.font D.monospace 12 mempty) x y (D.fillColor c) s

render :: Context2D -> CanvasElement -> DimensionPair -> DimensionPair -> Assets -> Point -> GameState -> Effect Unit
render ctx offscreenCanvas offscreenDims screenDims assets realMouse gameState = do
  let debugText = renderText 100.0 100.0 white (show gameState.renderOffset)
  -- TODO draw dragging player first, then in descending order by y coordinate
  let players =
        (foldMapWithIndex
          (\asset img ->
              case asset of
                APlayer p ->
                    let mDragPoint = unwrap do
                          ds <- First gameState.dragging
                          guard (ds.playerColor == p) (pure ds.dragPoint)
                    in maybe mempty (\pos -> drawPlayer offscreenDims pos mDragPoint realMouse img) (lookup p gameState.players)
                _ -> mempty)
        assets)
  let r = gameState.renderOffset
  let bg = maybe mempty image (Map.lookup ABackground assets)
  D.render ctx (GFX.background screenDims bg <> (translate (-r.x) (-r.y) $ (image $ canvasElementToImageSource offscreenCanvas) <> players) <> debugText)

resize :: CanvasElement -> DimensionPair -> Effect Unit
resize canvas dims = do
  setCanvasWidth canvas (toNumber dims.w)
  setCanvasHeight canvas (toNumber dims.h)

loadAsset :: AssetName -> String -> Effect (Signal Asset)
loadAsset name url = do
  c <- channel $ Tuple name Nothing
  tryLoadImage url (send c <<< Tuple name)
  pure (subscribe c)

loadAssets :: Map AssetName String -> Effect (Signal Assets)
loadAssets toLoad =
  foldWithIndexM
    (\name accum url -> do
        sig <- loadAsset name url
        pure $ map2 (\(Tuple k v) m -> Map.alter (const v) k m) sig accum)
    (constant (Map.empty))
    toLoad

main :: Effect Unit
main = onDOMContentLoaded do
    frames <- animationFrame
    upKey <- keyPressed keycodes.up
    rightKey <- keyPressed keycodes.right
    downKey <- keyPressed keycodes.down
    leftKey <- keyPressed keycodes.left
    mcanvas <- getCanvasElementById "canvas"
    doc <- toDocument <$> (document =<< window)
    offscreenCanvas <- unsafeCoerce <$> createElement "canvas" doc
    screenDims <- windowDimensions
    let offscreenDims = constant $ { w: floor tileHalfWidth * 2 * 4 * 9, h: floor tileHalfHeight * 2 * 4 * 9 }
    mPos <- mousePos
    mPressed <- mouseButtonPressed MouseLeftButton
    log "creating websocket"
    { ws, serverMsg } <- WS.create "wss://tmp.karshan.me/ws/"
    maybe
        (log "error no canvas")
        (\canvas -> do
            let mouseMove = { offscreenDims: _, mousePos: _, mousePressed: _, ws: _ } <$>
                           offscreenDims <*> mPos <*> mPressed <*> ws
            let arrowKeys = { up: _, right: _, down: _, left: _ } <$> upKey <*> rightKey <*> downKey <*> leftKey
            let inputs = merge (Keyboard <$> arrowKeys) $ merge (Mouse <$> sampleOn mPressed mouseMove) (ServerMsg <$> serverMsg)
            rerenderChan <- channel initialState.maze
            game <- foldEffect (gameLogic rerenderChan) initialState inputs
            let realMousePos = map2 (\g m -> g.renderOffset + toPoint m) game mPos
            ctx <- getContext2D canvas
            offscreenCtx <- getContext2D offscreenCanvas
            runSignal (resize canvas <$> screenDims)
            runSignal (resize offscreenCanvas <$> offscreenDims)
            renderedMaze <- mapEffect (renderMaze offscreenCtx)
            -- TODO show loading screen, wait for all assets to load then render
            assets <- loadAssets $ Map.fromFoldable [
                        Tuple (APlayer Red) "svg/player-red.svg",
                        Tuple (APlayer Yellow) "svg/player-yellow.svg",
                        Tuple (APlayer Green) "svg/player-green.svg",
                        Tuple (APlayer Purple) "svg/player-purple.svg",
                        Tuple (AExplore Red) "svg/explore-red.svg",
                        Tuple (AExplore Yellow) "svg/explore-yellow.svg",
                        Tuple (AExplore Green) "svg/explore-green.svg",
                        Tuple (AExplore Purple) "svg/explore-purple.svg",
                        Tuple ABackground "svg/background.svg"
                      ]
            -- TODO rerenderChan should only change when the maze changes, right now it changes on every server command received
            let renderedMazeSignal = renderedMaze $ { maze: _, offscreenDims: _, assets: _ } <$> subscribe rerenderChan <*> offscreenDims <*> assets
            -- TODO sampleOn animationFrame ?
            runSignal (render ctx offscreenCanvas <$> offscreenDims <*> screenDims <*> assets <*> realMousePos <*> game))
        mcanvas
