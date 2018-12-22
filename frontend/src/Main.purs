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
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, drawImage, getCanvasElementById, getContext2D, setCanvasHeight, setCanvasWidth, tryLoadImage, getImageData, ImageData, putImageData)
import Graphics.Drawing (translate, rectangle, filled, fillColor, Drawing, image, Point)
import Graphics.Drawing as D
import Graphics.Drawing.Font as D
import Signal (Signal, foldp, sampleOn, runSignal, constant, map2, merge)
import Signal.Channel (channel, send, subscribe)
import Signal.WebSocket (create) as WS

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

drawPlayer :: DimensionPair -> MapPoint -> Maybe Point -> CoordinatePair -> CanvasImageSource -> Drawing
drawPlayer dims playerPosition mDragPoint mouse canvasImage =
    (maybe
        (GFX.playerCell <<< mapToScreenD dims playerPosition)
        (\dragPoint -> translate' (toPoint mouse - dragPoint))
        mDragPoint)
    (image canvasImage)

-- TODO Render in large offscreen canvas then never re-render until maze changes.
--      Then we don't need to clear the background, it will be transparent by default.
renderMaze :: Context2D -> { maze :: Maze, dims :: DimensionPair, assets :: Assets } -> Effect ImageData
renderMaze ctx { maze, dims, assets } = do
  D.render ctx (filled (fillColor (rgba 0 0 0 0.0)) (rectangle 0.0 0.0 (toNumber dims.w) (toNumber dims.h)))
  let cells = forAllCells maze (drawCell dims maze.cells)
  let exploreCells =
        forAllCells maze
          (\x y cell ->
            case cell.special of
                 Just (STExplore col dir) ->
                   drawCellExplore dir dims x y (maybe mempty image (lookup (AExplore col) assets))
                 _ -> mempty)
  let walls = forAllCells maze (drawCellWall dims maze.cells)
  let bg = maybe mempty image (lookup ABackground assets)
  D.render ctx (GFX.background dims bg <> cells <> exploreCells <> walls)
  getImageData ctx 0.0 0.0 (toNumber dims.w) (toNumber dims.h)

renderText :: Number -> Number -> Color -> String -> Drawing
renderText x y c s = D.text (D.font D.monospace 12 mempty) x y (D.fillColor c) s

render :: Context2D -> DimensionPair -> Assets -> CoordinatePair -> ImageData -> GameState -> Effect Unit
render ctx dims assets mouse renderedMaze gameState = do
  let highlight = screenToMap dims (toScreenPoint mouse)
  let debugText = renderText 100.0 100.0 white (show $ gameState.maze.borders)
  -- TODO draw dragging player first, then in descending order by y coordinate
  let players =
        (foldMapWithIndex
          (\asset img ->
              case asset of
                APlayer p ->
                    let mDragPoint = unwrap do
                          ds <- First gameState.dragging
                          guard (ds.playerColor == p) (pure ds.dragPoint)
                    in maybe mempty (\pos -> drawPlayer dims pos mDragPoint mouse img) (lookup p gameState.players)
                _ -> mempty)
        assets)
  putImageData ctx renderedMaze 0.0 0.0
  D.render ctx (players <> debugText)

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
    leftInputs <- keyPressed keycodes.left
    rightInputs <- keyPressed keycodes.right
    upInputs <- keyPressed keycodes.up
    downInputs <- keyPressed keycodes.down
    mcanvas <- getCanvasElementById "canvas"
    dims <- windowDimensions
    mPos <- mousePos
    mPressed <- mouseButtonPressed MouseLeftButton
    log "creating websocket"
    { ws, serverMsg } <- WS.create "ws://localhost:3030"
    maybe
        (log "error no canvas")
        (\canvas -> do
            let mouseMove = { dims: _, mousePos: _, mousePressed: _, ws: _ } <$>
                           dims <*> mPos <*> mPressed <*> ws
            let inputs = merge (Mouse <$> sampleOn mPressed mouseMove) (ServerMsg <$> serverMsg)
            game <- foldEffect gameLogic initialState inputs
            ctx <- getContext2D canvas
            runSignal (resize canvas <$> dims)
            renderedMaze <- mapEffect (renderMaze ctx)
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
            -- FIXME game sampleOn ExploreCommand
            let renderedMazeSignal = renderedMaze $ { maze: _, dims: _, assets: _ } <$> (_.maze <$> game) <*> dims <*> assets
            -- TODO sampleOn animationFrame ?
            runSignal (render ctx <$> dims <*> assets <*> mPos <*> renderedMazeSignal <*> game))
        mcanvas
