module Main where

import Prelude
import Signal.DOM

import Color (Color, white, rgb, black)
import DOM (onDOMContentLoaded)
import Data.Array ((..))
import Data.Foldable (class Foldable, foldMap)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Int (toNumber, round, floor)
import Data.Map (Map, member, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import GFX as GFX
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, drawImage, getCanvasElementById, getContext2D, setCanvasHeight, setCanvasWidth, tryLoadImage, getImageData, ImageData, putImageData)
import Graphics.Drawing (translate, rectangle, filled, fillColor, Drawing, image, Point)
import Graphics.Drawing as D
import Graphics.Drawing.Font as D
import Signal (Signal, foldp, sampleOn, runSignal, constant, map2)
import Signal.Channel (channel, send, subscribe)
import Signal.Effect
import Types
import Isometric

translate' :: Point -> Drawing -> Drawing
translate' { x, y } = translate x y

keycodes :: DirMap Int
keycodes = {
        left: 37,
        up: 38,
        right: 39,
        down: 40
    }


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
            playerPos: MapPoint {
              x: 0,
              y: 0
            },
            dragging: Nothing
        }

gameLogic :: Inputs -> GameState -> GameState
gameLogic i g =
  maybe
    (if i.mousePressed then
       g { dragging = evalBBox (GFX.playerBBox i.dims g.playerPos) (toScreenPoint i.mousePos) }
     else
       g)
    (\dragPoint ->
        if not i.mousePressed then
          let sp = ScreenPoint
          in g {
                 playerPos = screenToMap i.dims
                     (toScreenPoint i.mousePos - sp dragPoint + sp GFX.playerCenterT),
                 dragging = Nothing
               }
        else
          g)
    g.dragging

drawCell :: DimensionPair -> Cells -> Int -> Int -> Cell -> Drawing
drawCell dims maze x y cell =
  let mp x_ y_ = MapPoint { x: x_, y: y_ }
  in
    mapToScreenD dims (MapPoint {x, y})
      (GFX.cell'
        cell.walkable
        (not $ member (mp x (y + 1)) maze)
        (not $ member (mp (x + 1) y) maze))

drawCellWall :: DimensionPair -> Cells -> Int -> Int -> Cell -> Drawing
drawCellWall dims maze x y cell =
 let mp x_ y_ = MapPoint { x: x_, y: y_ }
     cellT = mapToScreenD dims (MapPoint {x, y})
 in guard cell.walls.right (cellT GFX.wallRight)
 <> guard cell.walls.down (cellT GFX.wallDown)
 <> guard (cell.walls.right && cell.walls.down) (cellT GFX.wallSECorner')
 <> maybe mempty (const GFX.wallNWCorner')
      (do eastCell <- lookup (mp (x + 1) y) maze
          guard eastCell.walls.down (pure unit)
          southCell <- lookup (mp x (y + 1)) maze
          guard southCell.walls.right (pure unit))

toPoint :: { x :: Int, y :: Int } -> Point
toPoint { x, y } = { x: toNumber x, y: toNumber y }

drawPlayer :: DimensionPair -> Maybe Point -> CoordinatePair -> MapPoint -> Maybe CanvasImageSource -> Drawing
drawPlayer dims mDragPoint mouse playerPos mCanvasImage =
  maybe
    mempty
    ((maybe
        (GFX.playerCell <<< mapToScreenD dims playerPos)
        (\dragPoint -> translate' (toPoint mouse - dragPoint))
        mDragPoint) <<< image)
    mCanvasImage

forAllCells :: forall m. Monoid m => Maze -> (Int -> Int -> Cell -> m) -> m
forAllCells maze f =
  foldMap
    (\x ->
      foldMap
        (\y ->
          maybe
            mempty
            (f x y)
            (lookup (MapPoint { x, y }) maze.cells))
        (maze.borders.up .. maze.borders.down))
    (maze.borders.left .. maze.borders.right)

-- TODO Render in large offscreen canvas then never re-render until maze changes.
--      Then we don't need to clear the background, it will be transparent by default.
renderMaze :: Context2D -> Maze -> DimensionPair -> Effect ImageData
renderMaze ctx maze dims = do
  D.render ctx (filled (fillColor white) (rectangle 0.0 0.0 (toNumber dims.w) (toNumber dims.h)))
  let cells = forAllCells maze (drawCell dims maze.cells)
  let walls = forAllCells maze (drawCellWall dims maze.cells)
  D.render ctx (cells <> walls)
  getImageData ctx 0.0 0.0 (toNumber dims.w) (toNumber dims.h)

renderText :: Number -> Number -> Color -> String -> Drawing
renderText x y c s = D.text (D.font D.monospace 12 mempty) x y (D.fillColor c) s

render :: Context2D -> DimensionPair -> Assets -> CoordinatePair -> ImageData -> GameState -> Effect Unit
render ctx dims assets mPos renderedMaze gs = do
  putImageData ctx renderedMaze 0.0 0.0
  let highlight = screenToMap dims (toScreenPoint mPos)
  let debugText = renderText 100.0 100.0 black (show mPos)
  D.render ctx (maybe mempty (drawPlayer dims gs.dragging mPos gs.playerPos) (lookup (Player Red) assets))

resize :: CanvasElement -> DimensionPair -> Effect Unit
resize canvas dims = do
  setCanvasWidth canvas (toNumber dims.w)
  setCanvasHeight canvas (toNumber dims.h)

type Asset = Tuple AssetName (Maybe CanvasImageSource)
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
        pure $ map2 (\(Tuple k v) m -> Map.insert k v m) sig accum)
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
    maybe
        (log "error no canvas")
        (\canvas -> do
            let inputs = { left: _, right: _, up: _, down: _, dims: _, mousePos: _, mousePressed: _ } <$>
                          leftInputs <*> rightInputs <*> upInputs <*> downInputs <*> dims <*> mPos <*> mPressed
            let game = foldp gameLogic initialState (sampleOn frames inputs)
            ctx <- getContext2D canvas
            runSignal (resize canvas <$> dims)
            renderedMaze <- mapEffect (renderMaze ctx initialState.maze) -- TODO renderMaze <~ Signal Maze
            assets <- loadAssets $ Map.fromFoldable [ Tuple (Player Red) "svg/player-red.svg" ]
            runSignal (render ctx <$> dims <*> assets <*> mPos <*> (renderedMaze dims) <*> game))
        mcanvas
