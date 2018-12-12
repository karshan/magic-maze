module Main where

import Isometric
import Prelude
import Signal.DOM
import Signal.Effect
import Types

import Color (Color, white, rgb, rgba, black)
import DOM (onDOMContentLoaded)
import Data.Array ((..))
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
import Signal (Signal, foldp, sampleOn, runSignal, constant, map2)
import Signal.Channel (channel, send, subscribe)

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
            players: Map.fromFoldable [
                Tuple Red (MapPoint { x: 1, y: 1 }),
                Tuple Yellow (MapPoint { x: 2, y: 1 }),
                Tuple Green (MapPoint { x: 1, y: 2 }),
                Tuple Purple (MapPoint { x: 2, y: 2  })
            ],
            dragging: Nothing
        }

maybeStartDrag :: Inputs -> PlayerPositions -> Maybe DragState
maybeStartDrag i players =
  unwrap $
   foldlWithIndex
     (\playerColor accum position ->
         accum <>
           map ({ playerColor: playerColor, dragPoint: _ })
             (First $ evalBBox (GFX.playerBBox i.dims position) (toScreenPoint i.mousePos)))
     (First Nothing)
     players

dropPlayer :: Inputs -> DragState -> PlayerPositions -> PlayerPositions
dropPlayer i { playerColor, dragPoint } =
  Map.update (const $ Just $ screenToMap i.dims
                (ScreenPoint $ toPoint i.mousePos - dragPoint + GFX.playerCenterT))
    playerColor

data DragCommand =
    StartDrag
  | EndDrag DragState

gameLogic :: Inputs -> GameState -> GameState
gameLogic i g =
  let dragCommand = unwrap $ (do
        dragState <- First g.dragging
        guard (not i.mousePressed) (pure (EndDrag dragState))) <>
        guard (isNothing g.dragging && i.mousePressed) (pure StartDrag)
  in case dragCommand of
        Nothing -> g
        Just StartDrag -> g { dragging = maybeStartDrag i g.players }
        Just (EndDrag dragState) -> g { players = dropPlayer i dragState g.players, dragging = Nothing }

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

drawPlayer :: DimensionPair -> MapPoint -> Maybe Point -> CoordinatePair -> CanvasImageSource -> Drawing
drawPlayer dims playerPosition mDragPoint mouse canvasImage =
    (maybe
        (GFX.playerCell <<< mapToScreenD dims playerPosition)
        (\dragPoint -> translate' (toPoint mouse - dragPoint))
        mDragPoint)
    (image canvasImage)

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
renderMaze :: Context2D -> Maze -> { dims :: DimensionPair, assets :: Assets } -> Effect ImageData
renderMaze ctx maze { dims, assets } = do
  D.render ctx (filled (fillColor (rgba 0 0 0 0.0)) (rectangle 0.0 0.0 (toNumber dims.w) (toNumber dims.h)))
  let cells = forAllCells maze (drawCell dims maze.cells)
  let walls = forAllCells maze (drawCellWall dims maze.cells)
  let bg = maybe mempty image (lookup Background assets)
  D.render ctx (GFX.background dims bg <> cells <> walls)
  getImageData ctx 0.0 0.0 (toNumber dims.w) (toNumber dims.h)

renderText :: Number -> Number -> Color -> String -> Drawing
renderText x y c s = D.text (D.font D.monospace 12 mempty) x y (D.fillColor c) s

render :: Context2D -> DimensionPair -> Assets -> CoordinatePair -> ImageData -> GameState -> Effect Unit
render ctx dims assets mouse renderedMaze gs = do
  let highlight = screenToMap dims (toScreenPoint mouse)
  let debugText = renderText 100.0 100.0 white (show gs.players)
  let players =
        (foldMapWithIndex
          (\asset img ->
              case asset of
                Player p ->
                    let mDragPoint = unwrap do
                          ds <- First gs.dragging
                          guard (ds.playerColor == p) (pure ds.dragPoint)
                    in maybe mempty (\pos -> drawPlayer dims pos mDragPoint mouse img) (lookup p gs.players)
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
    maybe
        (log "error no canvas")
        (\canvas -> do
            let inputs = { left: _, right: _, up: _, down: _, dims: _, mousePos: _, mousePressed: _ } <$>
                          leftInputs <*> rightInputs <*> upInputs <*> downInputs <*> dims <*> mPos <*> mPressed
            let game = foldp gameLogic initialState (sampleOn frames inputs)
            ctx <- getContext2D canvas
            runSignal (resize canvas <$> dims)
            renderedMaze <- mapEffect (renderMaze ctx initialState.maze) -- TODO renderMaze <~ Signal Maze
            assets <- loadAssets $ Map.fromFoldable [
                        Tuple (Player Red) "svg/player-red.svg",
                        Tuple (Player Yellow) "svg/player-yellow.svg",
                        Tuple (Player Green) "svg/player-green.svg",
                        Tuple (Player Purple) "svg/player-purple.svg",
                        Tuple Background "svg/background.svg"
                      ]
            let renderedMazeSignal = renderedMaze $ { dims: _, assets: _ } <$> dims <*> assets
            runSignal (render ctx <$> dims <*> assets <*> mPos <*> renderedMazeSignal <*> game))
        mcanvas
