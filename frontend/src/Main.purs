module Main where

import Prelude
import Signal.DOM

import Color (white)
import DOM (onDOMContentLoaded)
import Data.Array ((..))
import Data.Foldable (foldMap)
import Data.Int (toNumber, round)
import Data.Map (Map, member, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import GFX as GFX
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, drawImage, getCanvasElementById, getContext2D, setCanvasHeight, setCanvasWidth, tryLoadImage, getImageData, ImageData, putImageData)
import Graphics.Drawing (translate, rectangle, filled, fillColor, Drawing, image)
import Graphics.Drawing as D
import Signal (foldp, sampleOn, runSignal)
import Signal.Channel (channel, send, subscribe)
import Signal.Effect

type DirMap v = { left :: v, up :: v, right :: v, down :: v }

type Point = { x :: Int, y :: Int }
type Maze = { cells :: Map Point Cell, borders :: DirMap Int }
type Cell = { walls :: { right :: Boolean, down :: Boolean }, walkable :: Boolean }

keycodes :: DirMap Int
keycodes = {
        left: 37,
        up: 38,
        right: 39,
        down: 40
    }

type Inputs = { left :: Boolean, up :: Boolean, right :: Boolean, down :: Boolean, dims :: DimensionPair, mPos :: CoordinatePair, mPressed :: Boolean }
type GameState = { maze :: Maze, playerPos :: Point, dragging :: Boolean }

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
                    Tuple { x: 0, y: 0 } { walls: rightWall, walkable: true },
                    Tuple { x: 1, y: 0 } { walls: noWalls, walkable: true },
                    Tuple { x: 2, y: 0 } { walls: downWall, walkable: true },
                    Tuple { x: 3, y: 0 } { walls: rightWall, walkable: true },
                    Tuple { x: 0, y: 1 } { walls: rightWall, walkable: true },
                    Tuple { x: 1, y: 1 } { walls: noWalls, walkable: true },
                    Tuple { x: 2, y: 1 } { walls: rightWall, walkable: true },
                    Tuple { x: 3, y: 1 } { walls: rightWall, walkable: true },
                    Tuple { x: 0, y: 2 } { walls: noWalls, walkable: true },
                    Tuple { x: 1, y: 2 } { walls: downWall, walkable: true },
                    Tuple { x: 2, y: 2 } { walls: rdWall, walkable: true },
                    Tuple { x: 3, y: 2 } { walls: noWalls, walkable: true },
                    Tuple { x: 0, y: 3 } { walls: downWall, walkable: true },
                    Tuple { x: 1, y: 3 } { walls: noWalls, walkable: true },
                    Tuple { x: 2, y: 3 } { walls: rdWall, walkable: true },
                    Tuple { x: 3, y: 3 } { walls: noWalls, walkable: false }
                ],
                borders: {
                    left: 0,
                    up: 0,
                    right: 3,
                    down: 3
                }
            },
            playerPos: {
              x: 0,
              y: 0
            },
            dragging: false
        }

dropPlayer :: DimensionPair -> CoordinatePair -> Point
dropPlayer dims mPos =
  let base = { x: (toNumber dims.w)/2.0 - 64.0, y: (toNumber dims.h)/2.0 - 128.0 }
      screen = { x: (toNumber mPos.x) - base.x, y: (toNumber mPos.y) - base.y }
  in
      { x: round (screen.x/64.0 + screen.y/32.0)/2
      , y: round (screen.y/32.0 - screen.x/64.0)/2
      }

inBBox :: CoordinatePair -> Point -> Boolean
inBBox _ _ = true

gameLogic :: Inputs -> GameState -> GameState
gameLogic i g =
  if g.dragging && i.mPressed == false then
    g { playerPos = dropPlayer i.dims i.mPos, dragging = false }
  else if g.dragging == false && i.mPressed && inBBox i.mPos g.playerPos then
    g { dragging = true }
  else
    g

translateN :: Int -> { x :: Number, y :: Number } -> Drawing -> Drawing
translateN n t = translate (t.x * (toNumber n)) (t.y * (toNumber n))

drawCell :: Map Point Cell -> Int -> Int -> Cell -> Drawing
drawCell maze x y cell =
  let south = { x: (-64.0), y: 32.0 }
      east  = { x: 64.0, y: 32.0 }
      cellT = translateN y south <<< translateN x east
  in
    cellT
      (GFX.cell'
        cell.walkable
        (not $ member { x: x, y: y + 1 } maze)
        (not $ member { x: x + 1, y: y } maze))
     <> guard cell.walls.right (cellT GFX.wallRight)
     <> guard cell.walls.down (cellT GFX.wallDown)
     <> guard (cell.walls.right && cell.walls.down) (cellT GFX.wallSECorner')
     <> maybe mempty (\_ -> GFX.wallNWCorner')
          (do eastCell <- lookup { x: x + 1, y: y } maze
              guard eastCell.walls.down (pure unit)
              southCell <- lookup { x: x, y: y + 1 } maze
              guard southCell.walls.right (pure unit))

-- TODO render in large offscreen canvas then never re-render until maze changes
renderMaze :: Context2D -> Maze -> DimensionPair -> Effect ImageData
renderMaze ctx maze dims = do
  D.render ctx (filled (fillColor white) (rectangle 0.0 0.0 (toNumber dims.w) (toNumber dims.h)))
  let base = translate ((toNumber dims.w)/2.0 - 64.0) ((toNumber dims.h)/2.0 - 128.0)
  let drawing =
        foldMap
          (\x ->
            foldMap
              (\y ->
                maybe mempty
                  (drawCell maze.cells x y)
                  (lookup { x: x, y: y } maze.cells))
              (maze.borders.up .. maze.borders.down))
          (maze.borders.left .. maze.borders.right)
  D.render ctx (base drawing)
  getImageData ctx 0.0 0.0 (toNumber dims.w) (toNumber dims.h)

render :: Context2D -> DimensionPair -> GameState -> Maybe CanvasImageSource -> CoordinatePair -> Boolean -> ImageData -> Effect Unit
render ctx dims gs mRed mPos mPressed renderedMaze = do
  let base = translate ((toNumber dims.w)/2.0 - 64.0) ((toNumber dims.h)/2.0 - 128.0)
  let player t = maybe mempty (t <<< translate 43.0 (-26.0) <<< image) mRed
  let south = { x: (-64.0), y: 32.0 }
  let east  = { x: 64.0, y: 32.0 }
  let playerT =
        if gs.dragging then
          translate (toNumber mPos.x - 60.0) (toNumber mPos.y)
        else
          translateN gs.playerPos.y south <<< translateN gs.playerPos.x east <<< base
  putImageData ctx renderedMaze 0.0 0.0
  D.render ctx (player playerT)

resize :: CanvasElement -> DimensionPair -> Effect Unit
resize canvas dims = do
  setCanvasWidth canvas (toNumber dims.w)
  setCanvasHeight canvas (toNumber dims.h)

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
            let inputs = { left: _, right: _, up: _, down: _, dims: _, mPos: _, mPressed: _ } <$> leftInputs <*> rightInputs <*> upInputs <*> downInputs <*> dims <*> mPos <*> mPressed
            let game = foldp gameLogic initialState inputs -- (sampleOn frames inputs)
            ctx <- getContext2D canvas
            runSignal (resize canvas <$> dims)
            redPChan <- channel Nothing
            renderedMaze <- mapEffect (renderMaze ctx initialState.maze)
            runSignal (render ctx <$> dims <*> game <*> subscribe redPChan <*> mPos <*> mPressed <*> (renderedMaze dims))
            tryLoadImage "svg/player-red.svg" (send redPChan))
        mcanvas
