module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import DOM (onDOMContentLoaded)
import Signal.DOM (animationFrame, keyPressed, windowDimensions, DimensionPair)
import Signal (foldp, sampleOn, runSignal)
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Maybe (Maybe (..), maybe)
import Data.Map (Map, member, lookup)
import Data.Map as Map
import Data.Monoid (guard)
import Data.Tuple (Tuple (..))
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, drawImage, getCanvasElementById, getContext2D, setCanvasHeight, setCanvasWidth, tryLoadImage)
import Graphics.Drawing (translate, rectangle, filled, fillColor, Drawing, image)
import Color (white)
import Graphics.Drawing as D
import GFX as GFX
import Data.Foldable (foldMap)
import Signal.Channel (channel, send, subscribe)

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

type Inputs = { left :: Boolean, up :: Boolean, right :: Boolean, down :: Boolean }
type GameState = { maze :: Maze }

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
            }
        }

gameLogic :: Inputs -> GameState -> GameState
gameLogic _ g = g

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

render :: Context2D -> DimensionPair -> GameState -> Maybe CanvasImageSource -> Effect Unit
render ctx dims gs mRed = do
  D.render ctx (filled (fillColor white) (rectangle 0.0 0.0 (toNumber dims.w) (toNumber dims.h)))
  let base = translate ((toNumber dims.w)/2.0 - 64.0) ((toNumber dims.h)/2.0 - 128.0)
  let drawing =
        foldMap
          (\x ->
            foldMap
              (\y ->
                maybe mempty
                  (drawCell gs.maze.cells x y)
                  (lookup { x: x, y: y } gs.maze.cells))
              (gs.maze.borders.up .. gs.maze.borders.down))
          (gs.maze.borders.left .. gs.maze.borders.right)
  let south = translate (-64.0) 32.0
  let east  = translate 64.0 32.0
  let player t = maybe mempty (t <<< translate 43.0 (-26.0) <<< base <<< image) mRed
  D.render ctx (base drawing <> player (translate 0.0 0.0) <> player (east <<< south))

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
    maybe
        (log "error no canvas")
        (\canvas -> do
            let inputs = { left: _, right: _, up: _, down: _ } <$> leftInputs <*> rightInputs <*> upInputs <*> downInputs
            let game = foldp gameLogic initialState inputs
            ctx <- getContext2D canvas
            runSignal (resize canvas <$> dims)
            redPChan <- channel Nothing
            runSignal (render ctx <$> dims <*> game <*> subscribe redPChan)
            tryLoadImage "svg/player-red.svg" (send redPChan))
        mcanvas
