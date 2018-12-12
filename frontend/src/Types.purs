module Types where

import Prelude
import Data.Maybe (Maybe)
import Data.Map (Map)
import Data.Int (toNumber)
import Graphics.Drawing (Point)
import Graphics.Canvas (CanvasImageSource)
import Signal.DOM (DimensionPair, CoordinatePair)

type Rect  = { x :: Number, y :: Number, w :: Number, h :: Number }

type DirMap v = { left :: v, up :: v, right :: v, down :: v }

type Cells = Map MapPoint Cell
type Maze = { cells :: Cells, borders :: DirMap Int }
type Cell = { walls :: { right :: Boolean, down :: Boolean }, walkable :: Boolean }

type Inputs = {
  left :: Boolean,
  up :: Boolean,
  right :: Boolean,
  down :: Boolean,
  dims :: DimensionPair,
  mousePos :: CoordinatePair,
  mousePressed :: Boolean
}

type GameState = { maze :: Maze, playerPos :: MapPoint, dragging :: Maybe Point }

newtype ScreenPoint = ScreenPoint Point
instance semiringScreenPoint :: Semiring ScreenPoint where
  add (ScreenPoint a) (ScreenPoint b) = ScreenPoint { x: a.x + b.x, y: a.y + b.y }
  mul (ScreenPoint a) (ScreenPoint b) = ScreenPoint { x: a.x * b.x, y: a.y * b.y }
  zero = ScreenPoint { x: 0.0, y: 0.0 }
  one = ScreenPoint { x: 1.0, y: 1.0 }
instance ringScreenPoint :: Ring ScreenPoint where
  sub (ScreenPoint a) (ScreenPoint b) = ScreenPoint { x: a.x - b.x, y: a.y - b.y }

toScreenPoint :: { x :: Int, y :: Int } -> ScreenPoint
toScreenPoint { x, y } = ScreenPoint { x: toNumber x, y: toNumber y }

newtype MapPoint = MapPoint { x :: Int, y :: Int }
derive instance eqMapPoint :: Eq MapPoint
derive instance ordMapPoint :: Ord MapPoint

data PlayerColor =
    Red
  | Yellow
  | Green
  | Purple
derive instance eqPlayerColor :: Eq PlayerColor
derive instance ordPlayerColor :: Ord PlayerColor

data AssetName =
    Player PlayerColor
  | Background
derive instance eqAssetName :: Eq AssetName
derive instance ordAssetName :: Ord AssetName

type Assets = Map AssetName (Maybe CanvasImageSource)
