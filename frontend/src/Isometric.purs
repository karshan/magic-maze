module Isometric
  (
    tileHalfWidth, tileHalfHeight
  , screenToMap, mapToScreen, mapToScreenD
  , evalBBox
  , evalIsoBBox
  )
  where

import Data.Int
import Data.Maybe
import Graphics.Drawing
import Prelude
import Signal.DOM
import Types

-- TODO cellHalfWidth, tiles are 4x4 cells
tileHalfWidth :: Number
tileHalfWidth = 64.0

tileHalfHeight :: Number
tileHalfHeight = 32.0

-- ScreenCoordinates of MapPoint 0 0. Two tileFullHeights above the middle of the screen
-- This way the initial 4x4 game tile will be centered on the screen
origin :: DimensionPair -> ScreenPoint
origin dims =
  ScreenPoint {
    x: ((toNumber dims.w)/2.0),
    y: ((toNumber dims.h)/2.0 - (4.0 * tileHalfHeight))
  }

screenToMap :: DimensionPair -> ScreenPoint -> MapPoint
screenToMap dims mPos =
  let (ScreenPoint screen) = mPos - origin dims
  in
      MapPoint {
        x: floor (screen.x/tileHalfWidth + screen.y/tileHalfHeight)/2,
        y: floor (screen.y/tileHalfHeight - screen.x/tileHalfWidth)/2
      }

mapToScreen :: DimensionPair -> MapPoint -> ScreenPoint
mapToScreen dims (MapPoint { x, y }) =
  let base = origin dims
      x_ = toNumber x
      y_ = toNumber y
      iso = ScreenPoint {
        x: (x_ - y_) * tileHalfWidth,
        y: (x_ + y_) * tileHalfHeight
      }
  in base + iso

mapToScreenD :: DimensionPair -> MapPoint -> Drawing -> Drawing
mapToScreenD dims = (\(ScreenPoint {x, y}) -> translate x y) <<< mapToScreen dims

-- If the given ScreenPoint is within the BBox (Rect) then Just (dx, dy)
-- where dx, dy is the position of the ScreenPoint relative to the BBox
-- else Nothing
evalBBox :: Rect -> ScreenPoint -> Maybe Point
evalBBox { x, y, w, h } (ScreenPoint p) =
  if x <= p.x && p.x <= x + w && y <= p.y && p.y <= y + h then
    Just { x: p.x - x, y: p.y - y }
  else
    Nothing

-- If the given ScreenPoint is within the Cell at the given MapPoint
-- then Just (dx, dy) where dx, dy is the position of the ScreenPoint
-- relative to the cell's NW corner
-- else Nothing
evalIsoBBox :: DimensionPair -> MapPoint -> ScreenPoint -> Maybe Point
evalIsoBBox dims mp (ScreenPoint { x, y }) =
  let ScreenPoint c = mapToScreen dims mp
      lineSide x0 y0 m = y - y0 - (m * (x - x0))
  in if lineSide c.x c.y 0.5 > 0.0 && -- N Wall
        lineSide c.x c.y (-0.5) > 0.0 && -- W wall
        lineSide (c.x - tileHalfWidth) (c.y + tileHalfHeight) 0.5 < 0.0 && -- S wall
        lineSide (c.x + tileHalfWidth) (c.y + tileHalfHeight) (-0.5) < 0.0 then -- E wall
        Just { x: x - c.x, y: y - c.y }
     else
        Nothing
