module GFX.Cell where

import Prelude
import Data.Map (lookup, member)
import Data.Maybe (Maybe (..), maybe)
import Data.Monoid (guard)
import Graphics.Drawing (Drawing, Point, translate, scale)
import Signal.DOM (DimensionPair)

import GFX as GFX
import Types
import Isometric

drawCell :: Assets -> DimensionPair -> Cells -> Int -> Int -> Cell -> Drawing
drawCell assets dims maze x y cell =
  let mp x_ y_ = MapPoint { x: x_, y: y_ }
  in
    mapToScreenD dims (MapPoint {x, y})
      (GFX.cell'
        assets
        cell.special
        (not $ member (mp x (y + 1)) maze)
        (not $ member (mp (x + 1) y) maze))

drawCellWall :: DimensionPair -> Cells -> Int -> Int -> Cell -> Drawing
drawCellWall dims maze x y cell =
 let mp x_ y_ = MapPoint { x: x_, y: y_ }
     cellT = mapToScreenD dims (MapPoint {x, y})
 in guard cell.walls.right (cellT GFX.wallRight)
 <> guard cell.walls.down (cellT GFX.wallDown)
 <> guard (cell.walls.right && cell.walls.down) (cellT GFX.wallSECorner')
 <> maybe mempty (const (cellT GFX.wallNWCorner'))
      (do eastCell <- lookup (mp (x + 1) y) maze
          guard eastCell.walls.down (pure unit)
          southCell <- lookup (mp x (y + 1)) maze
          guard southCell.walls.right (pure unit))

-- FIXME smaller bbox, this is a bbox for the whole cell right now
evalExploreBBox :: DimensionPair -> Dir -> MapPoint -> ScreenPoint -> Maybe Point
evalExploreBBox dims dir mp mousePos = evalIsoBBox dims mp mousePos

