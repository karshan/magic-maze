module GFX.Cell where

import Prelude
import Data.Map (lookup, member)
import Data.Maybe (maybe)
import Data.Monoid (guard)
import Graphics.Drawing (Drawing)
import Signal.DOM (DimensionPair)

import GFX as GFX
import Types (Cell, Cells, MapPoint(..))
import Isometric (mapToScreenD)

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
