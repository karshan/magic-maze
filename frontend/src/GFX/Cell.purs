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

drawCell :: DimensionPair -> Cells -> Int -> Int -> Cell -> Drawing
drawCell dims maze x y cell =
  let mp x_ y_ = MapPoint { x: x_, y: y_ }
  in
    mapToScreenD dims (MapPoint {x, y})
      (GFX.cell'
        (cell.special == Just STUnwalkable)
        (cell.special == Just STEntrance)
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

flipVertical = scale (-1.0) 1.0
flipHorizontal = scale 1.0 (-1.0)

-- FIXME all the translations are a bit off. fix with the help of figma
drawCellExplore :: Dir -> DimensionPair -> Int -> Int -> Drawing -> Drawing
drawCellExplore dir dims x y =
  let mapT = mapToScreenD dims (MapPoint { x, y })
  in case dir of
    N -> translate (51.0 - tileHalfWidth) (-3.0) <<< mapT
    S -> translate (80.0 - tileHalfWidth) (2.0 * tileHalfHeight) <<< mapT <<< flipVertical <<< flipHorizontal
    E -> translate (-5.0) (2.0 * tileHalfHeight) <<< mapT <<< flipHorizontal
    W -> translate 20.0 (-5.0) <<< mapT <<< flipVertical

-- FIXME smaller bbox, this is a bbox for the whole cell right now
evalExploreBBox :: DimensionPair -> Dir -> MapPoint -> ScreenPoint -> Maybe Point
evalExploreBBox dims dir mp mousePos = evalIsoBBox dims mp mousePos

