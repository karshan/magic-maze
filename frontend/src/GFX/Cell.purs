module GFX.Cell where

import Data.Lens ((^.))
import Data.Map (lookup)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import GFX as GFX
import Graphics.Drawing (Drawing, Point, image)
import Isometric (evalIsoBBox, mapToScreenD)
import Prelude
import Signal.DOM (DimensionPair)
import Types (Assets, AssetName(..), Cell, Cells, Dir, MapPoint (..), ScreenPoint, SpecialTile(..), down, right, special, walls)

-- TODO all these functions should be in the reader monad
drawCell :: Assets -> DimensionPair -> Cells -> Int -> Int -> Cell -> Drawing
drawCell assets dims maze x y cell =
  let mp x_ y_ = MapPoint { x: x_, y: y_ }
  in
    mapToScreenD dims (MapPoint {x, y})
      (GFX.cell
        assets
        (cell^.special)
        (maybe true (\c -> c^.special == Just STUnwalkable) $ lookup (mp x (y + 1)) maze)
        (maybe true (\c -> c^.special == Just STUnwalkable) $ lookup (mp (x + 1) y) maze))

drawCellWeapon :: Assets -> DimensionPair -> Cells -> Int -> Int -> Cell -> Drawing
drawCellWeapon assets dims maze x y cell =
  let mp x_ y_ = MapPoint { x: x_, y: y_ }
  in
    mapToScreenD dims (MapPoint {x, y})
      (GFX.cellWeapon
        assets
        (cell^.special))

-- FIXME this fails to draw NW corner in the case the NW corner has no NW neighbor
drawCellWall :: Assets -> DimensionPair -> Cells -> Int -> Int -> Cell -> Drawing
drawCellWall assets dims maze x y cell =
 let mp x_ y_ = MapPoint { x: x_, y: y_ }
     cellT = mapToScreenD dims (MapPoint {x, y})
 in guard (cell^.walls^.right && not (cell^.walls^.down)) (cellT $ maybe mempty image $ lookup AWallRight assets)
 <> guard (cell^.walls^.down && not (cell^.walls^.right)) (cellT $ maybe mempty image $ lookup AWallDown assets)
 <> guard (cell^.walls^.right && cell^.walls^.down) (cellT $ maybe mempty image $ lookup AWallRightDown assets)
 <> maybe mempty (const (cellT $ maybe mempty image $ lookup AWallNWCorner assets))
      (do eastCell <- lookup (mp (x + 1) y) maze
          guard (eastCell^.walls^.down) (pure unit)
          southCell <- lookup (mp x (y + 1)) maze
          guard (southCell^.walls^.right) (pure unit))

-- FIXME smaller bbox, this is a bbox for the whole cell right now
evalExploreBBox :: DimensionPair -> Dir -> MapPoint -> ScreenPoint -> Maybe Point
evalExploreBBox dims dir mp mousePos = evalIsoBBox dims mp mousePos

