module Tiles where

import Types

import Prelude
import Data.Array ((..))
import Data.Foldable
import Data.FoldableWithIndex
import Data.FunctorWithIndex
import Data.Lens ((^.), (.~), over)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe.First (First(..))
import Data.Maybe
import Data.Newtype
import Data.Set as Set
import Data.Tuple (Tuple(..))

-- rotate (n * 90) deg clockwise
-- x: (y - y0) + x0
-- y: -(x - x0) + y0
rotatePoint :: Int -> MapPoint -> MapPoint
rotatePoint 0 (MapPoint { x, y }) = MapPoint { x, y }
rotatePoint n (MapPoint { x, y }) = rotatePoint (n - 1) $ MapPoint { x: 3 - y, y: x }

mapKeys :: forall k v. Ord k => (k -> k) -> Map k v -> Map k v
mapKeys f m =
  let theArray :: forall a. Array a -> Array a
      theArray = identity
  in Map.fromFoldable $ theArray $ map (\(Tuple k v) -> Tuple (f k) v) $ Map.toUnfoldableUnordered m

translateTile :: MapPoint -> Tile -> Tile
translateTile mp t = t #
  cells .~ (mapKeys (_ + mp) (t^.cells)) #
  escalators .~ (map (\(Escalator mp1 mp2) -> Escalator (mp1 + mp) (mp2 + mp)) (t^.escalators))

rotateCells :: Int -> Cells -> Cells
rotateCells 0 cells = cells
rotateCells n cells =
  let mp x y = MapPoint { x, y }
      rotateDir N = E
      rotateDir E = S
      rotateDir S = W
      rotateDir W = N
      rotateSpecial (Just (STExplore col dir)) = Just $ STExplore col (rotateDir dir)
      rotateSpecial (Just (STExit col dir)) = Just $ STExit col (rotateDir dir)
      rotateSpecial s = s
  in rotateCells (n - 1) $
    foldMap
      (\x ->
        foldMap
          (\y ->
              let defaultCell = Cell { special: Nothing, walls: Walls { right: false, down: false } }
                  origCell = fromMaybe defaultCell $ Map.lookup (mp x y) cells
                  eastCell = fromMaybe defaultCell $ Map.lookup (mp (x + 1) y) cells
              in Map.singleton (mp x y)
                (Cell {
                  special: rotateSpecial (origCell^.special),
                  walls: Walls { right: eastCell^.walls^.down, down: origCell^.walls^.right }
                }))
          (0..3))
      (0..3)

-- We can't do rotateWalls n ... rotatePoint n. because the points and walls need to be rotated
-- 90 degrees at a time together.
rotateTile :: Int -> Tile -> Tile
rotateTile 0 t = t
rotateTile n t = rotateTile (n - 1) $ (t #
  cells .~ (rotateCells 1 $ mapKeys (rotatePoint 1) (t^.cells)) #
  escalators .~ (map (\(Escalator mp1 mp2) -> Escalator (rotatePoint 1 mp1) (rotatePoint 1 mp2)) (t^.escalators)))

addBorderWalls :: Tile -> Tile
addBorderWalls t =
  let noBorderWall (Just (STExplore _ _)) = true
      noBorderWall (Just STEntrance) = true
      noBorderWall _ = false
  in t # cells .~
        (mapWithIndex
          (\(MapPoint { x, y }) c ->
            if noBorderWall (c^.special) then
              c
            else
              if x == 3 then
                if y == 3 then
                  c # walls .~ Walls { right: true, down: true }
                else
                  c # over walls (right .~ true)
              else
                if y == 3 then
                  c # over walls (down .~ true)
                else
                  c)
          (t^.cells))

dirToInt :: Dir -> Int
dirToInt N = 0
dirToInt E = 1
dirToInt S = 2
dirToInt W = 3

getRotation :: Dir -> Dir -> Int
getRotation explore entrance = (dirToInt explore - dirToInt entrance + 2) `mod` 4

rotateAndTranslate :: MapPoint -> Dir -> Tile -> Tile
rotateAndTranslate mp dir tile  =
  let mkMp x y = MapPoint {x,y}
      rotated = addBorderWalls $ rotateTile (getRotation dir (tile^.entrance^.side)) tile
      findEntrance f = fromMaybe 0 $ unwrap $
          foldMap
            (\i -> First $ do
                c <- Map.lookup (f i) (rotated^.cells)
                if c^.special == Just STEntrance then Just i else Nothing)
            (0..3)
      south x = mkMp x 3
      west y = mkMp 0 y
      north x = mkMp x 0
      east y = mkMp 3 y
   in case dir of
           N -> translateTile (mp + mkMp (negate $ findEntrance south) (-4)) rotated
           E -> translateTile (mp + mkMp 1 (negate $ findEntrance west)) rotated
           S -> translateTile (mp + mkMp (negate $ findEntrance north) 1) rotated
           W -> translateTile (mp + mkMp (-4) (negate $ findEntrance east)) rotated

mergeTiles :: Maze -> Tile -> MapPoint -> Dir -> Maybe Maze
mergeTiles cur newTile mp dir = do
  let theArray :: forall a. Array a -> Array a
      theArray = identity
  let mkMp x y = MapPoint { x, y }
  let rntTile = rotateAndTranslate mp dir newTile
  let newCells = Map.union (cur^.cells) (rntTile^.cells)
  let newEscalators = Set.toUnfoldable $
                        Set.union (Set.fromFoldable $ cur^.escalators) (Set.fromFoldable $ rntTile^.escalators)
  let arrayCells = theArray $ Map.toUnfoldable newCells
  let xs = map (\(Tuple (MapPoint { x, y }) v) -> x) arrayCells
  let ys = map (\(Tuple (MapPoint { x, y }) v) -> y) arrayCells
  newBorders <- { up: _, down: _, left: _, right: _ } <$> minimum ys <*> maximum ys <*> minimum xs <*> maximum xs
  if length newCells /= length (cur^.cells) + 16 then Nothing else
    pure $ (cur #
      cells .~ newCells #
      borders .~ DirMap newBorders #
      escalators .~ newEscalators)
