module Tiles where

import Types

import Prelude
import Data.Array
import Data.Foldable
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe.First (First(..))
import Data.Maybe
import Data.Newtype
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
translateTile mp t = t { cells = mapKeys (_ + mp) t.cells }

-- FIXME rotate walls and rotate cell.explore dir's
rotateTile :: Int -> Tile -> Tile
rotateTile n t = t { cells = mapKeys (rotatePoint n) t.cells }

dirToInt :: Dir -> Int
dirToInt N = 0
dirToInt E = 1
dirToInt S = 2
dirToInt W = 3

getRotation :: Dir -> Dir -> Int
getRotation explore entrance = (dirToInt explore - dirToInt entrance + 2) `mod` 4

-- TODO switch out large case statement for some simple math
rotateAndTranslate :: MapPoint -> Dir -> Tile -> Tile
rotateAndTranslate mp dir tile  = 
  let mkMp x y = MapPoint {x,y}
      rotated = rotateTile (getRotation dir tile.entrance.side) tile
      findEntrance f = fromMaybe 0 $ unwrap $ 
          foldMap 
            (\i -> First $ do
                c <- Map.lookup (f i) rotated.cells
                if c.special == Just STEntrance then Just i else Nothing) 
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

mergeTiles :: Maze -> Int -> MapPoint -> Dir -> Maybe Maze
mergeTiles cur newIndex mp dir = do
  let theArray :: forall a. Array a -> Array a
      theArray = identity
  newTile <- tiles !! newIndex
  let mkMp x y = MapPoint { x, y }
  let newCells = Map.union cur.cells (rotateAndTranslate mp dir newTile).cells
  let arrayCells = theArray $ Map.toUnfoldable newCells
  let xs = map (\(Tuple (MapPoint { x, y }) v) -> x) arrayCells
  let ys = map (\(Tuple (MapPoint { x, y }) v) -> y) arrayCells
  newBorders <- { up: _, down: _, left: _, right: _ } <$> minimum ys <*> maximum ys <*> minimum xs <*> maximum xs
  pure $ cur { cells = Map.union cur.cells (rotateAndTranslate mp dir newTile).cells, borders = newBorders }

-- TODO calculate STExplore dir from map tile cell's coordinate
initialTile :: Maze
initialTile =
    let noWalls = { right: false, down: false }
        downWall = { right: false, down: true }
        rightWall = { right: true, down: false }
        rdWall = { right: true, down: true }
    in {
      cells: Map.fromFoldable [
        Tuple (MapPoint { x: 0, y: 0 }) { walls: noWalls, special: Just STUnwalkable },
        Tuple (MapPoint { x: 1, y: 0 }) { walls: noWalls, special: Nothing },
        Tuple (MapPoint { x: 2, y: 0 }) { walls: noWalls, special: Just (STExplore Yellow N) },
        Tuple (MapPoint { x: 3, y: 0 }) { walls: rdWall, special: Nothing },
        Tuple (MapPoint { x: 0, y: 1 }) { walls: rightWall, special: Just (STExplore Green W) },
        Tuple (MapPoint { x: 1, y: 1 }) { walls: noWalls, special: Nothing },
        Tuple (MapPoint { x: 2, y: 1 }) { walls: noWalls, special: Nothing },
        Tuple (MapPoint { x: 3, y: 1 }) { walls: rdWall, special: Nothing },
        Tuple (MapPoint { x: 0, y: 2 }) { walls: downWall, special: Nothing },
        Tuple (MapPoint { x: 1, y: 2 }) { walls: noWalls, special: Nothing },
        Tuple (MapPoint { x: 2, y: 2 }) { walls: noWalls, special: Nothing },
        Tuple (MapPoint { x: 3, y: 2 }) { walls: downWall, special: Just (STExplore Purple E) },
        Tuple (MapPoint { x: 0, y: 3 }) { walls: downWall, special: Nothing },
        Tuple (MapPoint { x: 1, y: 3 }) { walls: noWalls, special: Just (STExplore Red S) },
        Tuple (MapPoint { x: 2, y: 3 }) { walls: downWall, special: Nothing },
        Tuple (MapPoint { x: 3, y: 3 }) { walls: rdWall, special: Just STUnwalkable }
      ],
      borders: {
        left: 0,
        up: 0,
        right: 3,
        down: 3
      }
    }

tiles :: Array Tile
tiles =
  let noWalls = { right: false, down: false }
      downWall = { right: false, down: true }
      rightWall = { right: true, down: false }
      rdWall = { right: true, down: true }
      unwalk = Just STUnwalkable
      exp c d = Just (STExplore c d)
      mk x y walls special = 
        Tuple (MapPoint { x, y }) { walls, special }
  in [{
      entrance: { side: E, offset: 2 }, -- TODO calculate from .cells
      cells: Map.fromFoldable [
        mk 0 0 rightWall Nothing,
        mk 1 0 noWalls unwalk,
        mk 2 0 rightWall (exp Green N),
        mk 3 0 noWalls unwalk,
        mk 0 1 downWall (exp Purple W),
        mk 1 1 downWall Nothing,
        mk 2 1 rightWall Nothing,
        mk 3 1 noWalls unwalk,
        mk 0 2 noWalls unwalk,
        mk 1 2 downWall Nothing,
        mk 2 2 noWalls Nothing,
        mk 3 2 downWall (Just STEntrance),
        mk 0 3 noWalls unwalk,
        mk 1 3 noWalls unwalk,
        mk 2 3 rdWall Nothing,
        mk 3 3 noWalls unwalk
      ]
    }
  ]
