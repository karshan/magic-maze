module Tiles where

import Types

import Prelude
import Control.Monad.State
import Data.Array ((..))
import Data.Foldable
import Data.FoldableWithIndex
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
translateTile mp t = t { cells = mapKeys (_ + mp) t.cells, escalators = Set.map (\(Tuple mp1 mp2) -> Tuple (mp1 + mp) (mp2 + mp)) t.escalators }

-- FIXME rewrite fold to ensure it runs in increasing order of x coordinate
-- TODO rename rotateWallsAndSpecial
rotateWalls :: Int -> Cells -> Cells
rotateWalls 0 cells = cells
rotateWalls n cells =
  rotateWalls (n - 1) $ flip execState cells $ foldlWithIndex
    (\i acc origCell -> do
      let west (MapPoint { x, y }) = MapPoint { x: x - 1, y }
          rotateDir N = E
          rotateDir E = S
          rotateDir S = W
          rotateDir W = N
          rotateSpecial (Just (STExplore col dir)) = Just $ STExplore col (rotateDir dir)
          rotateSpecial (Just (STExit col dir)) = Just $ STExit col (rotateDir dir)
          rotateSpecial s = s
          grd b a = if b then a else pure unit
      acc
      modify_ (Map.update (\a -> Just $ a { walls = { right: false, down: origCell.walls.right } }) i)
      grd origCell.walls.down
        (modify_ (Map.update (\a -> Just $ a { walls = { right: true, down: a.walls.down } }) (west i)))
      modify_ (Map.update (\a -> Just $ a { special = rotateSpecial a.special }) i))
    (pure unit)
    cells

-- We can't do rotateWalls n ... rotatePoint n. because the points and walls need to be rotated
-- 90 degrees at a time together.
rotateTile :: Int -> Tile -> Tile
rotateTile 0 t = t
rotateTile n t = rotateTile (n - 1) $ t { cells = rotateWalls 1 $ mapKeys (rotatePoint 1) t.cells, escalators = Set.map (\(Tuple mp1 mp2) -> Tuple (rotatePoint 1 mp1) (rotatePoint 1 mp2)) t.escalators }

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

-- FIXME add border walls
mergeTiles :: Maze -> Tile -> MapPoint -> Dir -> Maybe Maze
mergeTiles cur newTile mp dir = do
  let theArray :: forall a. Array a -> Array a
      theArray = identity
  let mkMp x y = MapPoint { x, y }
  let rntTile = rotateAndTranslate mp dir newTile
  let newCells = Map.union cur.cells rntTile.cells
  let newEscalators = Set.union cur.escalators rntTile.escalators
  let arrayCells = theArray $ Map.toUnfoldable newCells
  let xs = map (\(Tuple (MapPoint { x, y }) v) -> x) arrayCells
  let ys = map (\(Tuple (MapPoint { x, y }) v) -> y) arrayCells
  newBorders <- { up: _, down: _, left: _, right: _ } <$> minimum ys <*> maximum ys <*> minimum xs <*> maximum xs
  if length newCells /= length cur.cells + 16 then Nothing else
    pure $ cur { cells = newCells, borders = newBorders, escalators = newEscalators }

-- TODO calculate STExplore dir from map tile cell's coordinate
initialTile :: Maze
initialTile =
    let noWalls = { right: false, down: false }
        downWall = { right: false, down: true }
        rightWall = { right: true, down: false }
        rdWall = { right: true, down: true }
        mp x y = MapPoint { x, y }
    in {
      cells: Map.fromFoldable [
        Tuple (MapPoint { x: 0, y: 0 }) { walls: rdWall, special: Just STUnwalkable },
        Tuple (MapPoint { x: 1, y: 0 }) { walls: noWalls, special: Nothing },
        Tuple (MapPoint { x: 2, y: 0 }) { walls: noWalls, special: Just (STExplore Yellow N) },
        Tuple (MapPoint { x: 3, y: 0 }) { walls: rdWall, special: Just (STWarp Green) },
        Tuple (MapPoint { x: 0, y: 1 }) { walls: rdWall, special: Just (STExplore Green W) },
        Tuple (MapPoint { x: 1, y: 1 }) { walls: noWalls, special: Nothing },
        Tuple (MapPoint { x: 2, y: 1 }) { walls: noWalls, special: Nothing },
        Tuple (MapPoint { x: 3, y: 1 }) { walls: rdWall, special: Just (STWarp Red) },
        Tuple (MapPoint { x: 0, y: 2 }) { walls: downWall, special: Just (STWarp Yellow) },
        Tuple (MapPoint { x: 1, y: 2 }) { walls: noWalls, special: Nothing },
        Tuple (MapPoint { x: 2, y: 2 }) { walls: noWalls, special: Nothing },
        Tuple (MapPoint { x: 3, y: 2 }) { walls: downWall, special: Just (STExplore Purple E) },
        Tuple (MapPoint { x: 0, y: 3 }) { walls: downWall, special: Just (STWarp Purple) },
        Tuple (MapPoint { x: 1, y: 3 }) { walls: noWalls, special: Just (STExplore Red S) },
        Tuple (MapPoint { x: 2, y: 3 }) { walls: downWall, special: Nothing },
        Tuple (MapPoint { x: 3, y: 3 }) { walls: rdWall, special: Just STTimer }
      ],
      escalators: Set.fromFoldable [ Tuple (mp 0 1) (mp 1 0)],
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
      warp c = Just (STWarp c)
      weapon c = Just (STWeapon c)
      mp x y = MapPoint { x, y }
      mk x y walls special =
        Tuple (mp x y) { walls, special }
  in [{
      entrance: { side: E, offset: 2 }, -- TODO calculate from .cells
      escalators: Set.empty,
      cells: Map.fromFoldable [
        mk 0 0 rightWall (warp Yellow),
        mk 1 0 rdWall unwalk,
        mk 2 0 rightWall (exp Green N),
        mk 3 0 rightWall unwalk,
        mk 0 1 downWall (exp Purple W),
        mk 1 1 downWall Nothing,
        mk 2 1 rightWall Nothing,
        mk 3 1 rdWall unwalk,
        mk 0 2 rightWall unwalk,
        mk 1 2 downWall (Just STTimer),
        mk 2 2 noWalls Nothing,
        mk 3 2 downWall (Just STEntrance),
        mk 0 3 downWall unwalk,
        mk 1 3 rdWall unwalk,
        mk 2 3 rdWall (warp Red),
        mk 3 3 rdWall unwalk
      ]
    }, {
      entrance: { side: S, offset: 1 },
      escalators: Set.empty,
      cells: Map.fromFoldable [
        mk 0 0 noWalls Nothing,
        mk 1 0 rightWall Nothing,
        mk 2 0 rightWall (warp Red),
        mk 3 0 rdWall unwalk,
        mk 0 1 rdWall (exp Purple W),
        mk 1 1 noWalls Nothing,
        mk 2 1 downWall Nothing,
        mk 3 1 rdWall (warp Green),
        mk 0 2 downWall (Just STTimer),
        mk 1 2 downWall Nothing,
        mk 2 2 noWalls Nothing,
        mk 3 2 downWall (exp Yellow E),
        mk 0 3 rdWall unwalk,
        mk 1 3 noWalls (Just STEntrance),
        mk 2 3 rdWall Nothing,
        mk 3 3 rdWall unwalk
      ]
    }, {
      entrance: { side: W, offset: 1 },
      escalators: Set.fromFoldable [ Tuple (mp 2 0) (mp 3 2) ],
      cells: Map.fromFoldable [
        mk 0 0 downWall (warp Green),
        mk 1 0 noWalls Nothing,
        mk 2 0 rdWall (exp Red N),
        mk 3 0 rightWall unwalk,
        mk 0 1 noWalls (Just STEntrance),
        mk 1 1 rdWall Nothing,
        mk 2 1 noWalls unwalk,
        mk 3 1 rdWall unwalk,
        mk 0 2 rdWall (warp Purple),
        mk 1 2 noWalls unwalk,
        mk 2 2 rightWall unwalk,
        mk 3 2 rightWall Nothing,
        mk 0 3 downWall unwalk,
        mk 1 3 downWall unwalk,
        mk 2 3 rdWall unwalk,
        mk 3 3 rightWall (Just (STExit Purple S))
      ]
    }, {
      entrance: { side: E, offset: 2 },
      escalators: Set.empty,
      cells: Map.fromFoldable [
        mk 0 0 noWalls Nothing,
        mk 1 0 noWalls Nothing,
        mk 2 0 downWall (exp Yellow N),
        mk 3 0 rdWall (weapon Purple),
        mk 0 1 rightWall Nothing,
        mk 1 1 rightWall (warp Red),
        mk 2 1 downWall unwalk,
        mk 3 1 rdWall unwalk,
        mk 0 2 rightWall Nothing,
        mk 1 2 rdWall unwalk,
        mk 2 2 noWalls Nothing,
        mk 3 2 downWall (Just STEntrance),
        mk 0 3 downWall Nothing,
        mk 1 3 downWall Nothing,
        mk 2 3 rdWall Nothing,
        mk 3 3 rdWall unwalk
      ]
    }, {
      entrance: { side: N, offset: 2 },
      escalators: Set.empty,
      cells: Map.fromFoldable [
        mk 0 0 noWalls Nothing,
        mk 1 0 downWall Nothing,
        mk 2 0 downWall (Just STEntrance),
        mk 3 0 rdWall (weapon Green),
        mk 0 1 rightWall (exp Purple W),
        mk 1 1 downWall unwalk,
        mk 2 1 noWalls unwalk,
        mk 3 1 rdWall unwalk,
        mk 0 2 rightWall Nothing,
        mk 1 2 rightWall (warp Yellow),
        mk 2 2 rdWall unwalk,
        mk 3 2 noWalls (exp Red E),
        mk 0 3 downWall Nothing,
        mk 1 3 downWall Nothing,
        mk 2 3 downWall Nothing,
        mk 3 3 rdWall Nothing
      ]
    }, {
      entrance: { side: N, offset: 2 },
      escalators: Set.fromFoldable [ Tuple (mp 2 0) (mp 1 2) ],
      cells: Map.fromFoldable [
        mk 0 0 rightWall (warp Yellow),
        mk 1 0 rightWall unwalk,
        mk 2 0 rdWall (Just STEntrance),
        mk 3 0 rdWall unwalk,
        mk 0 1 rightWall (exp Purple W),
        mk 1 1 downWall unwalk,
        mk 2 1 rdWall unwalk,
        mk 3 1 rightWall (weapon Red),
        mk 0 2 downWall Nothing,
        mk 1 2 noWalls Nothing,
        mk 2 2 downWall Nothing,
        mk 3 2 rdWall Nothing,
        mk 0 3 rdWall unwalk,
        mk 1 3 rdWall (warp Green),
        mk 2 3 downWall unwalk,
        mk 3 3 rdWall unwalk
      ]
    }, {
      entrance: { side: W, offset: 1 },
      escalators: Set.empty,
      cells: Map.fromFoldable [
        mk 0 0 rightWall (warp Purple),
        mk 1 0 rdWall unwalk,
        mk 2 0 rightWall (exp Red N),
        mk 3 0 rightWall unwalk,
        mk 0 1 downWall (Just STEntrance),
        mk 1 1 rightWall Nothing,
        mk 2 1 rightWall Nothing,
        mk 3 1 rdWall unwalk,
        mk 0 2 rightWall unwalk,
        mk 1 2 noWalls Nothing,
        mk 2 2 downWall Nothing,
        mk 3 2 downWall (exp Green E),
        mk 0 3 rightWall unwalk,
        mk 1 3 rdWall (weapon Yellow),
        mk 2 3 downWall unwalk,
        mk 3 3 rdWall unwalk
      ]
    }, {
      entrance: { side: E, offset: 2 },
      escalators: Set.empty,
      cells: Map.fromFoldable [
        mk 0 0 noWalls Nothing,
        mk 1 0 downWall Nothing,
        mk 2 0 rightWall (exp Green N),
        mk 3 0 rdWall unwalk,
        mk 0 1 rdWall (exp Red W),
        mk 1 1 downWall (Just STTimer),
        mk 2 1 noWalls Nothing,
        mk 3 1 rightWall Nothing,
        mk 0 2 noWalls Nothing,
        mk 1 2 downWall Nothing,
        mk 2 2 rightWall Nothing,
        mk 3 2 downWall (Just STEntrance),
        mk 0 3 downWall Nothing,
        mk 1 3 rightWall (exp Yellow S),
        mk 2 3 rdWall (warp Purple),
        mk 3 3 rdWall unwalk
      ]
    }
  ]
