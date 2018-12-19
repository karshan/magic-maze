module Tiles where

import Types

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

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
        Tuple (MapPoint { x: 0, y: 1 }) { walls: rdWall, special: Just (STExplore Green W) },
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

{-
initialTile_Scenario3 :: Maze
initialTile_Scenario3 =
    let noWalls = { right: false, down: false }
        downWall = { right: false, down: true }
        rightWall = { right: true, down: false }
        rdWall = { right: true, down: true }
    in {
      cells: Map.fromFoldable [
        Tuple (MapPoint { x: 0, y: 0 }) { walls: rightWall, walkable: true, explore: Nothing },
        Tuple (MapPoint { x: 1, y: 0 }) { walls: noWalls, walkable: true, explore: Nothing },
        Tuple (MapPoint { x: 2, y: 0 }) { walls: downWall, walkable: true, explore: Nothing },
        Tuple (MapPoint { x: 3, y: 0 }) { walls: rightWall, walkable: true, explore: Nothing },
        Tuple (MapPoint { x: 0, y: 1 }) { walls: rightWall, walkable: true, explore: Nothing },
        Tuple (MapPoint { x: 1, y: 1 }) { walls: noWalls, walkable: true, explore: Nothing },
        Tuple (MapPoint { x: 2, y: 1 }) { walls: rightWall, walkable: true, explore: Nothing },
        Tuple (MapPoint { x: 3, y: 1 }) { walls: rightWall, walkable: true, explore: Nothing },
        Tuple (MapPoint { x: 0, y: 2 }) { walls: noWalls, walkable: true, explore: Nothing },
        Tuple (MapPoint { x: 1, y: 2 }) { walls: downWall, walkable: true, explore: Nothing },
        Tuple (MapPoint { x: 2, y: 2 }) { walls: rdWall, walkable: true, explore: Nothing },
        Tuple (MapPoint { x: 3, y: 2 }) { walls: noWalls, walkable: true, explore: Nothing },
        Tuple (MapPoint { x: 0, y: 3 }) { walls: downWall, walkable: true, explore: Nothing },
        Tuple (MapPoint { x: 1, y: 3 }) { walls: noWalls, walkable: true, explore: Nothing },
        Tuple (MapPoint { x: 2, y: 3 }) { walls: rdWall, walkable: true, explore: Nothing },
        Tuple (MapPoint { x: 3, y: 3 }) { walls: noWalls, walkable: false, explore: Nothing }
      ],
      borders: {
        left: 0,
        up: 0,
        right: 3,
        down: 3
      }
    }
-}
