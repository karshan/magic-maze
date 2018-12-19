module Tiles where

import Data.Map as Map
import Data.Tuple (Tuple (..))

import Types (MapPoint (..), Maze)

initialTile :: Maze
initialTile =
    let noWalls = { right: false, down: false }
        downWall = { right: false, down: true }
        rightWall = { right: true, down: false }
        rdWall = { right: true, down: true }
    in {
      cells: Map.fromFoldable [
        Tuple (MapPoint { x: 0, y: 0 }) { walls: rightWall, walkable: true },
        Tuple (MapPoint { x: 1, y: 0 }) { walls: noWalls, walkable: true },
        Tuple (MapPoint { x: 2, y: 0 }) { walls: downWall, walkable: true },
        Tuple (MapPoint { x: 3, y: 0 }) { walls: rightWall, walkable: true },
        Tuple (MapPoint { x: 0, y: 1 }) { walls: rightWall, walkable: true },
        Tuple (MapPoint { x: 1, y: 1 }) { walls: noWalls, walkable: true },
        Tuple (MapPoint { x: 2, y: 1 }) { walls: rightWall, walkable: true },
        Tuple (MapPoint { x: 3, y: 1 }) { walls: rightWall, walkable: true },
        Tuple (MapPoint { x: 0, y: 2 }) { walls: noWalls, walkable: true },
        Tuple (MapPoint { x: 1, y: 2 }) { walls: downWall, walkable: true },
        Tuple (MapPoint { x: 2, y: 2 }) { walls: rdWall, walkable: true },
        Tuple (MapPoint { x: 3, y: 2 }) { walls: noWalls, walkable: true },
        Tuple (MapPoint { x: 0, y: 3 }) { walls: downWall, walkable: true },
        Tuple (MapPoint { x: 1, y: 3 }) { walls: noWalls, walkable: true },
        Tuple (MapPoint { x: 2, y: 3 }) { walls: rdWall, walkable: true },
        Tuple (MapPoint { x: 3, y: 3 }) { walls: noWalls, walkable: false }
      ],
      borders: {
        left: 0,
        up: 0,
        right: 3,
        down: 3
      }
    }
