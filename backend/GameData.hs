{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DuplicateRecordFields #-}
module GameData where

import Protolude
import Types
import qualified Data.Map as Map
import qualified Data.Set as Set

initialState :: ServerGameState
initialState = ServerGameState {
      maze = initialTile,
      tiles = [],
      players = Map.fromList [
        (Red, (MapPoint 1 1)),
        (Yellow, (MapPoint 2 1)),
        (Green, (MapPoint 1 2)),
        (Purple, (MapPoint 2 2))
      ]
    }

initialTile :: Maze
initialTile =
    let noWalls = Walls False False
        downWall = Walls False True
        rightWall = Walls True False
        rdWall = Walls True True
        mp x y = MapPoint x y
    in Maze {
      cells = Map.fromList [
        ((MapPoint { x = 0, y = 0 }), Cell { walls = rdWall, special = Just STUnwalkable }),
        ((MapPoint { x = 1, y = 0 }), Cell { walls = noWalls, special = Nothing }),
        ((MapPoint { x = 2, y = 0 }), Cell { walls = noWalls, special = Just (STExplore Yellow N) }),
        ((MapPoint { x = 3, y = 0 }), Cell { walls = rdWall, special = Just (STWarp Green) }),
        ((MapPoint { x = 0, y = 1 }), Cell { walls = rdWall, special = Just (STExplore Green W) }),
        ((MapPoint { x = 1, y = 1 }), Cell { walls = noWalls, special = Nothing }),
        ((MapPoint { x = 2, y = 1 }), Cell { walls = noWalls, special = Nothing }),
        ((MapPoint { x = 3, y = 1 }), Cell { walls = rdWall, special = Just (STWarp Red) }),
        ((MapPoint { x = 0, y = 2 }), Cell { walls = downWall, special = Just (STWarp Yellow) }),
        ((MapPoint { x = 1, y = 2 }), Cell { walls = noWalls, special = Nothing }),
        ((MapPoint { x = 2, y = 2 }), Cell { walls = noWalls, special = Nothing }),
        ((MapPoint { x = 3, y = 2 }), Cell { walls = downWall, special = Just (STExplore Purple E) }),
        ((MapPoint { x = 0, y = 3 }), Cell { walls = downWall, special = Just (STWarp Purple) }),
        ((MapPoint { x = 1, y = 3 }), Cell { walls = noWalls, special = Just (STExplore Red S) }),
        ((MapPoint { x = 2, y = 3 }), Cell { walls = downWall, special = Nothing }),
        ((MapPoint { x = 3, y = 3 }), Cell { walls = rdWall, special = Just STTimer })
      ],
      escalators = Set.fromList [ Escalator (mp 0 1) (mp 2 0) ],
      borders = DirMap {
        left = 0,
        up = 0,
        right = 3,
        down = 3
      }
    }
