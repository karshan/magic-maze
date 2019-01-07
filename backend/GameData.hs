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
      escalators = Set.fromList [ Escalator (mp 0 1) (mp 1 0) ],
      borders = DirMap {
        left = 0,
        up = 0,
        right = 3,
        down = 3
      }
    }

tiles :: [Tile]
tiles =
  let noWalls = Walls { right = False, down = False }
      downWall = Walls { right = False, down = True }
      rightWall = Walls { right = True, down = False }
      rdWall = Walls { right = True, down = True }
      unwalk = Just STUnwalkable
      exp c d = Just (STExplore c d)
      warp c = Just (STWarp c)
      weapon c = Just (STWeapon c)
      mp x y = MapPoint x y
      mk x y walls special =
        (mp x y, Cell walls special)
  in [ Tile {
      entrance = Entrance { side = E, offset = 2 }, -- TODO calculate from .cells
      escalators = Set.empty,
      cells = Map.fromList [
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
    }, Tile {
      entrance = Entrance { side = S, offset = 1 },
      escalators = Set.empty,
      cells = Map.fromList [
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
    }, Tile {
      entrance = Entrance { side = W, offset = 1 },
      escalators = Set.fromList [ Escalator (mp 2 0) (mp 3 2) ],
      cells = Map.fromList [
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
    }, Tile {
      entrance = Entrance { side = E, offset = 2 },
      escalators = Set.empty,
      cells = Map.fromList [
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
    }, Tile {
      entrance = Entrance { side = N, offset = 2 },
      escalators = Set.empty,
      cells = Map.fromList [
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
    }, Tile {
      entrance = Entrance { side = N, offset = 2 },
      escalators = Set.fromList [ Escalator (mp 2 0) (mp 1 2) ],
      cells = Map.fromList [
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
    }, Tile {
      entrance = Entrance { side = W, offset = 1 },
      escalators = Set.empty,
      cells = Map.fromList [
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
    }, Tile {
      entrance = Entrance { side = E, offset = 2 },
      escalators = Set.empty,
      cells = Map.fromList [
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
