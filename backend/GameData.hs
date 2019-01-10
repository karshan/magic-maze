{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DuplicateRecordFields #-}
module GameData where

import Protolude
import Types
import qualified Data.Map as Map
import qualified Data.Set as Set

initialState :: ServerGameState
initialState = ServerGameState {
      _maze = initialTile,
      _tiles = tiles,
      _players = Map.fromList [
        (Red, (MapPoint 1 1)),
        (Yellow, (MapPoint 2 1)),
        (Green, (MapPoint 1 2)),
        (Purple, (MapPoint 2 2))
      ],
      _timer = 150,
      _status = Started
    }

initialTile :: Maze
initialTile =
    let noWalls = Walls False False
        downWall = Walls False True
        rightWall = Walls True False
        rdWall = Walls True True
        mp x y = MapPoint x y
    in Maze {
      _cells = Map.fromList [
        ((MapPoint { _x = 0, _y = 0 }), Cell { _walls = rdWall, _special = Just STUnwalkable }),
        ((MapPoint { _x = 1, _y = 0 }), Cell { _walls = noWalls, _special = Nothing }),
        ((MapPoint { _x = 2, _y = 0 }), Cell { _walls = noWalls, _special = Just (STExplore Yellow N) }),
        ((MapPoint { _x = 3, _y = 0 }), Cell { _walls = rdWall, _special = Just (STWarp Green) }),
        ((MapPoint { _x = 0, _y = 1 }), Cell { _walls = rdWall, _special = Just (STExplore Green W) }),
        ((MapPoint { _x = 1, _y = 1 }), Cell { _walls = noWalls, _special = Nothing }),
        ((MapPoint { _x = 2, _y = 1 }), Cell { _walls = noWalls, _special = Nothing }),
        ((MapPoint { _x = 3, _y = 1 }), Cell { _walls = rdWall, _special = Just (STWarp Red) }),
        ((MapPoint { _x = 0, _y = 2 }), Cell { _walls = downWall, _special = Just (STWarp Yellow) }),
        ((MapPoint { _x = 1, _y = 2 }), Cell { _walls = noWalls, _special = Nothing }),
        ((MapPoint { _x = 2, _y = 2 }), Cell { _walls = noWalls, _special = Nothing }),
        ((MapPoint { _x = 3, _y = 2 }), Cell { _walls = downWall, _special = Just (STExplore Purple E) }),
        ((MapPoint { _x = 0, _y = 3 }), Cell { _walls = downWall, _special = Just (STWarp Purple) }),
        ((MapPoint { _x = 1, _y = 3 }), Cell { _walls = noWalls, _special = Just (STExplore Red S) }),
        ((MapPoint { _x = 2, _y = 3 }), Cell { _walls = downWall, _special = Nothing }),
        ((MapPoint { _x = 3, _y = 3 }), Cell { _walls = rdWall, _special = Just (STTimer True) })
      ],
      _escalators = Set.fromList [ Escalator (mp 0 1) (mp 1 0) ],
      _borders = DirMap {
        _left = 0,
        _up = 0,
        _right = 3,
        _down = 3
      }
    }

tiles :: [Tile]
tiles =
  let noWalls = Walls { _right = False, _down = False }
      downWall = Walls { _right = False, _down = True }
      rightWall = Walls { _right = True, _down = False }
      rdWall = Walls { _right = True, _down = True }
      unwalk = Just STUnwalkable
      exp c d = Just (STExplore c d)
      warp c = Just (STWarp c)
      weapon c = Just (STWeapon c)
      mp x y = MapPoint x y
      mk x y walls special =
        (mp x y, Cell walls special)
  in [ Tile {
      _entrance = Entrance { _side = E, _offset = 2 }, -- TODO calculate from .cells
      _escalators = Set.empty,
      _cells = Map.fromList [
        mk 0 0 rightWall (warp Yellow),
        mk 1 0 rdWall unwalk,
        mk 2 0 rightWall (exp Green N),
        mk 3 0 rightWall unwalk,
        mk 0 1 downWall (exp Purple W),
        mk 1 1 downWall Nothing,
        mk 2 1 rightWall Nothing,
        mk 3 1 rdWall unwalk,
        mk 0 2 rightWall unwalk,
        mk 1 2 downWall (Just (STTimer True)),
        mk 2 2 noWalls Nothing,
        mk 3 2 downWall (Just STEntrance),
        mk 0 3 downWall unwalk,
        mk 1 3 rdWall unwalk,
        mk 2 3 rdWall (warp Red),
        mk 3 3 rdWall unwalk
      ]
    }, Tile {
      _entrance = Entrance { _side = S, _offset = 1 },
      _escalators = Set.empty,
      _cells = Map.fromList [
        mk 0 0 noWalls Nothing,
        mk 1 0 rightWall Nothing,
        mk 2 0 rightWall (warp Red),
        mk 3 0 rdWall unwalk,
        mk 0 1 rdWall (exp Purple W),
        mk 1 1 noWalls Nothing,
        mk 2 1 downWall Nothing,
        mk 3 1 rdWall (warp Green),
        mk 0 2 downWall (Just (STTimer True)),
        mk 1 2 downWall Nothing,
        mk 2 2 noWalls Nothing,
        mk 3 2 downWall (exp Yellow E),
        mk 0 3 rdWall unwalk,
        mk 1 3 noWalls (Just STEntrance),
        mk 2 3 rdWall Nothing,
        mk 3 3 rdWall unwalk
      ]
    }, Tile {
      _entrance = Entrance { _side = W, _offset = 1 },
      _escalators = Set.fromList [ Escalator (mp 2 0) (mp 3 2) ],
      _cells = Map.fromList [
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
      _entrance = Entrance { _side = E, _offset = 2 },
      _escalators = Set.empty,
      _cells = Map.fromList [
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
      _entrance = Entrance { _side = N, _offset = 2 },
      _escalators = Set.empty,
      _cells = Map.fromList [
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
      _entrance = Entrance { _side = N, _offset = 2 },
      _escalators = Set.fromList [ Escalator (mp 2 0) (mp 1 2) ],
      _cells = Map.fromList [
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
      _entrance = Entrance { _side = W, _offset = 1 },
      _escalators = Set.empty,
      _cells = Map.fromList [
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
      _entrance = Entrance { _side = E, _offset = 2 },
      _escalators = Set.empty,
      _cells = Map.fromList [
        mk 0 0 noWalls Nothing,
        mk 1 0 downWall Nothing,
        mk 2 0 rightWall (exp Green N),
        mk 3 0 rdWall unwalk,
        mk 0 1 rdWall (exp Red W),
        mk 1 1 downWall (Just (STTimer True)),
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
