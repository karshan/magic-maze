{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Types where

import Data.Aeson.Types
import Data.Aeson.TH
import Data.Set (Set)
import PSBridge
import Protolude

data C2SCommand =
    CPlayerMove PlayerColor MapPoint MapPoint -- From To
  | CExplore MapPoint Dir
      deriving (Eq, Ord, Show)

data S2CCommand =
    SPlayerMove PlayerColor MapPoint
  | SExplore Int MapPoint Dir
  | SSetState ServerGameState
  | SSetAllowedDir Dir
      deriving (Eq, Ord, Show)

data PlayerColor =
    Red
  | Yellow
  | Green
  | Purple
      deriving (Eq, Ord, Show)
instance FromJSONKey PlayerColor
instance ToJSONKey PlayerColor

data Dir =
    N
  | E
  | S
  | W
      deriving (Eq, Ord, Show)

data MapPoint = MapPoint { _x :: Int, _y :: Int } deriving (Eq, Ord, Show)

instance Num MapPoint where
    (MapPoint x1 y1) + (MapPoint x2 y2) = MapPoint (x1 + x2) (y1 + y2)
    (MapPoint x1 y1) - (MapPoint x2 y2) = MapPoint (x1 - x2) (y1 - y2)
instance FromJSONKey MapPoint
instance ToJSONKey MapPoint

data DirMap v = DirMap { _left :: v, _up :: v, _right :: v, _down :: v } deriving (Eq, Ord, Show)

data SpecialTile =
    STUnwalkable
  | STEntrance
  | STExplore PlayerColor Dir
  | STWarp PlayerColor
  | STExit PlayerColor Dir
  | STWeapon PlayerColor
  | STTimer Bool -- isActive
      deriving (Eq, Ord, Show)

type Cells = Map MapPoint Cell
data Escalator = Escalator MapPoint MapPoint deriving (Eq, Ord, Show)
data Maze = Maze { _cells :: Cells, _borders :: DirMap Int, _escalators :: Set Escalator } deriving (Eq, Ord, Show)
data Entrance = Entrance { _side :: Dir, _offset :: Int } deriving (Eq, Ord, Show)
data Tile = Tile { _cells :: Cells, _entrance :: Entrance, _escalators :: Set Escalator } deriving (Eq, Ord, Show)
data Walls = Walls { _right :: Bool, _down :: Bool } deriving (Eq, Ord, Show)
data Cell = Cell { _walls :: Walls, _special :: Maybe SpecialTile } deriving (Eq, Ord, Show)

data GameStatus =
    Started
  | WeaponsAcquired
  | Won
  | Lost
      deriving (Eq, Ord, Show)

-- TODO nonempty map like dirmap
type PlayerPositions = Map PlayerColor MapPoint
data ServerGameState = ServerGameState {
    _maze :: Maze
  , _tiles :: [Tile]
  , _players :: PlayerPositions
  , _timer :: Int
  , _status :: GameStatus
  } deriving (Eq, Ord, Show)

$(deriveJSON purescriptOptions ''Dir)
$(deriveJSON purescriptOptions ''MapPoint)
$(deriveJSON purescriptOptions ''PlayerColor)
$(deriveJSON purescriptOptions ''C2SCommand)
$(deriveJSON purescriptOptions ''S2CCommand)
$(deriveJSON purescriptOptions ''SpecialTile)
$(deriveJSON purescriptOptions ''Walls)
$(deriveJSON purescriptOptions ''Cell)
$(deriveJSON purescriptOptions ''DirMap)
$(deriveJSON purescriptOptions ''Escalator)
$(deriveJSON purescriptOptions ''Maze)
$(deriveJSON purescriptOptions ''Entrance)
$(deriveJSON purescriptOptions ''Tile)
$(deriveJSON purescriptOptions ''GameStatus)
$(deriveJSON purescriptOptions ''ServerGameState)
