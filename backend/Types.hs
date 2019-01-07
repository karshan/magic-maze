{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Types where

import Data.Aeson.Types
import Data.Aeson.TH
import Data.Set (Set)
import PSBridge
import Protolude

data Command =
    PlayerMove PlayerColor MapPoint
  | Explore MapPoint Dir
  | SetState ServerGameState
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

data MapPoint = MapPoint { x :: Int, y :: Int } deriving (Eq, Ord, Show)
instance FromJSONKey MapPoint
instance ToJSONKey MapPoint

data DirMap v = DirMap { left :: v, up :: v, right :: v, down :: v } deriving (Eq, Ord, Show)

data SpecialTile =
    STUnwalkable
  | STEntrance
  | STExplore PlayerColor Dir
  | STWarp PlayerColor
  | STExit PlayerColor Dir
  | STWeapon PlayerColor
  | STTimer
      deriving (Eq, Ord, Show)

type Cells = Map MapPoint Cell
data Escalator = Escalator MapPoint MapPoint deriving (Eq, Ord, Show)
data Maze = Maze { cells :: Cells, borders :: DirMap Int, escalators :: Set Escalator } deriving (Eq, Ord, Show)
data Entrance = Entrance { side :: Dir, offset :: Int } deriving (Eq, Ord, Show)
data Tile = Tile { cells :: Cells, entrance :: Entrance, escalators :: Set Escalator } deriving (Eq, Ord, Show)
data Walls = Walls { right :: Bool, down :: Bool } deriving (Eq, Ord, Show)
data Cell = Cell { walls :: Walls, special :: Maybe SpecialTile } deriving (Eq, Ord, Show)

-- TODO nonempty map like dirmap
type PlayerPositions = Map PlayerColor MapPoint
data ServerGameState = ServerGameState { maze :: Maze, tiles :: [Tile], players :: PlayerPositions } deriving (Eq, Ord, Show)

$(deriveJSON purescriptOptions ''Dir)
$(deriveJSON purescriptOptions ''MapPoint)
$(deriveJSON purescriptOptions ''PlayerColor)
$(deriveJSON purescriptOptions ''Command)
$(deriveJSON purescriptOptions ''SpecialTile)
$(deriveJSON purescriptOptions ''Walls)
$(deriveJSON purescriptOptions ''Cell)
$(deriveJSON purescriptOptions ''DirMap)
$(deriveJSON purescriptOptions ''Escalator)
$(deriveJSON purescriptOptions ''Maze)
$(deriveJSON purescriptOptions ''Entrance)
$(deriveJSON purescriptOptions ''Tile)
$(deriveJSON purescriptOptions ''ServerGameState)
