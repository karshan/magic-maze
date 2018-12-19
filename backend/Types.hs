{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Aeson.TH
import PSBridge

data Command =
    PlayerMove PlayerColor MapPoint
      deriving (Eq, Ord, Show)

data PlayerColor =
    Red
  | Yellow
  | Green
  | Purple
      deriving (Eq, Ord, Show)

data MapPoint = MapPoint { x :: Int, y :: Int } deriving (Eq, Ord, Show)

$(deriveJSON purescriptOptions ''MapPoint)
$(deriveJSON purescriptOptions ''PlayerColor)
$(deriveJSON purescriptOptions ''Command)
