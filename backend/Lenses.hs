{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
module Lenses where

import           Control.Lens.TH (makeFieldsNoPrefix)
import Types

makeFieldsNoPrefix ''MapPoint
makeFieldsNoPrefix ''Walls
makeFieldsNoPrefix ''Cell
makeFieldsNoPrefix ''DirMap
makeFieldsNoPrefix ''Maze
makeFieldsNoPrefix ''Entrance
makeFieldsNoPrefix ''Tile
makeFieldsNoPrefix ''ServerGameState
