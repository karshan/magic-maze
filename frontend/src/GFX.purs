module GFX where

import Prelude
import Graphics.Drawing (Drawing, Point, filled, rectangle, svgPath, fillColor, outlined, outlineColor, scale, translate, image)
import Color (Color, rgb, rgba)
import Data.Array ((..))
import Data.Foldable (foldMap, fold)
import Data.Maybe (Maybe (..), maybe)
import Data.Monoid (guard)
import Data.Map (lookup)
import Data.Int
import Types
import Isometric
import Signal.DOM (DimensionPair)

type ColorSet = { base :: Color, light1 :: Color, light2 :: Color }

drawCellWeapon :: PlayerColor -> Drawing -> Drawing
drawCellWeapon col =
  case col of
    Yellow -> translate (41.0 - tileHalfWidth) (-72.0)
    Red -> translate (39.0 - tileHalfWidth) (-25.0)
    Green -> translate (26.0 - tileHalfWidth) (-80.0)
    Purple -> translate (32.0 - tileHalfWidth) (-48.0)

cell :: Assets -> Maybe SpecialTile -> Boolean -> Boolean -> Drawing
cell _ (Just STUnwalkable) le re = mempty
cell assets (Just (STExplore col dir)) le re = cell assets Nothing le re <>
  image (assetLookup (AExplore col dir) assets)
cell assets (Just (STWarp col)) le re = cell assets Nothing le re <>
  image (assetLookup (AWarp col) assets)
cell assets (Just (STExit col dir)) le re = cell assets Nothing le re <>
  image (assetLookup (AExit dir) assets)
cell assets (Just (STTimer active)) le re = cell assets Nothing le re <>
  image (assetLookup (if active then AHourglassRed else AHourglassBlack) assets)
cell assets _ le re =
    image (assetLookup ACellTop assets)
 <> translate 0.0 tileHalfHeight (guard le (image $ assetLookup ACellBottomLeft assets))
 <> translate tileHalfWidth tileHalfHeight (guard re (image $ assetLookup ACellBottomRight assets))

cellWeapon :: Assets -> Maybe SpecialTile -> Drawing
cellWeapon assets (Just (STWeapon col)) = drawCellWeapon col (image $ assetLookup (AWeapon col) assets)
cellWeapon _ _ = mempty

type Translation = Point
-- TODO playerDims :: Dimensions
playerWidth :: Number
playerWidth = 42.0
playerHeight :: Number
playerHeight = 69.0
playerCellT :: Translation
playerCellT = { x: -playerWidth/2.0, y: -26.0 }

-- Relative to player top left corner
playerCenterT :: Translation
playerCenterT = { x: playerWidth/2.0, y: playerHeight - 14.0 }

playerCell :: Drawing -> Drawing
playerCell = translate playerCellT.x playerCellT.y

playerBBox :: DimensionPair -> MapPoint -> Rect
playerBBox dims mp =
  let (ScreenPoint p) = mapToScreen dims mp
  in {
    x: p.x + playerCellT.x,
    y: p.y + playerCellT.y,
    w: playerWidth,
    h: playerHeight
  }

bgWidth :: Number
bgWidth = 740.0
bgHeight :: Number
bgHeight = 486.0

iterate :: forall a. (a -> a) -> Int -> a -> a
iterate _ 0 a = a
iterate f n a = iterate f (n - 1) (f a)

background :: DimensionPair -> Drawing -> Drawing
background dims bg =
  let r = translate bgWidth 0.0
      d = translate 0.0 bgHeight
  in fold do
  x <- 0 .. (ceil (toNumber dims.w/bgWidth))
  y <- 0 .. (ceil (toNumber dims.h/bgHeight))
  pure $ iterate r x $ iterate d y $ bg
