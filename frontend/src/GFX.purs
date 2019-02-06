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

-- TODO lol flipVertical is actually flipHorizontal rename everywhere
flipVertical = scale (-1.0) 1.0
flipHorizontal = scale 1.0 (-1.0)

-- FIXME all the translations are a bit off. fix with the help of figma
drawCellExplore :: Dir -> Drawing -> Drawing
drawCellExplore dir =
  case dir of
    N -> translate (51.0 - tileHalfWidth) (-3.0)
    S -> translate (80.0 - tileHalfWidth) (2.0 * tileHalfHeight) <<< flipVertical <<< flipHorizontal
    E -> translate (-5.0) (2.0 * tileHalfHeight) <<< flipHorizontal
    W -> translate 20.0 (-5.0) <<< flipVertical

exitWidth = 61.0
exitHeight = 56.0

drawCellExit :: Dir -> Drawing -> Drawing
drawCellExit dir =
  let center = translate (-exitWidth/2.0) (-exitHeight/2.0)
      uncenter = translate (exitWidth/2.0) (exitHeight/2.0)
  in case dir of
    N -> translate (35.0 - tileHalfWidth) 8.0 <<< uncenter <<< flipVertical <<< center
    S -> translate (32.0 - tileHalfWidth) 3.0 <<< uncenter <<< flipHorizontal <<< center
    E -> translate (32.0 - tileHalfWidth) 3.0 <<< uncenter <<< flipHorizontal <<< flipVertical <<< center
    W -> translate (35.0 - tileHalfWidth) 8.0

drawCellWeapon :: PlayerColor -> Drawing -> Drawing
drawCellWeapon col =
  case col of
    Yellow -> translate (41.0 - tileHalfWidth) (-72.0)
    Red -> translate (39.0 - tileHalfWidth) (-25.0)
    Green -> translate (26.0 - tileHalfWidth) (-80.0)
    Purple -> translate (32.0 - tileHalfWidth) (-48.0)

drawCellWarp :: Drawing -> Drawing
drawCellWarp = translate (-tileHalfWidth + 27.0) 0.0

cell :: Assets -> Maybe SpecialTile -> Boolean -> Boolean -> Drawing
cell _ (Just STUnwalkable) le re = mempty
cell assets (Just (STExplore col dir)) le re = cell assets Nothing le re <>
  drawCellExplore dir (maybe mempty image $ lookup (AExplore col) assets)
cell assets (Just (STWarp col)) le re = cell assets Nothing le re <>
  drawCellWarp (maybe mempty image $ lookup (AWarp col) assets)
cell assets (Just (STExit col dir)) le re = cell assets Nothing le re <>
  drawCellExit dir (maybe mempty image $ lookup (AExit col) assets)
cell assets (Just (STTimer active)) le re = cell assets Nothing le re <>
  (translate (-tileHalfWidth + 54.0) 10.0 (maybe mempty image $ lookup (if active then AHourglassRed else AHourglassBlack) assets))
cell assets _ le re = translate (-tileHalfWidth) 0.0 $
    maybe mempty image (lookup ACellTop assets)
 <> translate 0.0 tileHalfHeight (guard le (maybe mempty image $ lookup ACellBottomLeft assets))
 <> translate tileHalfWidth tileHalfHeight (guard re (maybe mempty image $ lookup ACellBottomRight assets))
  
cellWeapon :: Assets -> Maybe SpecialTile -> Drawing
cellWeapon assets (Just (STWeapon col)) = drawCellWeapon col (maybe mempty image $ lookup (AWeapon col) assets)
cellWeapon _ _ = mempty

-- TODO light1 must = base, or wall corners will look weird
baseColors :: ColorSet
baseColors = { base: rgb 0x94 0xA7 0xC3, light1: rgb 0x94 0xA7 0xC3, light2: rgb 0xC0 0xD0 0xE7 }

wallDown :: Drawing
wallDown = translate (-tileHalfWidth) 19.0 $ wallX baseColors

wallRight :: Drawing
wallRight = translate (-4.0) 19.0 $ wallY baseColors

wallSECorner' :: Drawing
wallSECorner' = translate 0.0 51.0 $ wallSECorner baseColors.base

wallNWCorner' :: Drawing
wallNWCorner' = translate (-4.0) 49.0 $ wallNWCorner baseColors.light2

wallX :: ColorSet -> Drawing
wallX c =
    filled (fillColor c.light1) (svgPath "M68 32L64 34V45L68 43V32Z")
 <> filled (fillColor c.base) (svgPath "M0 2L64 34V45L0 13V2Z")
 <> filled (fillColor c.light2) (svgPath "M68 32L4 0L0 2L64 34L68 32Z")

wallY :: ColorSet -> Drawing
wallY c =
    filled (fillColor c.light1) (svgPath "M0 32L4 34V45L0 43L0 32Z")
 <> filled (fillColor c.base) (svgPath "M68 2L4 34V45L68 13V2Z")
 <> filled (fillColor c.light2) (svgPath "M0 32L64 0L68 2L4 34L0 32Z")

wallSECorner :: Color -> Drawing
wallSECorner c =
    filled (fillColor c) (svgPath "M4 0L0 2V13L4 11L4 0Z")

wallNWCorner :: Color -> Drawing
wallNWCorner c =
    filled (fillColor c) (svgPath "M0 2L4 0L8 2L4 4L0 2Z")

type Translation = Point
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
