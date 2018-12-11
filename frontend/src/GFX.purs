module GFX where

import Prelude
import Graphics.Drawing (Drawing, filled, svgPath, fillColor, outlined, outlineColor, translate)
import Color (Color, rgb)
import Data.Array ((..))
import Data.Foldable (foldMap)

type ColorSet = { base :: Color, light1 :: Color, light2 :: Color }

cell' :: Boolean -> Boolean -> Boolean -> Drawing
cell' walkable =
  if walkable then
    cell (rgb 0xE6 0xF1 0xF8) (rgb 0xC0 0xDB 0xEC)
  else
    cell (rgb 0x10 0x10 0x10) (rgb 0x00 0x00 0x00)

cell :: Color -> Color -> Boolean -> Boolean -> Drawing
cell bright dark leftEdge rightEdge =
    filled (fillColor bright) (svgPath "M0 32L64 0L128 32L64 64L0 32Z")
 <> outlined (outlineColor dark) (svgPath "M0 32L64 0L128 32L64 64L0 32Z")
 <> (if leftEdge then filled (fillColor dark) (svgPath "M0 32L64 64V72L0 40V32Z") else mempty)
 <> (if rightEdge then filled (fillColor dark) (svgPath "M128 32L64 64V72L128 40V32Z") else mempty)

-- FIXME light1 must = base, or wall corners will look weird
base :: ColorSet
base = { base: rgb 0x94 0xA7 0xC3, light1: rgb 0x94 0xA7 0xC3, light2: rgb 0xC0 0xD0 0xE7 }

-- Translations are relative to cell
wallDown :: Drawing
wallDown = translate 0.0 19.0 $ wallX base

wallRight :: Drawing
wallRight = translate 60.0 19.0 $ wallY base

wallSECorner' :: Drawing
wallSECorner' = translate 64.0 51.0 $ wallSECorner base.base

wallNWCorner' :: Drawing
wallNWCorner' = translate 60.0 49.0 $ wallNWCorner base.light2

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
