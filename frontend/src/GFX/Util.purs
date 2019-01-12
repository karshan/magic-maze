module GFX.Util where

import Prelude
import Graphics.Drawing (Color, Drawing, fillColor, text)
import Graphics.Drawing.Font (font, monospace)

renderText :: Number -> Number -> Color -> Int -> String -> Drawing
renderText x y c fontSize s = text (font monospace fontSize mempty) x y (fillColor c) s
