module GFX.UIOverlay where

import Color (rgba, white)
import Data.Map as Map
import Data.Maybe (maybe)
import GFX.Util (renderText)
import Graphics.Drawing (Drawing, translate, scale, image, filled, fillColor, rectangle)
import Prelude
import Types (Assets, AssetName (ACard, ANametag, AOverlay), GameState, GameStatus (..))

overlay :: { w :: Number, h :: Number } -> GameState -> Assets -> Drawing
overlay scrDims gameState assets =
  let overlayDims = { w: 1479.0, h: 283.0 }
      clip x a b = if x < a then a else if x > b then b else x
      overlayScale = clip (scrDims.w/overlayDims.w) 0.0 0.5
      overlayTranslate = translate ((scrDims.w - (overlayDims.w * overlayScale))/2.0)
        (scrDims.h - (overlayDims.h * overlayScale))
      card = translate (997.0 * overlayScale) (80.0 * overlayScale) $
        overlayTranslate $ scale (2.0 * overlayScale) (2.0 * overlayScale) $
          maybe mempty image (Map.lookup (ACard gameState.allowedDir) assets)
      overl = (overlayTranslate $ scale overlayScale overlayScale $ maybe mempty image (Map.lookup AOverlay assets)) <> card
      gameOverOverlay = if gameState.status == Won || gameState.status == Lost then (filled (fillColor (rgba 0 0 0 0.3)) $ rectangle 0.0 0.0 scrDims.w scrDims.h) <> renderText (scrDims.w/2.0) (scrDims.h/2.0) white 20 (show gameState.status) else mempty
  in overl <> gameOverOverlay
  -- 11.718
