module GFX.UIOverlay where

import Color (rgba, white)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (maybe)
import GFX.Util (renderText)
import Graphics.Drawing (Drawing, translate, scale, image, filled, fillColor, rectangle)
import Prelude
import Types

clip :: forall a. Ord a => a -> a -> a -> a
clip x a b = if x < a then a else if x > b then b else x

card :: Assets -> Dir -> Drawing
card assets d = image (assetLookup (ACard d) assets)

-- when implementing zoom, this function needs to change
-- TODO(codequality) somehow change this so we can import this from figma essentially
overlay :: { w :: Number, h :: Number } -> GameState -> Assets -> Drawing
overlay scrDims gameState assets =
  let overlayDims = { w: 1479.0, h: 283.0 }
      overlayScale = clip (scrDims.w/overlayDims.w) 0.0 0.5
      overlayTranslate = translate ((scrDims.w - (overlayDims.w * overlayScale))/2.0)
        (scrDims.h - (overlayDims.h * overlayScale))
      playerCardT = translate (997.0 * overlayScale) (80.0 * overlayScale) <<< overlayTranslate <<<
                      scale (2.0 * overlayScale) (2.0 * overlayScale)
      otherCardT n = translate ((997.0 - ((toNumber (n + 1)) * 250.0)) * overlayScale) (80.0 * overlayScale) <<< overlayTranslate <<<
                      scale (1.7 * overlayScale) (1.7 * overlayScale)
      rcard = playerCardT $ card assets gameState.allowedDir
      othercards = foldMapWithIndex (\i -> otherCardT i) $ Map.values $ map (card assets) gameState.clients
      overl = (overlayTranslate $ scale overlayScale overlayScale $ image (assetLookup AOverlay assets)) <> rcard <> othercards
      gameOverOverlay = if gameState.status == Won || gameState.status == Lost then (filled (fillColor (rgba 0 0 0 0.3)) $ rectangle 0.0 0.0 scrDims.w scrDims.h) <> renderText (scrDims.w/2.0) (scrDims.h/2.0) white 20 (show gameState.status) else mempty
      timer = translate (50.0 * overlayScale) (250.0 * overlayScale) $ overlayTranslate $ renderText 0.0 0.0 white 20 (show gameState.timer)
  in overl <> timer <> gameOverOverlay
