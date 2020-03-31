module Main where

import Color (rgba, white, black)
import DOM (onDOMContentLoaded)
import Data.Foldable (foldl, foldMap, length)
import Data.FoldableWithIndex (foldWithIndexM, foldMapWithIndex)
import Data.Int (floor, toNumber)
import Data.Lens ((^.))
import Data.List (head) as L
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Maybe.First (First(..))
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import GFX as GFX
import GFX.Cell (drawCell, drawCellWall, drawCellWeapon)
import GFX.UIOverlay (overlay)
import GFX.Util (renderText)
import GameLogic (gameLogic, initialState)
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, getCanvasElementById, getContext2D, setCanvasHeight, setCanvasWidth, tryLoadImage, canvasElementToImageSource)
import Graphics.Drawing (Drawing, Point, filled, image, lineWidth, outlineColor, outlined, path, rectangle, translate)
import Graphics.Drawing (fillColor, render, scale) as D
import Isometric (mapToScreen, mapToScreenD, tileHalfHeight, tileHalfWidth)
import Prelude
import Signal (Signal, sampleOn, runSignal, constant, map2, merge)
import Signal.Channel (channel, send, subscribe)
import Signal.DOM (DimensionPair, MouseButton(..), animationFrame, keyPressed, mouseButtonPressed, mousePos, windowDimensions)
import Signal.Effect (foldEffect, mapEffect)
import Signal.MouseWheel (create) as Wheel
import Signal.Touch (create) as Touch
import Signal.Touch
import Web.TouchEvent.Touch (pageX, pageY)
import Signal.Time (every)
import Signal.WebSocket (create) as WS
import Types (AssetName(..), GAssets, Assets, DirMap(..), Dir(..), Escalator(..), GameState, GameStatus(..), Inputs(..), MapPoint, Maze, PlayerColor(..), assetLookup, cells, down, escalators, forAllCells, left, right, toPoint, up, wepacq)
import Unsafe.Coerce (unsafeCoerce) -- TODO(codequality) move to purescript-canvas
import Web.DOM.Document (createElement)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document, location)
import Web.HTML.Location (host, pathname, protocol)
import Record.Extra

translate' :: Point -> Drawing -> Drawing
translate' { x, y } = translate x y

keycodes :: DirMap Int
keycodes = DirMap {
        left: 37,
        up: 38,
        right: 39,
        down: 40
    }

-- TODO implement user changeable zoom
zoom :: Number
zoom = 1.0

drawPlayer :: DimensionPair -> MapPoint -> Maybe Point -> Point -> CanvasImageSource -> Drawing
drawPlayer offscreenDims playerPosition mDragPoint realMouse canvasImage =
    (maybe
        (GFX.playerCell <<< mapToScreenD offscreenDims playerPosition)
        (\dragPoint -> translate' (realMouse - dragPoint))
        mDragPoint)
    (image canvasImage)

drawEscalator :: DimensionPair -> MapPoint -> MapPoint -> Drawing
drawEscalator offscreenDims mp1 mp2 =
  let mp2p = unwrap <<< mapToScreen offscreenDims
  in translate 0.0 (tileHalfHeight - 2.0) $
       outlined (lineWidth 4.0 <> outlineColor (rgba 120 120 120 1.0)) $
         path $ map mp2p [ mp1, mp2 ]

renderMaze :: Context2D -> { maze :: Maze, offscreenDims :: DimensionPair, mAssets :: Maybe Assets } -> Effect Unit
renderMaze ctx { maze, offscreenDims, mAssets } =
  maybe (pure unit) (\assets -> do
    D.render ctx (filled (D.fillColor (rgba 0 0 0 0.0)) (rectangle 0.0 0.0 (toNumber offscreenDims.w) (toNumber offscreenDims.h)))
    let rcells = forAllCells maze (drawCell assets offscreenDims (maze^.cells))
    -- FIXME draw border walls on left and top
    let rwalls = forAllCells maze (drawCellWall assets offscreenDims (maze^.cells))
    let rweapons = forAllCells maze (drawCellWeapon assets offscreenDims (maze^.cells))
    let rescalators = foldMap (\(Escalator mp1 mp2) -> drawEscalator offscreenDims mp1 mp2) (maze^.escalators)
    -- We translate the cell and walls so that Isometric.mapToScreen maps to the NW corner of the tile
    D.render ctx ((translate (-tileHalfWidth) 0.0 (rcells <> rwalls)) <> rescalators <> (if maze^.wepacq == false then rweapons else mempty)))
    mAssets

loading :: DimensionPair -> Drawing
loading screenDims =
    filled (D.fillColor black) (rectangle 0.0 0.0 (toNumber screenDims.w) (toNumber screenDims.h)) <>
      (renderText (toNumber screenDims.w/2.0) (toNumber screenDims.h/2.0) white 12 "Loading")

render :: String -> Context2D -> CanvasElement -> DimensionPair -> DimensionPair -> Maybe Assets -> Point -> Point -> GameState -> Effect Unit
render path ctx offscreenCanvas offscreenDims screenDims mAssets realMouse realTouchPos gameState =
  maybe (D.render ctx $ loading screenDims) (\assets -> do
    if gameState.status == Waiting then do
      let scrDims = { w: toNumber screenDims.w, h: toNumber screenDims.h }
      D.render ctx $ GFX.background screenDims (image $ assetLookup ABackground assets) <>
        renderText (toNumber screenDims.w * 0.45) (toNumber screenDims.h/2.0) white 18 "Waiting for players" <>
        overlay false scrDims gameState assets
     else do
      let debugText = renderText 100.0 100.0 white 12 (show $ {
            mouse: realTouchPos,
            renderOffset: gameState.renderOffset
          })
      -- FIXME draw dragging player first, then in descending order by y coordinate
      let players =
            foldMapWithIndex
              (\pCol pPos ->
                  let mDragPoint = unwrap do
                        ds <- First gameState.dragging
                        guard (ds.playerColor == pCol) (pure ds.dragPoint)
                      p = if maybe false (\ds -> ds.isMouse) gameState.dragging then realMouse else realTouchPos
                  in drawPlayer offscreenDims pPos mDragPoint p (assetLookup (APlayer pCol) assets))
              gameState.players
      let r = gameState.renderOffset
      let bg = image (assetLookup ABackground assets)
      let scrDims = { w: toNumber screenDims.w, h: toNumber screenDims.h }
      D.render ctx
        (D.scale zoom zoom (GFX.background screenDims bg <>
          (translate (-r.x) (-r.y) (
            image (canvasElementToImageSource offscreenCanvas) <>
            players)) <>
          overlay true scrDims gameState assets <> debugText)))
    mAssets

resize :: CanvasElement -> DimensionPair -> Effect Unit
resize canvas dims = do
  setCanvasWidth canvas (toNumber dims.w)
  setCanvasHeight canvas (toNumber dims.h)

loadAsset :: String -> Effect (Signal (Maybe CanvasImageSource))
loadAsset url = do
  c <- channel Nothing
  tryLoadImage url (send c)
  pure (subscribe c)

loadAssets :: GAssets String -> Effect (Signal (Maybe Assets))
loadAssets assetUrls = map (map sequenceRecord) $ map sequenceRecord $ sequenceRecord $ mapRecord loadAsset assetUrls

main :: Effect Unit
main = onDOMContentLoaded do
    frames <- animationFrame
    upKey <- keyPressed $ keycodes^.up
    rightKey <- keyPressed $ keycodes^.right
    downKey <- keyPressed $ keycodes^.down
    leftKey <- keyPressed $ keycodes^.left
    mcanvas <- getCanvasElementById "canvas"
    doc <- toDocument <$> (document =<< window)
    offscreenCanvas <- unsafeCoerce <$> createElement "canvas" doc
    screenDims <- windowDimensions
    let scrDims = map (\{ w, h } -> { w: toNumber w, h: toNumber h }) screenDims
    let offscreenDims = constant $ { w: floor tileHalfWidth * 2 * 4 * 9, h: floor tileHalfHeight * 2 * 4 * 9 }
    mPos_ <- mousePos
    let mPos = (\{x, y} -> {x: toNumber x/zoom, y: toNumber y/zoom}) <$> mPos_
    mPressed <- mouseButtonPressed MouseLeftButton
    log "creating websocket"
    path <- pathname =<< location =<< window
    host <- host =<< location =<< window
    protocol <- protocol =<< location =<< window
    { ws, serverMsg } <- WS.create $ (if protocol == "https:" then "wss://" else "ws://") <> host <> path
    mouseWheel <- Wheel.create
    maybe
        (log "error no canvas")
        (\canvas -> do
            touch <- Touch.create (unsafeCoerce canvas)
            let mouseMove = { offscreenDims: _, mousePos: _, mousePressed: _, ws: _ } <$>
                            offscreenDims <*> mPos <*> mPressed <*> ws
            let arrowKeys = { screenDims: _, offscreenDims: _, up: _, right: _, down: _, left: _ } <$>
                            scrDims <*> offscreenDims <*> upKey <*> rightKey <*> downKey <*> leftKey
            let mouseWheelInputs = { screenDims: _, offscreenDims: _, mWheelEvent: _ } <$>
                            scrDims <*> offscreenDims <*> mouseWheel
            let touchInputs = { screenDims: _, offscreenDims: _, mTouchEvent: _, ws: _ } <$>
                            scrDims <*> offscreenDims <*> touch <*> ws
            let inputs =
                  foldl merge
                    (Keyboard <$> arrowKeys) [
                      MouseWheel <$> mouseWheelInputs,
                      Touch <$> touchInputs,
                      Mouse <$> sampleOn mPressed mouseMove,
                      ServerMsg <$> serverMsg,
                      const Tick <$> every 1000.0
                    ]
            rerenderChan <- channel initialState.maze
            game <- foldEffect (gameLogic rerenderChan) initialState inputs
            let realMousePos = (\g m -> g.renderOffset + m) <$> game <*> mPos
            let realTouchPos = (\g m -> case m of
                                          (Just (TouchMove e)) -> do
                                              let touchToPoint t = { x: toNumber (pageX t), y: toNumber (pageY t) }
                                              let mP = touchToPoint <$> (L.head $ Map.values $ changedTouches e)
                                              g.renderOffset + (fromMaybe {x:0.0, y:0.0} mP)
                                          _ -> g.renderOffset) <$> game <*> touch
            ctx <- getContext2D canvas
            offscreenCtx <- getContext2D offscreenCanvas
            runSignal (resize canvas <$> screenDims)
            runSignal (resize offscreenCanvas <$> offscreenDims)
            renderedMaze <- mapEffect (renderMaze offscreenCtx)
            mAssets <- loadAssets {
                        playerRed: "/svg/player-red.svg",
                        playerYellow: "/svg/player-yellow.svg",
                        playerGreen: "/svg/player-green.svg",
                        playerPurple: "/svg/player-purple.svg",
                        exploreRedN: "/svg/explore-red-n.svg",
                        exploreRedE: "/svg/explore-red-e.svg",
                        exploreRedS: "/svg/explore-red-s.svg",
                        exploreRedW: "/svg/explore-red-w.svg",
                        exploreYellowN: "/svg/explore-yellow-n.svg",
                        exploreYellowE: "/svg/explore-yellow-e.svg",
                        exploreYellowS: "/svg/explore-yellow-s.svg",
                        exploreYellowW: "/svg/explore-yellow-w.svg",
                        exploreGreenN: "/svg/explore-green-n.svg",
                        exploreGreenE: "/svg/explore-green-e.svg",
                        exploreGreenS: "/svg/explore-green-s.svg",
                        exploreGreenW: "/svg/explore-green-w.svg",
                        explorePurpleN: "/svg/explore-purple-n.svg",
                        explorePurpleE: "/svg/explore-purple-e.svg",
                        explorePurpleS: "/svg/explore-purple-s.svg",
                        explorePurpleW: "/svg/explore-purple-w.svg",
                        wallRight: "/svg/wall-right.svg",
                        wallDown: "/svg/wall-down.svg",
                        wallRightDown: "/svg/wall-right-down.svg",
                        wallNWCorner: "/svg/wall-nw-corner.svg",
                        warpRed: "/svg/warp-red.svg",
                        warpYellow: "/svg/warp-yellow.svg",
                        warpGreen: "/svg/warp-green.svg",
                        warpPurple: "/svg/warp-purple.svg",
                        weaponRed: "/svg/weapon-red.svg",
                        weaponYellow: "/svg/weapon-yellow.svg",
                        weaponGreen: "/svg/weapon-green.svg",
                        weaponPurple: "/svg/weapon-purple.svg",
                        exitPurpleN: "/svg/exit-purple-n.svg",
                        exitPurpleE: "/svg/exit-purple-e.svg",
                        exitPurpleS: "/svg/exit-purple-s.svg",
                        exitPurpleW: "/svg/exit-purple-w.svg",
                        cardN: "/svg/card-north.svg",
                        cardE: "/svg/card-east.svg",
                        cardS: "/svg/card-south.svg",
                        cardW: "/svg/card-west.svg",
                        cellTop: "/svg/cell-top.svg",
                        cellBottomRight: "/svg/cell-bottom-right.svg",
                        cellBottomLeft: "/svg/cell-bottom-left.svg",
                        hourglassRed: "/svg/hourglass-red.svg",
                        hourglassBlack: "/svg/hourglass-black.svg",
                        background: "/svg/background.svg",
                        overlay: "/svg/overlay.svg",
                        nametag: "/svg/nametag.svg"
                      }
            -- TODO(codequality) rerenderChan should only change when the maze changes, right now it changes on every server command received
            let renderedMazeSignal = renderedMaze $ { maze: _, offscreenDims: _, mAssets: _ } <$> subscribe rerenderChan <*> offscreenDims <*> mAssets
            -- TODO(minor) sampleOn animationFrame ?
            runSignal (render path ctx offscreenCanvas <$> offscreenDims <*> screenDims <*> mAssets <*> realMousePos <*> realTouchPos <*> game))
        mcanvas
