module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import DOM (onDOMContentLoaded)
import Signal.DOM (animationFrame, keyPressed)
import Signal (foldp, sampleOn, runSignal)

leftKeyCode :: Int
leftKeyCode = 37

rightKeyCode :: Int
rightKeyCode = 39

upKeyCode :: Int
upKeyCode = 38

downKeyCode :: Int
downKeyCode = 40

type Inputs = { left :: Boolean, right :: Boolean, up :: Boolean, down :: Boolean }
type GameState = Boolean

initialState :: GameState
initialState = false

gameLogic :: Inputs -> GameState -> GameState
gameLogic _ g = g

render :: GameState -> Effect Unit
render _ = log "hello"

main :: Effect Unit
main = onDOMContentLoaded do
    frames <- animationFrame
    leftInputs <- keyPressed leftKeyCode
    rightInputs <- keyPressed rightKeyCode
    upInputs <- keyPressed upKeyCode
    downInputs <- keyPressed downKeyCode
    let inputs = { left: _, right: _, up: _, down: _ } <$> leftInputs <*> rightInputs <*> upInputs <*> downInputs
    let game = foldp gameLogic initialState (sampleOn frames inputs)
    runSignal (render <$> game)
