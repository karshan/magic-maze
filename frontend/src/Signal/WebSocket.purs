module Signal.WebSocket (create) where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Foreign as F
import Signal (Signal)
import Signal.Channel (channel, send, subscribe)
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket (create, toEventTarget) as WS
import Web.Socket.Event.EventTypes (onMessage, onOpen) as WS
import Web.Socket.Event.MessageEvent as MessageEvent
import Web.Event.EventTarget (addEventListener, eventListener)

-- TODO onMessage error reporting
create :: String -> Effect ({ ws :: Signal (Maybe WebSocket), serverMsg :: Signal (Maybe String) })
create url = do
  wsChan <- channel Nothing
  msgChan <- channel Nothing
  ws <- WS.create url []
  onOpenL <- eventListener (\e -> send wsChan $ Just ws)
  onMessageL <- eventListener
    (\e -> send msgChan (do
               msgE <- MessageEvent.fromEvent e
               hush $ runExcept $ F.readString $ MessageEvent.data_ msgE))
  addEventListener WS.onOpen onOpenL false (WS.toEventTarget ws)
  addEventListener WS.onMessage onMessageL false (WS.toEventTarget ws)
  pure $ { ws: subscribe wsChan, serverMsg: subscribe msgChan }
