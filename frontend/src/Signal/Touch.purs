module Signal.Touch (create, TouchEvent_(..)) where

import Prelude
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Signal (Signal)
import Signal.Channel (channel, send, subscribe)
import Web.DOM.Document as Doc
import Web.TouchEvent.EventTypes (touchstart, touchend, touchmove, touchcancel)
import Web.TouchEvent.TouchEvent (TouchEvent)
import Web.TouchEvent.TouchEvent as Touch
import Web.Event.Event (preventDefault)
import Web.Event.EventTarget (addEventListener, eventListener)
-- import Web.DOM.Document
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)
import Web.Event.Internal.Types (EventTarget)


data TouchEvent_ =
    TouchStart TouchEvent
  | TouchEnd TouchEvent
  | TouchMove TouchEvent
  | TouchCancel TouchEvent

create :: EventTarget -> Effect (Signal (Maybe TouchEvent_))
create target = do
  chan <- channel Nothing
  startL <- eventListener (\e -> preventDefault e *> send chan (TouchStart <$> Touch.fromEvent e))
  endL <- eventListener (\e -> preventDefault e *> send chan (TouchEnd <$> Touch.fromEvent e))
  moveL <- eventListener (\e -> preventDefault e *> send chan (TouchMove <$> Touch.fromEvent e))
  cancelL <- eventListener (\e -> preventDefault e *> send chan (TouchCancel <$> Touch.fromEvent e))
  addEventListener touchstart startL false target
  addEventListener touchend endL false target
  addEventListener touchmove moveL false target
  addEventListener touchcancel cancelL false target
  pure $ subscribe chan
