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

data TouchEvent_ =
    TouchStart TouchEvent
  | TouchEnd TouchEvent
  | TouchMove TouchEvent
  | TouchCancel TouchEvent

create :: Effect (Signal (Maybe TouchEvent_))
create = do
  chan <- channel Nothing
  startL <- eventListener (\e -> preventDefault e *> send chan (TouchStart <$> Touch.fromEvent e))
  endL <- eventListener (\e -> preventDefault e *> send chan (TouchEnd <$> Touch.fromEvent e))
  moveL <- eventListener (\e -> preventDefault e *> send chan (TouchMove <$> Touch.fromEvent e))
  cancelL <- eventListener (\e -> preventDefault e *> send chan (TouchCancel <$> Touch.fromEvent e))
  doc <- toDocument <$> (document =<< window)
  addEventListener touchstart startL false (Doc.toEventTarget doc)
  addEventListener touchend endL false (Doc.toEventTarget doc)
  addEventListener touchmove moveL false (Doc.toEventTarget doc)
  addEventListener touchcancel cancelL false (Doc.toEventTarget doc)
  pure $ subscribe chan
