module Signal.MouseWheel (create) where

import Prelude
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Signal (Signal)
import Signal.Channel (channel, send, subscribe)
import Web.DOM.Document as Doc
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent as Wheel
import Web.UIEvent.WheelEvent.EventTypes (wheel)
import Web.Event.Event (preventDefault)
import Web.Event.EventTarget (addEventListener, eventListener)
-- import Web.DOM.Document
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toDocument)

create :: Effect (Signal (Maybe WheelEvent))
create = do
  chan <- channel Nothing
  wheelL <- eventListener (\e -> preventDefault e *> send chan (Wheel.fromEvent e))
  doc <- toDocument <$> (document =<< window)
  addEventListener wheel wheelL false (Doc.toEventTarget doc)
  pure $ subscribe chan
