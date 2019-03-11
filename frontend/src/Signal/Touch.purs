module Signal.Touch where

import Prelude
import Data.Array ((..))
import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe (..), maybe)
import Effect (Effect)
import Signal (Signal)
import Signal.Channel (channel, send, subscribe)
import Web.DOM.Document as Doc
import Web.TouchEvent.EventTypes (touchstart, touchend, touchmove, touchcancel)
import Web.TouchEvent.TouchEvent (TouchEvent)
import Web.TouchEvent.TouchEvent as Touch
import Web.TouchEvent.Touch (Touch, identifier)
import Web.TouchEvent.TouchList as TL
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

changedTouches :: TouchEvent -> Map Int Touch
changedTouches e =
  let c = Touch.changedTouches e
      l = TL.length c
   in foldMap (\i -> maybe Map.empty (\x -> Map.singleton (identifier x) x) $ TL.item i c) (0..l)

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
