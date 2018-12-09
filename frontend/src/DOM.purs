module DOM where

import Effect (Effect)
import Data.Unit (Unit)

foreign import onDOMContentLoaded :: forall a. Effect a -> Effect Unit
