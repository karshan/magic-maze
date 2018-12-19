module PSBridge where

import Data.Aeson.TH

purescriptOptions = defaultOptions {
    sumEncoding = ObjectWithSingleField
  , allNullaryToStringTag = False
  , tagSingleConstructors = True
  }
