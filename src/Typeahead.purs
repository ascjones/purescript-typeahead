module Typeahead where

import Prelude
import Control.Monad.Eff.JQuery

type Options =
  { highlight   :: Boolean
  , hint        :: Boolean
  , minLength   :: Int
  , classNames  :: ClassNames
  }

type ClassNames =
  { input       :: String
  , hint        :: String
  , menu        :: String
  , dataset     :: String
  , suggestion  :: String
  , empty       :: String
  , open        :: String
  , cursor      :: String
  , highlight   :: String
  }

type Dataset =
  { source : Source }

type Source =
  String ->
  (Array String -> Eff (dom :: DOM | eff)) -> -- callback with sync results
  (Array String -> Eff (dom :: DOM | eff))    -- callback with async results (use Aff somehow???)

foreign import typeahead :: forall eff. JQuery -> Options -> Array Dataset -> JQuery
