module Typeahead where

import Prelude
import DOM (DOM())
import Control.Monad.Eff
import Control.Monad.Eff.JQuery (JQuery())
import Data.Function

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
  { source :: Source
  , name   :: String
  }

type Source =
  Fn3
  String
  (Array String -> Eff (dom :: DOM) Unit) -- callback with sync results
  (Array String -> Eff (dom :: DOM) Unit) -- callback with async results
  (Eff (dom :: DOM) Unit)

foreign import typeahead :: forall eff. JQuery -> Options -> Array Dataset -> Eff (dom :: DOM | eff) JQuery

defaultOptions :: Options
defaultOptions =
  { highlight   : true
  , hint        : false
  , minLength   : 1
  , classNames  : defaultClassNames
  }

defaultClassNames :: ClassNames
defaultClassNames =
  { input       : "tt-input"
  , hint        : "tt-hint"
  , menu        : "tt-menu"
  , dataset     : "tt-dataset"
  , suggestion  : "tt-suggestion"
  , empty       : "tt-empty"
  , open        : "tt-open"
  , cursor      : "tt-cursor"
  , highlight   : "tt-highlight"
  }
