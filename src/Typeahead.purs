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

-- | The typeahead instance
foreign import data Typeahead :: *

foreign import data TYPEAHEAD :: !

foreign import typeahead :: forall eff. JQuery -> Options -> Array Dataset -> Eff (ta :: TYPEAHEAD | eff) Typeahead

-- | Returns the current value of the typeahead. The value is the text the user has entered into the input element.
foreign import getVal :: forall eff. Typeahead -> Eff (ta :: TYPEAHEAD | eff) String

-- | Sets the value of the typeahead
foreign import setVal :: forall eff. Typeahead -> String -> Eff (ta :: TYPEAHEAD | eff) Unit

-- | Opens the suggestion menu.
foreign import open :: forall eff. Typeahead -> Eff (ta :: TYPEAHEAD | eff) Unit

-- | Closes the suggestion menu.
foreign import close :: forall eff. Typeahead -> Eff (ta :: TYPEAHEAD | eff) Unit

-- | Removes typeahead functionality and reverts the input element back to its original state.
foreign import destroy :: forall eff. Typeahead -> Eff (ta :: TYPEAHEAD | eff) Unit

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
