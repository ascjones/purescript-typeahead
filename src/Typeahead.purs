module Typeahead where

import Prelude
import DOM (DOM())
import Control.Monad.Eff
import Control.Monad.Eff.JQuery (JQuery(), JQueryEvent())
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

type UpdateResults = Array String -> Eff (dom :: DOM) Unit

type Dataset =
  { source :: Fn3 String UpdateResults UpdateResults (Eff (dom :: DOM) Unit)
  , name   :: String
  }


type Source =
  String
  -> UpdateResults -- callback with sync results
  -> UpdateResults -- callback with async results
  -> (Eff (dom :: DOM) Unit)

dataset :: String -> Source -> Dataset
dataset name source = { name : name, source : mkFn3 source }

-- | The typeahead instance
foreign import data Typeahead :: *

foreign import typeahead :: forall eff. JQuery -> Options -> Array Dataset -> Eff (ta :: DOM | eff) Typeahead

-- | Returns the current value of the typeahead. The value is the text the user has entered into the input element.
foreign import getVal :: forall eff. Typeahead -> Eff (ta :: DOM | eff) String

-- | Sets the value of the typeahead
foreign import setVal :: forall eff. Typeahead -> String -> Eff (ta :: DOM | eff) Unit

-- | Opens the suggestion menu.
foreign import open :: forall eff. Typeahead -> Eff (ta :: DOM | eff) Unit

-- | Closes the suggestion menu.
foreign import close :: forall eff. Typeahead -> Eff (ta :: DOM | eff) Unit

-- | Removes typeahead functionality and reverts the input element back to its original state.
foreign import destroy :: forall eff. Typeahead -> Eff (ta :: DOM | eff) Unit

type TypeaheadEvent = JQueryEvent

foreign import select :: forall eff. (TypeaheadEvent -> String -> Eff (dom :: DOM | eff) Unit) -> Typeahead -> Eff (dom :: DOM | eff) Unit

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
