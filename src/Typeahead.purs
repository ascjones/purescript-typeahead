module Typeahead where

import Prelude
import DOM (DOM())

import Data.Function

import Control.Monad.Eff
import Control.Monad.Eff.JQuery (JQuery(), JQueryEvent())

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

type UpdateResults a = Array a -> Eff (dom :: DOM) Unit

type Source a =
  String
  -> UpdateResults a -- callback with sync results
  -> UpdateResults a -- callback with async results
  -> Eff (dom :: DOM) Unit

type Dataset a =
  { source  :: Fn3 String (UpdateResults a) (UpdateResults a) (Eff (dom :: DOM) Unit)
  , name    :: String
  -- , limit   :: Int
  , display :: a -> String
  }

dataset :: forall a. (Show a) => String -> Source a -> Dataset a
dataset name source =
  { name    : name
  , source  : mkFn3 source
  , display : show }

-- | The typeahead instance
foreign import data Typeahead :: *

foreign import typeahead :: forall a eff. JQuery -> Options -> Array (Dataset a) -> Eff (ta :: DOM | eff) Typeahead

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

type BindEvent1 eff = Typeahead -> (TypeaheadEvent -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

foreign import bindEventImpl1 :: forall eff. String -> BindEvent1 eff

onActive :: forall eff. BindEvent1 eff
onActive = bindEventImpl1 "typeahead:active"

onIdle :: forall eff. BindEvent1 eff
onIdle = bindEventImpl1 "typeahead:idle"

onOpen :: forall eff. BindEvent1 eff
onOpen = bindEventImpl1 "typeahead:open"

onClose :: forall eff. BindEvent1 eff
onClose = bindEventImpl1 "typeahead:close"

type BindEvent2 a eff = Typeahead -> (TypeaheadEvent -> a -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

foreign import bindEventImpl2 :: forall a eff. String -> BindEvent2 a eff

onSelect :: forall a eff. BindEvent2 a eff
onSelect = bindEventImpl2 "typeahead:select"

onAutocomplete :: forall a eff. BindEvent2 a eff
onAutocomplete = bindEventImpl2 "typeahead:autocomplete"

onCursorChange :: forall a eff. BindEvent2 a eff
onCursorChange = bindEventImpl2 "typeahead:cursorchange"

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
