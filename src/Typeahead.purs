module Typeahead where

import Prelude
import DOM (DOM())

import Data.Function
import Data.Foreign.Callback

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

type UpdateResults a eff = Array a -> Eff (dom :: DOM | eff) Unit

-- type Source a eff =
--   String
--   -> UpdateResults a eff -- callback with sync results
--   -> UpdateResults a eff -- callback with async results
--   -> Eff (dom :: DOM | eff) Unit

-- newtype UpdateResults a = UpdateResults (Callback1 (Array a) Unit)

type Dataset a eff =
  { source  :: Callback3 String (UpdateResults a eff) (UpdateResults a eff) Unit
  , name    :: String
  , display :: a -> String
  }

mkDataset
  :: forall a eff
   . (Show a)
  => String
  -> (String -> (UpdateResults a eff) -> (UpdateResults a eff) -> Eff (dom :: DOM | eff) Unit)
  -> Dataset a eff
mkDataset name source =
  { name    : name
  , source  : callback3 source --(\q sync async -> source q (callback1 sync) (callback1 async))
  , display : show
  }

  -- where
  -- mkSource
  --   :: String
  --   -> (Array a -> Eff (dom :: DOM | eff) Unit)
  --   -> (Array a -> Eff (dom :: DOM | eff) Unit)
  --   -> Eff (dom :: DOM | eff) Unit)
  --   -> (Callback3 String (UpdateResults a) (UpdateResults a) Unit)
  -- mkSource q sync async
  --
  -- mkUpdateResults :: (Array a -> Eff (dom :: DOM | eff) Unit) -> UpdateResults a
  -- mkUpdateResults = UpdateResults <<< callback1


  -- where
  -- sourceCallback :: Callback3 String (UpdateResults a eff) (UpdateResults a eff) Unit
  -- sourceCallback = callback3 (\q sync async -> do
    -- source q (runFn1 sync) (runFn1 async))
    -- source q (\res -> do sync res) (\res -> do async res))

-- | The typeahead instance
foreign import data Typeahead :: *

-- foreign import mkSource :: forall a eff. Source a eff -> Fn3 String (UpdateResults a eff) (UpdateResults a eff) (Eff (dom :: DOM | eff) Unit)

foreign import typeahead :: forall a eff. JQuery -> Options -> Array (Dataset a eff) -> Eff (dom :: DOM | eff) Typeahead

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
