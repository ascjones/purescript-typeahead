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
  -- , classNames  :: ClassNames
  }

defaultOptions :: Options
defaultOptions = { highlight : true, hint : false, minLength : 1 }

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

--foreign import data Source :: forall eff. String -> (Array String -> Eff (dom :: DOM) Unit) -> (Array String -> Eff (dom :: DOM) Unit)
type Source =
  Fn2
  String
  (Array String -> Eff (dom :: DOM) Unit) -- callback with sync results
  -- (Array String -> Eff (dom :: DOM) Unit) -> -- callback with async results
  (Eff (dom :: DOM) Unit)

-- create the source function
-- source :: String -> (Array String -> Eff (dom :: DOM) Unit) -> Eff (dom :: DOM) Unit -> Source
-- source query syncResults asyncResults = mkFn2 query syncResults asyncResults

foreign import typeahead :: forall eff. JQuery -> Options -> Array Dataset -> Eff (dom :: DOM | eff) JQuery
