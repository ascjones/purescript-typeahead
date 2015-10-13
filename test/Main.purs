module Test.Main (main) where

import Prelude

import Typeahead

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Array
import Data.Function
import Data.String.Regex

import qualified Control.Monad.Eff.JQuery as J

states :: Array String
states =
  [ "Alabama", "Alaska", "Arizona", "Arkansas", "California"
  , "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii"
  , "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana"
  , "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota"
  , "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire"
  , "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota"
  , "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island"
  , "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont"
  , "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming" ]

main = do
  statesInput <- J.select "#main .typeahead"
  let source = substringMatcher states
  typeahead statesInput defaultOptions [{ name : "states", source : source }]

  where
  substringMatcher :: Array String -> Source
  substringMatcher strs = mkFn3 $ \q cb _ -> do
    let substrRegex = regex q (parseFlags "i")
    cb $ filter (test substrRegex) strs
