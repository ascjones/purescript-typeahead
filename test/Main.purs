module Test.Main (main) where

import Prelude

import Typeahead

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Array
import Data.Function
import Data.String.Regex

import qualified Control.Monad.Eff.JQuery as J

newtype USState = USState String

instance showUSState :: Show USState where
  show (USState name) = name

states :: Array USState
states = USState <$>
  [ "Alabama", "Alaska", "Arizona", "Arkansas", "California"
  , "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii"
  , "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana"
  , "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota"
  , "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire"
  , "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota"
  , "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island"
  , "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont"
  , "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
  ]

main = do
  statesInput <- J.select "#main .typeahead"
  let statesData = dataset "states" $ substringMatcher states
  ta <- typeahead statesInput defaultOptions [statesData]

  onSelect (\_ sugg -> log $ "Suggestion selected: " ++ sugg) ta
  onActive  (\_ -> log "Active triggered") ta
  onOpen    (\_ -> log "Open triggered") ta
  onClose   (\_ -> log "Close triggered") ta
  onIdle    (\_ -> log "Idle triggered") ta

  where
  substringMatcher :: forall a. (Show a) => Array a -> Source a
  substringMatcher arr = \q cb _ -> do
    let substrRegex = regex q (parseFlags "i")
    cb $ filter (test substrRegex <<< show) arr
