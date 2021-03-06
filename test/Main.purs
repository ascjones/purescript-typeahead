module Test.Main (main) where

import Prelude

import DOM (DOM())
import Typeahead

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (EXCEPTION())

import Data.Array
import Data.Function
import Data.String.Regex

import qualified Control.Monad.Eff.JQuery as J

newtype USState = USState String

instance showUSState :: Show USState where
  show (USState name) = name

-- the states that will be supplied as results to the callback for synchrounous results
statesSync :: Array USState
statesSync = USState <$>
  [ "Alabama", "Alaska", "Arizona", "Arkansas", "California"
  , "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii"
  , "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana"
  , "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota"
  , "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire"
  ]

statesAsync :: Array USState
statesAsync = USState <$>
  [ "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota"
  , "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island"
  , "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont"
  , "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
  ]

substringMatcher :: forall a. (Show a) => Array a -> String -> Array a
substringMatcher arr q =
  let substrRegex = regex q (parseFlags "i") in
  filter (test substrRegex <<< show) arr

statesSource :: Source USState (console :: CONSOLE)
statesSource q updateSync updateAsync = do
  updateSync $ syncResults q
  runAff (\err -> log $ show err) updateAsync (asyncResults q)

  where
  syncResults :: String -> Array USState
  syncResults = substringMatcher statesSync

  -- simulate async call, would most likely be an AJAX call in practice
  asyncResults :: forall eff. String -> Aff eff (Array USState)
  asyncResults q = do
    return $ substringMatcher statesAsync q

main :: Eff (dom :: DOM, console :: CONSOLE) Unit
main = do
  statesInput <- J.select "#main .typeahead"
  let statesData = mkDataset "states" statesSource
  ta <- typeahead statesInput defaultOptions [statesData]

  onSelect       ta (\_ sugg -> log $ "Suggestion selected: " ++ sugg)
  -- onAutocomplete ta (\_ sugg -> log $ "Autocomplete triggered: " ++ sugg)
  -- onCursorChange ta (\_ sugg -> log $ "CursorChange triggered: " ++ sugg)
  --
  -- onActive ta (\_ -> log "Active triggered")
  -- onOpen   ta (\_ -> log "Open triggered")
  -- onClose  ta (\_ -> log "Close triggered")
  -- onIdle   ta (\_ -> log "Idle triggered")
