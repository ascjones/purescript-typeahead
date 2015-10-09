module Test.Main (main) where

import Prelude

import Typeahead

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Function

import qualified Control.Monad.Eff.JQuery as J

main = do
  states <- J.select "#main .states"
  typeahead states defaultOptions [{ name : "states", source : substringMatcher }]

  where
  substringMatcher :: Source
  substringMatcher = mkFn2 $ \q cb -> do cb ["Michigan", "California"]
