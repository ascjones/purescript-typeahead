module Main (main) where

import Prelude

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Console

fakeAff :: forall eff. Aff eff String
fakeAff = pure "Result"

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main =
  runAff (\err -> pure unit) (\res -> log res) fakeAff
