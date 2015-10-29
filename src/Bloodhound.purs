module Bloodhound where

import Prelude
import DOM (DOM())

foreign import data Bloodhound :: *

type Options a =
  { datumTokenizer  :: a -> Array String
  , queryTokenizer  :: String -> Array String
  , initialize      :: Boolean
  , identify        :: a -> String
  , sufficient      :: Int
  , ordering        :: a -> a -> Int
  }

foreign import bloodhoundImpl :: forall a. Options a -> Bloodhound

class (Ord a) <= Datum a where
  identify :: a -> String
  tokenize :: a -> Array String

type QueryTokenizer = String -> Array String

bloodhound :: forall a. (Datum a) => QueryTokenizer -> Boolean -> Int -> Bloodhound  -- todo add more options
bloodhound queryTokenizer initialize sufficient = bloodhoundImpl options
  where

  options :: Options a
  options = { datumTokenizer  : tokenize
            , queryTokenizer  : queryTokenizer
            , initialize      : initialize
            , identify        : identify
            , sufficient      : sufficient
            , ordering        : ordering
            }

  ordering :: a -> a -> Int
  ordering a b =
    case compare a b of
      LT -> -1
      EQ -> 0
      GT -> 1
