module StringRewriting.Utils where

import Prelude
import Data.Array (uncons, drop, length, snoc)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | Returns true if the second list starts with the first
isPrefixOf :: forall a. Eq a => Array a -> Array a -> Boolean
isPrefixOf as bs = case uncons as, uncons bs of
    Just {head: a, tail: as'}, Just {head: b, tail: bs'} ->
        if a == b
        then isPrefixOf as' bs'
        else false
    Nothing, _ -> true
    _, _ -> false

-- | Returns true if the second list contains the first list unbroken
isInfixOf :: forall a. Eq a => Array a -> Array a -> Boolean
isInfixOf as bs = case uncons as, uncons bs of
    Just {head: a, tail: as'}, Just {head: b, tail: bs'} ->
        if a == b && isPrefixOf as' bs'
        then true
        else isInfixOf as bs'
    _, _ -> false

splitSubArray :: forall a. Eq a => Array a -> Array a -> Maybe (Tuple (Array a) (Array a))
splitSubArray pattern' word' = splitSubArray' [] pattern' word' where
  splitSubArray' accumulator pattern word =
    if isPrefixOf pattern word
    then Just $ Tuple accumulator (drop (length pattern) word)
    else
      case uncons word of
        Just {head: w, tail: ws'} ->
          splitSubArray' (snoc accumulator w) pattern ws'
        _ -> Nothing
