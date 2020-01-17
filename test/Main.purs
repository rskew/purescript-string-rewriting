module Test.Main where

import Prelude

import Data.Array (all, cons, elem)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console as Console
import KnuthBendix (Equation(..), Rule(..), applyRule, contains, criticalPairs, eqModuloTheory, knuthBendix, normalise, overlaps, rulesFromPairs, shortlex)
import Test.Assert (assert, assertFalse)
import Utils (isInfixOf, isPrefixOf, splitSubArray)

arr :: String -> Array Char
arr = toCharArray

eqUpToReOrdering :: forall a. Eq a => Ord a => Array a -> Array a -> Boolean
eqUpToReOrdering as bs =
  all (\someA -> elem someA bs) as
  &&
  all (\someB -> elem someB as) bs

-- Symmetry group of the square.
-- "a" is rotation by 90 degrees
-- "b" is flippy
c4Equations :: Array Equation
c4Equations =
  [ Equation (arr "aaaa") (arr "")
  , Equation (arr "bb") (arr "")
  , Equation (arr "aaab") (arr "ba")
  ]

c4Rules :: Array Rule
c4Rules =
  [ Rule (arr "baa") (arr "aab")
  , Rule (arr "bab") (arr "aaa")
  , Rule (arr "aba") (arr "b")
  , Rule (arr "aaaa") (arr "")
  , Rule (arr "bb") (arr "")
  , Rule (arr "aaab") (arr "ba")
  , Rule (arr "aaba") (arr "ab")
  ]

main :: Effect Unit
main = do
  assert $ isPrefixOf (arr "asdf") (arr "asdfMisc")
  assertFalse $ isPrefixOf (arr "asdf") (arr "asdzMisc")

  assert $ isInfixOf (arr "asdf") (arr "otherasdfmisc")
  assertFalse $ isInfixOf (arr "asdf") (arr "otherasdzmisc")

  assert $ splitSubArray (arr "Asdf") (arr "otherAsdfMisc")
    == Just (Tuple (arr "other") (arr "Misc"))

  assert $ splitSubArray (arr "Asdf") (arr "otherAsdzMisc")
    == Nothing

  assert $ applyRule (arr "otherAsdfMisc") (Rule (arr "Asdf") (arr "Fdsa"))
    == Just (arr "otherFdsaMisc")

  assert $ normalise c4Rules (arr "baaab")
    == arr "a"

  assert $ eqUpToReOrdering
    (overlaps (arr "abcbc") (arr "bcbcd"))
    [ {a: arr "abc", b: arr "bc", c: arr "bcd"}
    , {a: arr "a", b: arr "bcbc", c: arr "d"}
    ]

  assert $ eqUpToReOrdering
    (contains (arr "ababa") (arr "aba"))
    [ {a: arr "", b: arr "aba", c: arr "ba"}
    , {a: arr "ab", b: arr "aba", c: arr ""}
    ]

  let
    rule1 = Rule (arr "abca") (arr "adf")
    rule2 = Rule (arr "cad") (arr "")
    pairs11 = [ Equation (arr "abcadf") (arr "adfbca")
              , Equation (arr "adf") (arr "adf")
              ]
    pair12 = Equation (arr "adfd") (arr "ab")
  assert $ eqUpToReOrdering
    (criticalPairs rule1 rule1)
    pairs11
  assert $ eqUpToReOrdering
    (criticalPairs rule1 rule2)
    [ pair12 ]

  assert $ shortlex (arr "abcd") (arr "zzz") == GT
  assert $ shortlex (arr "abcd") (arr "zzzz") == LT
  assert $ shortlex (arr "abcd") (arr "abcd") == EQ

  let rules = [ Rule (arr "adfbca") (arr "adfdf")
              , Rule (arr "adfd") (arr "ab")
              ]
  assert $ eqUpToReOrdering
    (rulesFromPairs [rule1, rule2] (cons pair12 pairs11))
    rules

  assert $ eqUpToReOrdering
    (knuthBendix c4Equations)
    c4Rules

  assert $ eqModuloTheory c4Rules (arr "baaaab") (arr "")

  Console.log "Tests passed :D"
