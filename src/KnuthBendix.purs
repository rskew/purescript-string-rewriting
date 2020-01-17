-- | Largely informed by
-- | https://haskellformaths.blogspot.com/2010/05/string-rewriting-and-knuth-bendix.html
module KnuthBendix where

import Prelude

import Data.Array (catMaybes, concatMap, cons, drop, dropEnd, elem, filter, length, nub, take, takeEnd, uncons)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Utils (isPrefixOf, splitSubArray)

type Word = Array Char

data Equation = Equation Word Word
derive instance ordEquation :: Ord Equation
instance eqEquation :: Eq Equation where
  eq (Equation leftA rightA) (Equation leftB rightB) =
    leftA == leftB && rightA == rightB
    ||
    leftA == rightB && rightA == leftB
instance showEquation :: Show Equation where
  show (Equation left right) = "(" <> show left <> " == " <> show right <> ")"

data Rule = Rule Word Word
derive instance ordRule :: Ord Rule
derive instance eqRule :: Eq Rule
instance showRule :: Show Rule where
  show (Rule left right) = "(" <> show left <> " --> " <> show right <> ")"

-- | Use a rule to update a word. Leave it unmodified if the rule doesn't match.
applyRule :: Word -> Rule -> Maybe Word
applyRule word (Rule ruleLeft ruleRight) =
  case splitSubArray ruleLeft word of
    Nothing -> Nothing
    Just (Tuple prefix suffix) ->
      Just $ prefix <> ruleRight <> suffix

-- | Repeatedly apply rules until no more can be applied
normalise :: Array Rule -> Word -> Word
normalise rules word =
  let
    applyRuleIfPossible word' rule' = fromMaybe word' (applyRule word' rule')
    reduced = foldl applyRuleIfPossible word rules
  in
    if reduced == word
    then reduced
    else normalise rules reduced

-- | Finds triples A,B,C where wordA = AB and wordB = BC for nonempty A and C
overlaps :: Word -> Word -> Array {a :: Word, b :: Word, c :: Word}
overlaps wordA' wordB' = overlaps' [] 1 wordA' wordB' where
  -- Keep overlapping the words further over each other, looking for ABC cases.
  overlaps' accumulator overlapAmount wordA wordB =
    if overlapAmount >= min (length wordA) (length wordB)
    then accumulator
    else
      let
        a = dropEnd overlapAmount wordA
        b = takeEnd overlapAmount wordA
      in
        if isPrefixOf b wordB
        then
          let
            c = drop overlapAmount wordB
            updatedAccumulator = cons {a: a, b: b, c: c} accumulator
          in
            overlaps' updatedAccumulator (overlapAmount + 1) wordA wordB
        else
            overlaps' accumulator (overlapAmount + 1) wordA wordB

-- | Finds triples A,B,C where wordA = ABC and wordB = B, where A and C can be empty
contains :: Word -> Word -> Array {a :: Word, b :: Word, c :: Word}
contains wordA' wordB' = contains' [] [] wordA' wordB' where
    contains' accumulator a [] wordB = accumulator
    contains' accumulator a wordA wordB =
      if isPrefixOf wordB wordA
      then
        let
          c = drop (length wordB) wordA
          updatedAccumulator = cons {a: a, b: wordB, c: c} accumulator
        in
          contains' updatedAccumulator (a <> (take 1 wordA)) (drop 1 wordA) wordB
      else
        contains' accumulator (a <> (take 1 wordA)) (drop 1 wordA) wordB

-- | When a rule can be reduced two different exclusive ways by two different rules, the
-- | resulting reductions form a critical pair.
-- | This functions returns all such pairs the could result from the given two rules.
criticalPairs :: Rule -> Rule -> Array Equation
criticalPairs (Rule leftA rightA) (Rule leftB rightB) =
  let
    overlapsABPairs = (\triples -> Equation (rightA <> triples.c) (triples.a <> rightB))
                      <$> overlaps leftA leftB
    overlapsBAPairs = (\triples -> Equation (rightB <> triples.c) (triples.a <> rightA))
                      <$> overlaps leftB leftA
    containsABPairs = (\triples -> Equation rightA (triples.a <> rightB <> triples.c))
                      <$> contains leftA leftB
    containsBAPairs = (\triples -> Equation rightB (triples.a <> rightA <> triples.c))
                      <$> contains leftB leftA
  in
    overlapsABPairs <> overlapsBAPairs <> containsABPairs <> containsBAPairs
    # nub

shortlex :: Word -> Word -> Ordering
shortlex left right = compare (Tuple (length left) left) (Tuple (length right) right)

pairToRule :: Array Rule -> Equation -> Maybe Rule
pairToRule rules (Equation left right) =
  let
    normLeft = normalise rules left
    normRight = normalise rules right
  in
    case shortlex normLeft normRight of
      GT -> Just $ Rule normLeft normRight
      LT -> Just $ Rule normRight normLeft
      EQ -> Nothing

rulesFromPairs :: Array Rule -> Array Equation -> Array Rule
rulesFromPairs rules pairs =
  pairs
  <#> pairToRule rules
  # catMaybes
  # nub
  # filter (\rule -> not $ elem rule rules)

-- | Find all the rules that can be found from critical pairs of the
-- | given rule with each of the rules in the given re-write system.
allNewRulesFromRule :: Array Rule -> Rule -> Array Rule
allNewRulesFromRule rules rule =
  rules
  # concatMap (\oldRule ->
      criticalPairs rule oldRule
      # rulesFromPairs rules)
  # nub

-- | Take a set of equations and derive a confluent, terminating re-write system,
-- | solving the word problem for the equational theory i.e. giving a procedure
-- | for judging the equality of two words wrt the theory's exioms.
-- | This routine may not terminate, and solving the word problem is undecidable in
-- | general.
knuthBendix :: Array Equation -> Array Rule
knuthBendix equations =
  let
    rules = equations
            # rulesFromPairs []
            # nub
  in
    knuthBendix' [] rules where
      knuthBendix' :: Array Rule -> Array Rule -> Array Rule
      knuthBendix' markedRules unmarkedRules =
        case uncons unmarkedRules of
          Nothing -> markedRules
          Just {head: nextUnmarkedRule, tail: restUnmarkedRules} ->
            let
              newRules = allNewRulesFromRule (markedRules <> unmarkedRules) nextUnmarkedRule
              updatedMarkedRules = cons nextUnmarkedRule markedRules
              updatedUnmarkedRules = restUnmarkedRules <> newRules
            in
              knuthBendix' updatedMarkedRules updatedUnmarkedRules

eqModuloTheory :: Array Rule -> Word -> Word -> Boolean
eqModuloTheory rules left right =
  normalise rules left == normalise rules right