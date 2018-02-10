module Test.Main where

import Data.SortedArray

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, const, discard, negate, ($), (-), (/=), (<), (==))
import Test.Assert (assert, ASSERT)
import Unsafe.Coerce (unsafeCoerce)

mkSortedArray ∷ ∀ a. Array a → SortedArray a
mkSortedArray = unsafeCoerce

main ∷ ∀ e. Eff (console ∷ CONSOLE, assert ∷ ASSERT | e) Unit
main = do
  let emptyArray = mkSortedArray ([] ∷ Array Int)
  
  -- fromFoldable
  -- toUnfoldable
  
  log "singleton should construct an array with a single value"
  assert $ singleton 1 == mkSortedArray [1]
  assert $ singleton "foo" == mkSortedArray ["foo"]

  log "range should create an inclusive array of integers for the specified start and end"
  assert $ (range 0 5) == mkSortedArray [0, 1, 2, 3, 4, 5]
  assert $ (range 2 (-3)) == mkSortedArray [-3, -2, -1, 0, 1, 2]

  log "replicate should produce an array containg an item a specified number of times"
  assert $ replicate 3 true == mkSortedArray [true, true, true]
  assert $ replicate 1 "foo" == mkSortedArray ["foo"]
  assert $ replicate 0 "foo" == (unsafeCoerce [])
  assert $ replicate (-1) "foo" == (unsafeCoerce [])
  -- some
  -- many

  log "null should return false for non-empty arrays"
  assert $ null (mkSortedArray [1]) == false
  assert $ null (mkSortedArray [1, 2, 3]) == false

  log "null should return true for an empty array"
  assert $ null emptyArray == true

  log "length should return the number of items in an array"
  assert $ length emptyArray == 0
  assert $ length (mkSortedArray [1]) == 1
  assert $ length (mkSortedArray [1, 2, 3, 4, 5]) == 5

  log "cons should add an item to the start of an array"
  assert $ 4 : (mkSortedArray [1, 2, 3]) == [4, 1, 2, 3]
  assert $ 1 : emptyArray == [1]

  log "snoc should add an item to the end of an array"
  assert $ (mkSortedArray [1, 2, 3]) `snoc` 4 == [4, 1, 2, 3]
  assert $ emptyArray `snoc` 1 == [1]

  log "insert should add an item at the appropriate place in a sorted array"
  assert $ insert 1.5 (mkSortedArray [1.0, 2.0, 3.0]) == mkSortedArray [1.0, 1.5, 2.0, 3.0]
  assert $ insert 4 (mkSortedArray [1, 2, 3]) == mkSortedArray [1, 2, 3, 4]
  assert $ insert 0 (mkSortedArray [1, 2, 3]) == mkSortedArray [0, 1, 2, 3]

  log "head should return a Just-wrapped first value of a non-empty array"
  assert $ head (mkSortedArray ["bar", "foo"]) == Just "bar"

  log "head should return Nothing for an empty array"
  assert $ head emptyArray == Nothing

  log "last should return a Just-wrapped last value of a non-empty array"
  assert $ last (mkSortedArray ["bar", "foo"]) == Just "foo"

  log "last should return Nothing for an empty array"
  assert $ last emptyArray == Nothing

  log "tail should return a Just-wrapped array containing all the items in an array apart from the first for a non-empty array"
  assert $ tail (mkSortedArray ["bar", "baz", "foo"]) == Just (mkSortedArray ["baz", "foo"])

  log "tail should return Nothing for an empty array"
  assert $ tail emptyArray == Nothing

  log "init should return a Just-wrapped array containing all the items in an array apart from the first for a non-empty array"
  assert $ init (mkSortedArray ["bar", "baz", "foo"]) == Just (mkSortedArray ["bar", "baz"])

  log "init should return Nothing for an empty array"
  assert $ init emptyArray == Nothing

  log "uncons should return nothing when used on an empty array"
  assert $ isNothing (uncons emptyArray)

  log "uncons should split an array into a head and tail record when there is at least one item"
  let u1 = unsafePartial $ fromJust $ uncons $ mkSortedArray [1]
  assert $ u1.head == 1
  assert $ u1.tail == emptyArray
  let u2 = unsafePartial $ fromJust $ uncons $ mkSortedArray [1, 2, 3]
  assert $ u2.head == 1
  assert $ u2.tail == (mkSortedArray [2, 3])

  log "unsnoc should return nothing when used on an empty array"
  assert $ isNothing (unsnoc emptyArray)

  log "unsnoc should split an array into an init and last record when there is at least one item"
  let u3 = unsafePartial $ fromJust $ unsnoc $ mkSortedArray [1]
  assert $ u3.init == emptyArray
  assert $ u3.last == 1
  let u4 = unsafePartial $ fromJust $ unsnoc $ mkSortedArray [1, 2, 3]
  assert $ u4.init == (mkSortedArray [1, 2])
  assert $ u4.last == 3

  log "(!!) should return Just x when the index is within the bounds of the array"
  assert $ (mkSortedArray [1, 2, 3]) !! 0 == (Just 1)
  assert $ (mkSortedArray [1, 2, 3]) !! 1 == (Just 2)
  assert $ (mkSortedArray [1, 2, 3]) !! 2 == (Just 3)

  log "(!!) should return Nothing when the index is outside of the bounds of the array"
  assert $ (mkSortedArray [1, 2, 3]) !! 6 == Nothing
  assert $ (mkSortedArray [1, 2, 3]) !! (-1) == Nothing

  log "elemIndex should return the index of an item that a predicate returns true for in an array"
  assert $ (elemIndex 1 (mkSortedArray [1, 2, 3])) == Just 0
  assert $ (elemIndex 4 (mkSortedArray [1, 2, 3])) == Nothing

  log "elemLastIndex should return the last index of an item in an array"
  assert $ (elemLastIndex 1 (mkSortedArray [1, 2, 3])) == Just 0
  assert $ (elemLastIndex 4 (mkSortedArray [1, 2, 3])) == Nothing

  log "findIndex should return the index of an item that a predicate returns true for in an array"
  assert $ (findIndex 2 (mkSortedArray [1, 2, 3])) == Just 1
  assert $ (findIndex 4 (mkSortedArray [1, 2, 3])) == Nothing

  log "findLastIndex should return the last index of an item in an array"
  assert $ (findLastIndex 1 (mkSortedArray [1, 1, 1, 3])) == Just 2
  assert $ (findLastIndex 2 (mkSortedArray [1, 1, 1, 3])) == Nothing

  log "deleteAt should remove an item at the specified index"
  assert $ (deleteAt 0 (mkSortedArray [1, 2, 3])) == Just (mkSortedArray [2, 3])
  assert $ (deleteAt 1 (mkSortedArray [1, 2, 3])) == Just (mkSortedArray [1, 3])

  log "deleteAt should return Nothing if the index is out of range"
  assert $ (deleteAt 1 emptyArray) == Nothing

  log "filter should remove items that don't match a predicate"
  assert $ filter 5 (range 0 10) == (mkSortedArray [5])

  log "sort should reorder a list into ascending order based on the result of compare"
  assert $ sort [1, 3, 2, 5, 6, 4] == mkSortedArray [1, 2, 3, 4, 5, 6]

  log "takeWhile should keep all values that match a predicate from the front of an array"
  assert $ (takeWhile (_ /= 2) (mkSortedArray [1, 2, 3])) == mkSortedArray [1]
  assert $ (takeWhile (_ /= 3) (mkSortedArray [1, 2, 3])) == mkSortedArray [1, 2]
  assert $ (takeWhile (_ /= 1) emptyArray) == emptyArray

  log "take should keep the specified number of items from the end of an array, discarding the rest"
  assert $ (takeEnd 1 (mkSortedArray [1, 2, 3])) == mkSortedArray [3]
  assert $ (takeEnd 2 (mkSortedArray [1, 2, 3])) == mkSortedArray [2, 3]
  assert $ (takeEnd 1 emptyArray) == emptyArray

  log "drop should remove the specified number of items from the front of an array"
  assert $ (drop 1 (mkSortedArray [1, 2, 3])) == mkSortedArray [2, 3]
  assert $ (drop 2 (mkSortedArray [1, 2, 3])) == mkSortedArray [3]
  assert $ (drop 1 emptyArray) == emptyArray

  log "dropWhile should remove all values that match a predicate from the front of an array"
  assert $ (dropWhile (_ /= 1) (mkSortedArray [1, 2, 3])) == mkSortedArray [1, 2, 3]
  assert $ (dropWhile (_ /= 2) (mkSortedArray [1, 2, 3])) == mkSortedArray [2, 3]
  assert $ (dropWhile (_ /= 1) emptyArray) == emptyArray

  log "drop should remove the specified number of items from the end of an array"
  assert $ (dropEnd 1 (mkSortedArray [1, 2, 3])) == mkSortedArray [1, 2]
  assert $ (dropEnd 2 (mkSortedArray [1, 2, 3])) == mkSortedArray [1]
  assert $ (dropEnd 1 emptyArray) == emptyArray

  log "take and drop should treat negative arguments as zero"
  assert $ (take (-2) (mkSortedArray [1, 2, 3])) == emptyArray
  assert $ (drop (-2) (mkSortedArray [1, 2, 3])) == mkSortedArray [1, 2, 3]

  log "span should split an array in two based on a predicate"
  let testSpan { p, input, init_, rest_ } = do
        let result = span p input
        assert $ result.init == init_
        assert $ result.rest == rest_

  let oneToSeven = mkSortedArray [1, 2, 3, 4, 5, 6, 7]
  testSpan { p: (_ < 4), input: oneToSeven, init_: mkSortedArray [1, 2, 3], rest_: mkSortedArray [4, 5, 6, 7] }

  log "span with all elements satisfying the predicate"
  testSpan { p: const true, input: oneToSeven, init_: oneToSeven, rest_: emptyArray }

  log "span with no elements satisfying the predicate"
  testSpan { p: const false, input: oneToSeven, init_: emptyArray, rest_: oneToSeven }

  log "span with large inputs: 10000"
  let testBigSpan n =
        testSpan { p: (_ < n), input: range 1 n, init_: range 1 (n-1), rest_: mkSortedArray [n] }
  testBigSpan 10000

  log "span with large inputs: 40000"
  testBigSpan 40000

  log "span with large inputs: 100000"
  testBigSpan 100000

  log "nub should remove duplicate elements from the list, keeping the first occurence"
  assert $ nub (mkSortedArray [1, 2, 2, 3, 4]) == mkSortedArray [1, 2, 3, 4]

  log "delete should remove the first matching item from an array"
  assert $ delete 1 (mkSortedArray [1, 2, 3]) == mkSortedArray [2, 3]
  assert $ delete 2 (mkSortedArray [1, 2, 3]) == mkSortedArray [1, 3]
  