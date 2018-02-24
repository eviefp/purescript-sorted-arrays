-- | `SortedArray` is a newtype wrapper on top of `Array`. You can construct a `SortedArray` by
-- | using the sort function provided in this module. The main benefit is being able to
-- | binary search through the array through `elemIndex`, `elemLastIndex`, `findIndex`,
-- | `findLastIndex` and `filter`.
-- |
-- | A large number of functions from `Data.Array` were ported over since they just work. However,
-- | a few do not make sense (for example, insert at index). A few others take a `SortedArray` but
-- | must return an `Array` and are convenience functions for unwrapping and then applying the
-- | operation on the underlying `Array`.
-- |
-- | `SortedArray` has the following instances: `Eq`, `Foldable`, `Show`, `Semigroup` and `Monoid`.
-- |
-- | Please note that there is no `Functor` instance but there is a `map` function that returns
-- | an `Array b`.
module Data.SortedArray
  ( SortedArray
  , unSortedArray
  , fromFoldable
  , singleton
  , range
  , (..)
  , replicate
  , null
  , length
  , cons
  , (:)
  , snoc
  , insert
  , head
  , last
  , tail
  , init
  , uncons
  , unsnoc
  , index
  , (!!)
  , elemIndex
  , elemLastIndex
  , findIndex
  , findLastIndex
  , delete
  , deleteAt
  , filter
  , partition
  , map
  , mapWithIndex
  , sort
  , slice
  , take
  , takeEnd
  , takeWhile
  , drop
  , dropEnd
  , dropWhile
  , span
  , nub
  , nubBy
  ) where

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Monoid (class Monoid)
import Data.Ord (class Ord1, lessThanOrEq)
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Ord, class Semigroup, class Show, Ordering(EQ, GT, LT), compare, eq, flip, id, max, min, otherwise, show, ($), (+), (-), (/), (<$>), (<<<), (==), (>>=))
import Prelude as P


-- | You can create `SortedArray`s by using the `sort` functions. You can get the underlying
-- | `Array` using `unSortedArray`.
newtype SortedArray a = SortedArray (Array a)

-- | Comparison is done using `<=`, so the operation is left-biased with regards to EQ.
-- | This also means that the operation is commutative up to EQ.
instance sortedArraySemigroup ∷ Ord a ⇒ Semigroup (SortedArray a) where
  append = sortedArrayConcatImpl lessThanOrEq

instance sortedArrayMonoid ∷ Ord a ⇒ Monoid (SortedArray a) where
  mempty = mkSortedArray []


foreign import sortedArrayConcatImpl ∷ ∀ a. (a → a → Boolean) → SortedArray a → SortedArray a → SortedArray a

data Direction = Forward | Backward

-- | Unwraps to the inner `Array`.
unSortedArray ∷ ∀ a. SortedArray a → Array a
unSortedArray (SortedArray a) = a

mkSortedArray ∷ ∀ a. Array a → SortedArray a
mkSortedArray = SortedArray

derive newtype instance eqSortedArray ∷ Eq a ⇒ Eq (SortedArray a)

instance sortedArrayEq1 ∷ Eq1 SortedArray where
  eq1 = eq

derive instance sortedArrayOrd ∷ Ord a ⇒ Ord (SortedArray a)

instance sortedArrayOrd1 ∷ Ord1 SortedArray where
  compare1 = compare

derive newtype instance foldableSortedArray ∷ Foldable SortedArray

instance showSortedArray ∷ Show a ⇒ Show (SortedArray a) where
  show = show <<< unSortedArray

-- | Unfold and sort a `Foldable` structure to a `SortedArray`.
fromFoldable ∷ ∀ f a. Foldable f ⇒ Ord a ⇒ f a → SortedArray a
fromFoldable = sort <<< Array.fromFoldable

-- | Creates a singleton array which is by definition sorted.
singleton ∷ ∀ a. a → SortedArray a
singleton = mkSortedArray <<< Array.singleton

-- | Creates an array containing a range of integers, including the bounds.
range ∷ Int → Int → SortedArray Int
range from to = mkSortedArray <<< Array.range (min from to) $ max from to

-- | Infix synonym for `range`.
infix 8 range as ..

-- | Create a `SortedArray` with `n` copies of a value.
replicate ∷ ∀ a. Int → a → SortedArray a
replicate n = mkSortedArray <<< Array.replicate n

-- | Tests whether the array is empty.
null ∷ ∀ a. SortedArray a → Boolean
null = Array.null <<< unSortedArray

-- | Gets the length of the array.
length ∷ ∀ a. SortedArray a → Int
length = Array.length <<< unSortedArray

-- | Convenience function for adding an item at the beginning of the sorted array. The result is
-- | a plain `Array`. Use `insert` if you need the result to also be a `SortedArray`.
cons ∷ ∀ a. a → SortedArray a → Array a
cons a = Array.cons a <<< unSortedArray

-- | Infix synonym for `cons`.
infix 8 cons as :

-- | Flipped version of `cons`.
snoc ∷ ∀ a. SortedArray a → a → Array a
snoc = flip cons

-- | Insert an item in the sorted array. The array remains sorted. The item goes in the first
-- | position it can (so if they are duplicates, it will be the first item in that particular
-- | EQ _group_).
insert ∷ ∀ a. Ord a ⇒ a → SortedArray a → SortedArray a
insert a sa = mkSortedArray <<< unsafePartial $ fromJust <<< Array.insertAt (maybe 0 id <<< go 0 $ length sa) a <<< unSortedArray $ sa
  where
  f ∷ a → Ordering
  f = compare a

  go ∷ Int → Int → Maybe Int
  go low high
    | low == high = Just low
    | otherwise =
        let mid = ((high + low) / 2) in
        case f <$> index sa mid of
          Nothing → Nothing
          Just eq → case eq of
            EQ → goDir (_ - 1) mid
            LT → go low mid
            GT → go (mid + 1) high

  goDir ∷ (Int → Int) → Int → Maybe Int
  goDir dir idx =
    case f <$> index sa (dir idx) of
      Nothing → Just idx
      Just eq → case eq of
        EQ → goDir dir (dir idx)
        _  → Just idx

-- | Gets the first item of the array, or `Nothing` if the array is empty.
head ∷ ∀ a. SortedArray a → Maybe a
head = Array.head <<< unSortedArray

-- | Gets the last item of the array, or `Nothing` if the array is empty.
last ∷ ∀ a. SortedArray a → Maybe a
last = Array.last <<< unSortedArray

-- | Gets the rest of the array (all except the first item), or `Nothing` if the array is empty.
tail ∷ ∀ a. SortedArray a → Maybe (SortedArray a)
tail = P.map mkSortedArray <<< Array.tail <<< unSortedArray

-- | Gets all the items in the array except the last item, or `Nothing` if the array is empty.
init ∷ ∀ a. SortedArray a → Maybe (SortedArray a)
init = P.map mkSortedArray <<< Array.init <<< unSortedArray

-- | Deconstructs the array to a `head` and `tail`, or returns `Nothing` if the array is empty.
uncons ∷ ∀ a. SortedArray a → Maybe { head ∷ a, tail ∷ SortedArray a }
uncons = P.map (\m -> { head: m.head, tail: mkSortedArray m.tail }) <<< Array.uncons <<< unSortedArray

-- | Flipped version of `uncons`.
unsnoc ∷ ∀ a. SortedArray a → Maybe { init ∷ SortedArray a, last ∷ a }
unsnoc = P.map (\m -> { init: mkSortedArray m.init, last: m.last }) <<< Array.unsnoc <<< unSortedArray

-- | Gets the item at the specified index, or `Nothing` if it is out of bounds.
index ∷ ∀ a. SortedArray a → Int → Maybe a
index = Array.index <<< unSortedArray

-- | Infix synonym for `index`.
infix 8 index as !!

-- | Finds the first index of the first occurrence of the provided item. Uses binary search.
elemIndex ∷ ∀ a. Ord a ⇒ a → SortedArray a → Maybe Int
elemIndex a = findIndex' Forward a

-- | Finds the last index of the first occurrence of the provided item. Uses binary search.
elemLastIndex ∷ ∀ a. Ord a ⇒ a → SortedArray a → Maybe Int
elemLastIndex a = findIndex' Backward a

-- | Finds the first index for which the provided compare function tests equal (`EQ`).
-- | Uses binary search.
findIndex ∷ ∀ a. Ord a ⇒ a → SortedArray a → Maybe Int
findIndex a = findIndex' Forward a

-- | Finds the last index for which the provided compare function tests equal (`EQ`).
-- | Uses binary search.
findLastIndex ∷ ∀ a. Ord a ⇒ a → SortedArray a → Maybe Int
findLastIndex a = findIndex' Backward a

findIndex' ∷ ∀ a. Ord a ⇒ Direction → a → SortedArray a → Maybe Int
findIndex' dir a sa = go 0 <<< length $ sa
  where

  f ∷ a → Ordering
  f = compare a

  go ∷ Int → Int → Maybe Int
  go low high
    | low == high = Nothing
    | otherwise =
        let mid = ((high + low) / 2) in
        case f <$> index sa mid of
          Nothing → Nothing
          Just eq → case eq of
            EQ → case dir of
              Forward  → goDir (_ - 1) mid
              Backward → goDir (_ + 1) mid
            LT → go low mid
            GT → go (mid + 1) high

  goDir ∷ (Int → Int) → Int → Maybe Int
  goDir dir idx =
    case f <$> index sa (dir idx) of
      Nothing → Just idx
      Just eq → case eq of
        EQ → goDir dir (dir idx)
        _  → Just idx

delete ∷ ∀ a. Ord a ⇒ a → SortedArray a → SortedArray a
delete a xs = unsafePartial $ fromJust $ (findIndex a xs >>= (flip deleteAt) xs) <|> Just xs

-- | Deletes item at index.
deleteAt ∷ ∀ a. Int → SortedArray a → Maybe (SortedArray a)
deleteAt idx = P.map mkSortedArray <<< Array.deleteAt idx <<< unSortedArray

-- | Returns all items for which the provided compare function tests equal ('EQ').
-- | Uses binary search.
filter ∷ ∀ a. Ord a ⇒ a → SortedArray a → SortedArray a
filter a sa = mkSortedArray <<< go (_ == a) <<< findIndex a $ sa
    where
    go f' = case _ of
      Nothing → []
      Just i →
        case index sa i of
          Nothing → []
          Just val → case f' val of
            true  → Array.cons val (go f' <<< Just $ i + 1)
            false → []

-- | Splits the array in two arrays depending on whether they test true or false for the provided
-- | predicate. Ordering is retained, so both arrays are still sorted.
partition ∷ ∀ a. (a → Boolean) → SortedArray a → { yes ∷ SortedArray a, no ∷ SortedArray a }
partition f = (\res → { yes: mkSortedArray res.yes, no: mkSortedArray res.no }) <<< Array.partition f <<< unSortedArray

-- | Functor-like convenience function, equivalent to unwrapping and applying the Array map.
map ∷ ∀ a b. (a → b) → SortedArray a → Array b
map f = P.map f <<< unSortedArray

-- | Apply function to each element, supplying a zero-based index. Result is a regular `Array`.
mapWithIndex ∷ ∀ a b. (Int → a → b) → SortedArray a → Array b
mapWithIndex f = Array.mapWithIndex f <<< unSortedArray

-- | Sort an array and wrap it as a `SortedArray`.
sort ∷ ∀ a. Ord a ⇒ Array a → SortedArray a
sort = mkSortedArray <<< Array.sort

-- | Extracts a subarray by a start and end index.
slice ∷ ∀ a. Int → Int → SortedArray a → SortedArray a
slice start end = mkSortedArray <<< Array.slice start end <<< unSortedArray

-- | Creates a new array by taking the first `n` elements of the array.
take ∷ ∀ a. Int → SortedArray a → SortedArray a
take n = mkSortedArray <<< Array.take n <<< unSortedArray

-- | Creates a new array by taking the last `n` elements of the array.
takeEnd ∷ ∀ a. Int → SortedArray a → SortedArray a
takeEnd n = mkSortedArray <<< Array.takeEnd n <<< unSortedArray

-- | Creates a new array by taking the longest subarray of items that satisfy the specified predicate.
takeWhile ∷ ∀ a. (a → Boolean) → SortedArray a → SortedArray a
takeWhile pred = mkSortedArray <<< Array.takeWhile pred <<< unSortedArray

-- | Creates a new array by skipping the first `n` elements of the array.
drop ∷ ∀ a. Int → SortedArray a → SortedArray a
drop n = mkSortedArray <<< Array.drop n <<< unSortedArray

-- | Creates a new array by skipping the last `n` elements of the array.
dropEnd ∷ ∀ a. Int → SortedArray a → SortedArray a
dropEnd n = mkSortedArray <<< Array.dropEnd n <<< unSortedArray

-- | Creates a new array by skipping the longest subarray of items that satisfy the specified predicate.
dropWhile ∷ ∀ a. (a → Boolean) → SortedArray a → SortedArray a
dropWhile pred = mkSortedArray <<< Array.dropWhile pred <<< unSortedArray

-- | Splits an array into the longest initial subarray of elements that satisfy the specified predicate and
-- | the remaining elements.
span ∷ ∀ a. (a → Boolean) → SortedArray a → { init ∷ SortedArray a, rest ∷ SortedArray a }
span pred = (\res → { init: mkSortedArray res.init, rest: mkSortedArray res.rest}) <<< Array.span pred <<< unSortedArray

-- | Creates a new array that contains no duplicates.
nub ∷ ∀ a. Eq a ⇒ SortedArray a → SortedArray a
nub = mkSortedArray <<< Array.nub <<< unSortedArray

-- | Creates a new array that contains no duplicates by using the specified equivalence relation.
nubBy ∷ ∀ a. (a → a → Boolean) → SortedArray a → SortedArray a
nubBy pred = mkSortedArray <<< Array.nubBy pred <<< unSortedArray
