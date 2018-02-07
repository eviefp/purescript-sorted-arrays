-- | `SortedArray` is a newtype wrapper on top of `Array`. You can construct a `SortedArray` by
-- | using the sort functions provided in this module. The main benefit is being able to
-- | binary search through the array through `elemIndex`, `elemLastIndex`, `findIndex`,
-- | `findLastIndex` and `filter`.
-- |
-- | A large number of functions from `Data.Array` were ported over since they just work. However,
-- | a few do not make sense (for example, insert at index). A few others take a `SortedArray` but
-- | must return an `Array` and are convenience functions for unwrapping and then applying the
-- | operation on the underlying `Array`.
-- |
-- | Please note that there is no `Functor` instance but there is a `map'` function that returns
-- | an `Array b`.
module Data.SortedArray
  ( SortedArray
  , unSortedArray
  , singleton
  , range
  , (..)
  , replicate
  , null
  , length
  , cons
  , snoc
  , insert
  , insertBy
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
  , deleteAt
  , filter
  , partition
  , map'
  , mapWithIndex'
  , sort
  , sortBy
  , sortWith
  , slice
  , take
  , takeWhile
  , drop
  , dropWhile
  , span
  , nub
  , nubBy
  ) where

import Data.Array as Array
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (class Foldable)
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Ord, Ordering(EQ, GT, LT), compare, flip, id, map, negate, otherwise, ($), (+), (-), (/), (<$>), (<<<), (==), (>=))

-- | You can create `SortedArray`s by using the `sort` functions. You can get the underlying
-- | `Array` using `unSortedArray`.
-- |
-- | TODO: Add info about foldable. Can we derive any other useful instance?
newtype SortedArray a = SortedArray (Array a)

-- | Unwraps the `Array`.
unSortedArray ∷ ∀ a. SortedArray a → Array a
unSortedArray (SortedArray xs) = xs

derive newtype instance foldableSortedArray ∷ Foldable SortedArray

-- | Creates a singleton array which is by definition sorted.
singleton ∷ ∀ a. a → SortedArray a
singleton = SortedArray <<< Array.singleton

-- | Creates an array containing a range of integers, including the bounds.
range ∷ Int → Int → SortedArray Int
range from = SortedArray <<< Array.range from

-- | Infix synonym for `range`.
infix 8 range as ..

-- | Repeats the value for the specified amount of times.
replicate ∷ ∀ a. Int → a → SortedArray a
replicate n = SortedArray <<< Array.replicate n

-- | Tests whether the array is empty.
null ∷ ∀ a. SortedArray a → Boolean
null = Array.null <<< unSortedArray

-- | Gets the length of the array.
length ∷ ∀ a. SortedArray a → Int
length = Array.length <<< unSortedArray

-- | Convenience function for adding an item at the beginning of the sorted array. The result is
-- | a plain `Array`.
cons ∷ ∀ a. a → SortedArray a → Array a
cons a = Array.cons a <<< unSortedArray

-- | Infix synonym for `cons`.
infix 8 cons as :

-- | Flipped cons.
snoc ∷ ∀ a. SortedArray a → a → Array a
snoc = flip cons

-- | Insert an item in the sorted array. The array remains sorted. The item goes in the first
-- | position it can (so if they are duplicates, it will be the first item in that particular
-- | EQ group).
insert ∷ ∀ a. Ord a ⇒ a → SortedArray a → SortedArray a
insert = insertBy compare

-- | Insert an item in the sorted array using the ordering function provided.
insertBy ∷ ∀ a. (a → a → Ordering) → a → SortedArray a → SortedArray a
insertBy cmp x xs =
  let i = maybe 0 (_ + 1) (findLastIndex (cmp x) xs)
  in unsafePartial $ SortedArray <<< fromJust <<< Array.insertAt i x <<< unSortedArray $ xs

-- | Gets the first item of the array, or `Nothing` if the array is empty.
head ∷ ∀ a. SortedArray a → Maybe a
head = Array.head <<< unSortedArray

-- | Gets the last item of the array, or `Nothing` if the array is empty.
last ∷ ∀ a. SortedArray a → Maybe a
last = Array.last <<< unSortedArray

-- | Gets the rest of the array (except the first item), or `Nothing` if the array is empty.
tail ∷ ∀ a. SortedArray a → Maybe (SortedArray a)
tail = map SortedArray <<< Array.tail <<< unSortedArray

-- | Gets all the items in the array except the last item, or `Nothing` if the array is empty.
init ∷ ∀ a. SortedArray a → Maybe (SortedArray a)
init = map SortedArray <<< Array.init <<< unSortedArray

-- | Deconstructs the array in a `head` and `tail`, or returns `Nothing` if the array is empty.
uncons ∷ ∀ a. SortedArray a → Maybe { head ∷ a, tail ∷ SortedArray a }
uncons = map (\m -> { head: m.head, tail: SortedArray m.tail }) <<< Array.uncons <<< unSortedArray

-- | Flipped version of `uncons`.
unsnoc ∷ ∀ a. SortedArray a → Maybe { init ∷ SortedArray a, last ∷ a }
unsnoc = map (\m -> { init: SortedArray m.init, last: m.last }) <<< Array.unsnoc <<< unSortedArray

-- | Gets the item at the specified index, or `Nothing` if it is out of bounds.
index ∷ ∀ a. SortedArray a → Int → Maybe a
index = Array.index <<< unSortedArray

-- | Infix synonym for `index`.
infix 8 index as !!

-- | Finds the first index of the first occurrence of the provided item. Uses binary search. 
elemIndex ∷ ∀ a. Ord a ⇒ a → SortedArray a → Maybe Int
elemIndex a = findIndex' false (compare a)

-- | Finds the last index of the first occurrence of the provided item. Uses binary search. 
elemLastIndex ∷ ∀ a. Ord a ⇒ a → SortedArray a → Maybe Int
elemLastIndex a = findIndex' true (compare a)

-- | Finds the first index for which the provided compare function tests equal (`EQ`).
-- | Uses binary search.
findIndex ∷ ∀ a. (a → Ordering) → SortedArray a → Maybe Int
findIndex f = findIndex' false f

-- | Finds the last index for which the provided compare function tests equal (`EQ`).
-- | Uses binary search.
findLastIndex ∷ ∀ a. (a → Ordering) → SortedArray a → Maybe Int 
findLastIndex f = findIndex' true f

findIndex' ∷ ∀ a. Boolean → (a → Ordering) → SortedArray a → Maybe Int
findIndex' findLast f arr = go 0 <<< length $ arr
  where
  go low high
    | low == high = Nothing
    | otherwise =
        let mid = ((high + low) / 2) in
        case f <$> index arr mid of
          Nothing → Nothing
          Just eq → case eq of
            EQ → if findLast then goDir (_ + 1) mid else goDir (_ - 1) mid
            LT → go low mid
            GT → go (mid + 1) high
  goDir dir idx =
    case f <$> index arr (dir idx) of
      Nothing → Just idx
      Just eq → case eq of
        EQ → goDir dir (dir idx)
        _  → Just idx

-- | Deletes item at index.
deleteAt ∷ ∀ a. Int → SortedArray a → Maybe (SortedArray a)
deleteAt idx = map SortedArray <<< Array.deleteAt idx <<< unSortedArray

-- | Returns all items for which the provided compare function tests equal ('EQ').
-- | Uses binary search.
filter ∷ ∀ a. (a → Ordering) → SortedArray a → Array a
filter f arr = go ((_ == EQ) <<< f) (maybe (-1) id <<< findIndex f $ arr)
    where
    go f' idx
      | idx == (-1) = []
      | idx >= length arr = []
      | otherwise =
          case index arr idx of
            Nothing → []
            Just val → case f' val of
              true  → Array.cons val (go f' $ idx + 1)
              false → []

-- | Splits the array in two arrays depending on whether they test true or false for the provided
-- | predicate. Ordering is retained, so both arrays are still sorted.
partition ∷ ∀ a. (a → Boolean) → SortedArray a → { yes ∷ SortedArray a, no ∷ SortedArray a }
partition f = (\res → { yes: SortedArray res.yes, no: SortedArray res.no }) <<< Array.partition f <<< unSortedArray

-- | Functor-like convenience function, equivalent to unwrapping and applying the Array map.
map' ∷ ∀ a b. (a → b) → SortedArray a → Array b
map' f = map f <<< unSortedArray

-- | Apply function to each element, supplying a zero-based index. Result is a regular `Array`.
mapWithIndex' ∷ ∀ a b. (Int → a → b) → SortedArray a → Array b
mapWithIndex' f = Array.mapWithIndex f <<< unSortedArray

-- | Sort an array and wrap it as a `SortedArray`.
sort ∷ ∀ a. Ord a ⇒ Array a → SortedArray a
sort = SortedArray <<< Array.sort

-- | Sort an array using the provided compare function and wrap it as a `SortedArray`.
sortBy ∷ ∀ a. (a → a → Ordering) → Array a → SortedArray a
sortBy f = SortedArray <<< Array.sortBy f

sortWith ∷ ∀ a b. Ord b ⇒ (a → b) → Array a → SortedArray a
sortWith f = SortedArray <<< Array.sortWith f

slice ∷ ∀ a. Int → Int → SortedArray a → SortedArray a
slice start end = SortedArray <<< Array.slice start end <<< unSortedArray

take ∷ ∀ a. Int → SortedArray a → SortedArray a
take n = SortedArray <<< Array.take n <<< unSortedArray

takeWhile ∷ ∀ a. (a → Boolean) → SortedArray a → SortedArray a
takeWhile pred = SortedArray <<< Array.takeWhile pred <<< unSortedArray

drop ∷ ∀ a. Int → SortedArray a → SortedArray a
drop n = SortedArray <<< Array.drop n <<< unSortedArray

dropWhile ∷ ∀ a. (a → Boolean) → SortedArray a → SortedArray a
dropWhile pred = SortedArray <<< Array.dropWhile pred <<< unSortedArray

span ∷ ∀ a. (a → Boolean) → SortedArray a → { init ∷ SortedArray a, rest ∷ SortedArray a }
span pred = (\res → { init: SortedArray res.init, rest: SortedArray res.rest}) <<< Array.span pred <<< unSortedArray

nub ∷ ∀ a. Eq a ⇒ SortedArray a → SortedArray a
nub = SortedArray <<< Array.nub <<< unSortedArray

nubBy ∷ ∀ a. (a → a → Boolean) → SortedArray a → SortedArray a
nubBy pred = SortedArray <<< Array.nubBy pred <<< unSortedArray
