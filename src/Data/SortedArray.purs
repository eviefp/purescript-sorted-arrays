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
  , null
  , length
  , cons
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
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Ord, Ordering(EQ, GT, LT), compare, flip, id, map, negate, otherwise, ($), (+), (-), (/), (<$>), (<<<), (==), (>=))

-- | You can create `SortedArray`s by using the `sort` functions. You can get the underlying
-- | `Array` using `unSortedArray`.
-- |
-- | TODO: Add info about foldable. Can we derive any other useful instance?
newtype SortedArray a = SortedArray 
  { array ∷ (Array a)
  , cmp   ∷ (a → a → Ordering)
  }

-- | Unwraps the `Array`.
unSortedArray ∷ ∀ a. SortedArray a → Array a
unSortedArray (SortedArray a) = a.array

getCmp ∷ ∀ a. SortedArray a → a → a → Ordering
getCmp (SortedArray a) = a.cmp

mkSortedArray ∷ ∀ a. (a → a → Ordering) → Array a → SortedArray a
mkSortedArray c a = SortedArray { array: a, cmp: c }

-- derive newtype instance foldableSortedArray ∷ Foldable SortedArray

-- | Creates a singleton array which is by definition sorted.
singleton ∷ ∀ a. Ord a ⇒ a → SortedArray a
singleton = mkSortedArray compare <<< Array.singleton

singleton' ∷ ∀ a. (a → a → Ordering) → a → SortedArray a
singleton' cmp a = mkSortedArray cmp [a]

-- | Creates an array containing a range of integers, including the bounds.
range ∷ Int → Int → SortedArray Int
range from = mkSortedArray compare <<< Array.range from

-- | Infix synonym for `range`.
infix 8 range as ..

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
insert ∷ ∀ a. a → SortedArray a → SortedArray a
insert a xs@(SortedArray {array, cmp}) =
  let i = maybe 0 (_ + 1) (findLastIndex a xs)
  in unsafePartial $ mkSortedArray cmp <<< fromJust <<< Array.insertAt i a $ array

-- | Gets the first item of the array, or `Nothing` if the array is empty.
head ∷ ∀ a. SortedArray a → Maybe a
head = Array.head <<< unSortedArray

-- | Gets the last item of the array, or `Nothing` if the array is empty.
last ∷ ∀ a. SortedArray a → Maybe a
last = Array.last <<< unSortedArray

-- | Gets the rest of the array (except the first item), or `Nothing` if the array is empty.
tail ∷ ∀ a. SortedArray a → Maybe (SortedArray a)
tail (SortedArray {array, cmp}) = map (mkSortedArray cmp) <<< Array.tail $ array

-- | Gets all the items in the array except the last item, or `Nothing` if the array is empty.
init ∷ ∀ a. SortedArray a → Maybe (SortedArray a)
init (SortedArray {array, cmp}) = map (mkSortedArray cmp) <<< Array.init $ array

-- | Deconstructs the array in a `head` and `tail`, or returns `Nothing` if the array is empty.
uncons ∷ ∀ a. SortedArray a → Maybe { head ∷ a, tail ∷ SortedArray a }
uncons (SortedArray {array, cmp}) = map (\m -> { head: m.head, tail: mkSortedArray cmp m.tail }) <<< Array.uncons $ array

-- | Flipped version of `uncons`.
unsnoc ∷ ∀ a. SortedArray a → Maybe { init ∷ SortedArray a, last ∷ a }
unsnoc (SortedArray {array, cmp}) = map (\m -> { init: mkSortedArray cmp m.init, last: m.last }) <<< Array.unsnoc $ array

-- | Gets the item at the specified index, or `Nothing` if it is out of bounds.
index ∷ ∀ a. SortedArray a → Int → Maybe a
index = Array.index <<< unSortedArray

-- | Infix synonym for `index`.
infix 8 index as !!

-- | Finds the first index of the first occurrence of the provided item. Uses binary search. 
elemIndex ∷ ∀ a. a → SortedArray a → Maybe Int
elemIndex a = findIndex' false a

-- | Finds the last index of the first occurrence of the provided item. Uses binary search. 
elemLastIndex ∷ ∀ a. a → SortedArray a → Maybe Int
elemLastIndex a = findIndex' true a

-- | Finds the first index for which the provided compare function tests equal (`EQ`).
-- | Uses binary search.
findIndex ∷ ∀ a. a → SortedArray a → Maybe Int
findIndex a = findIndex' false a

-- | Finds the last index for which the provided compare function tests equal (`EQ`).
-- | Uses binary search.
findLastIndex ∷ ∀ a. a → SortedArray a → Maybe Int 
findLastIndex a = findIndex' true a

findIndex' ∷ ∀ a. Boolean → a → SortedArray a → Maybe Int
findIndex' findLast a arr@(SortedArray {array, cmp}) = go 0 <<< length $ arr
  where

  f ∷ a → Ordering 
  f = cmp a

  go ∷ Int → Int → Maybe Int
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
  
  goDir ∷ (Int → Int) → Int → Maybe Int
  goDir dir idx =
    case f <$> index arr (dir idx) of
      Nothing → Just idx
      Just eq → case eq of
        EQ → goDir dir (dir idx)
        _  → Just idx

-- | Deletes item at index.
deleteAt ∷ ∀ a. Int → SortedArray a → Maybe (SortedArray a)
deleteAt idx (SortedArray {array, cmp}) = map (mkSortedArray cmp) <<< Array.deleteAt idx $ array

-- | Returns all items for which the provided compare function tests equal ('EQ').
-- | Uses binary search.
filter ∷ ∀ a. a → SortedArray a → Array a
filter a sa@(SortedArray {array, cmp}) = go ((_ == EQ) <<< cmp a) (maybe (-1) id <<< findIndex a $ sa)
    where
    go f' idx
      | idx == (-1) = []
      | idx >= length sa = []
      | otherwise =
          case index sa idx of
            Nothing → []
            Just val → case f' val of
              true  → Array.cons val (go f' $ idx + 1)
              false → []

-- | Splits the array in two arrays depending on whether they test true or false for the provided
-- | predicate. Ordering is retained, so both arrays are still sorted.
partition ∷ ∀ a. (a → Boolean) → SortedArray a → { yes ∷ SortedArray a, no ∷ SortedArray a }
partition f (SortedArray {array, cmp}) = (\res → { yes: mkSortedArray cmp res.yes, no: mkSortedArray cmp res.no }) <<< Array.partition f $ array

-- | Functor-like convenience function, equivalent to unwrapping and applying the Array map.
map' ∷ ∀ a b. (a → b) → SortedArray a → Array b
map' f = map f <<< unSortedArray

-- | Apply function to each element, supplying a zero-based index. Result is a regular `Array`.
mapWithIndex' ∷ ∀ a b. (Int → a → b) → SortedArray a → Array b
mapWithIndex' f = Array.mapWithIndex f <<< unSortedArray

-- | Sort an array and wrap it as a `SortedArray`.
sort ∷ ∀ a. Ord a ⇒ Array a → SortedArray a
sort array = mkSortedArray compare <<< Array.sort $ array

-- | Sort an array using the provided compare function and wrap it as a `SortedArray`.
sortBy ∷ ∀ a. (a → a → Ordering) → Array a → SortedArray a
sortBy cmp array = mkSortedArray cmp <<< Array.sortBy cmp $ array

sortWith ∷ ∀ a b. Ord b ⇒ (a → b) → Array a → SortedArray a
sortWith f array = mkSortedArray (\x y → compare (f x) (f y)) <<< Array.sortWith f $ array

slice ∷ ∀ a. Int → Int → SortedArray a → SortedArray a
slice start end (SortedArray {array, cmp}) = mkSortedArray cmp <<< Array.slice start end $ array

take ∷ ∀ a. Int → SortedArray a → SortedArray a
take n (SortedArray {array, cmp}) = mkSortedArray cmp <<< Array.take n $ array

takeWhile ∷ ∀ a. (a → Boolean) → SortedArray a → SortedArray a
takeWhile pred (SortedArray {array, cmp}) = mkSortedArray cmp <<< Array.takeWhile pred $ array

drop ∷ ∀ a. Int → SortedArray a → SortedArray a
drop n (SortedArray {array, cmp}) = mkSortedArray cmp <<< Array.drop n $ array

dropWhile ∷ ∀ a. (a → Boolean) → SortedArray a → SortedArray a
dropWhile pred (SortedArray {array, cmp}) = mkSortedArray cmp <<< Array.dropWhile pred $ array

span ∷ ∀ a. (a → Boolean) → SortedArray a → { init ∷ SortedArray a, rest ∷ SortedArray a }
span pred (SortedArray {array, cmp}) = (\res → { init: mkSortedArray cmp res.init, rest: mkSortedArray cmp res.rest}) <<< Array.span pred $ array

nub ∷ ∀ a. Eq a ⇒ SortedArray a → SortedArray a
nub (SortedArray {array, cmp}) = mkSortedArray cmp <<< Array.nub $ array

nubBy ∷ ∀ a. (a → a → Boolean) → SortedArray a → SortedArray a
nubBy pred (SortedArray {array, cmp}) = mkSortedArray cmp <<< Array.nubBy pred $ array
