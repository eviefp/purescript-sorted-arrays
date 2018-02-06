module Data.SortedArray
  ( SortedArray
  , singleton
  , range
  , (..)
  , replicate
  , null
  , length
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
  , filter
  , sort
  , sortBy
  , sortWith
  ) where

import Data.Array as Array
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Foldable)
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Ord, Ordering(EQ, GT, LT), compare, flip, id, map, negate, otherwise, ($), (+), (-), (/), (<$>), (<<<), (==), (>=))

newtype SortedArray a = SortedArray (Array a)

derive instance newtypeSortedArray ∷ Newtype (SortedArray a) _
derive newtype instance foldableSortedArray ∷ Foldable SortedArray

singleton ∷ ∀ a. a → SortedArray a
singleton = SortedArray <<< Array.singleton

range ∷ Int → Int → SortedArray Int
range from = SortedArray <<< Array.range from

infix 8 range as ..

replicate ∷ ∀ a. Int → a → SortedArray a
replicate n = SortedArray <<< Array.replicate n

null ∷ ∀ a. SortedArray a → Boolean
null = Array.null <<< unwrap

length ∷ ∀ a. SortedArray a → Int
length = Array.length <<< unwrap

cons ∷ ∀ a. a → SortedArray a → Array a
cons a = Array.cons a <<< unwrap

infix 8 cons as :

snoc ∷ ∀ a. SortedArray a → a → Array a
snoc = flip cons

insert ∷ ∀ a. Ord a ⇒ a → SortedArray a → SortedArray a
insert = insertBy compare

insertBy ∷ ∀ a. (a → a → Ordering) → a → SortedArray a → SortedArray a
insertBy cmp x xs =
  let i = maybe 0 (_ + 1) (findLastIndex (cmp x) xs)
  in unsafePartial $ SortedArray <<< fromJust <<< Array.insertAt i x <<< unwrap $ xs

head ∷ ∀ a. SortedArray a → Maybe a
head = Array.head <<< unwrap

last ∷ ∀ a. SortedArray a → Maybe a
last = Array.last <<< unwrap

tail ∷ ∀ a. SortedArray a → Maybe (SortedArray a)
tail = map SortedArray <<< Array.tail <<< unwrap

init ∷ ∀ a. SortedArray a → Maybe (SortedArray a)
init = map SortedArray <<< Array.init <<< unwrap

uncons ∷ ∀ a. SortedArray a → Maybe { head ∷ a, tail ∷ SortedArray a }
uncons = map (\m -> { head: m.head, tail: SortedArray m.tail }) <<< Array.uncons <<< unwrap

unsnoc ∷ ∀ a. SortedArray a → Maybe { init ∷ SortedArray a, last ∷ a }
unsnoc = map (\m -> { init: SortedArray m.init, last: m.last }) <<< Array.unsnoc <<< unwrap

index ∷ ∀ a. SortedArray a → Int → Maybe a
index = Array.index <<< unwrap

infix 8 index as !!

elemIndex ∷ ∀ a. Ord a ⇒ a → SortedArray a → Maybe Int
elemIndex a = findIndex' false (compare a)

elemLastIndex ∷ ∀ a. Ord a ⇒ a → SortedArray a → Maybe Int
elemLastIndex a = findIndex' true (compare a)

findIndex ∷ forall a. (a → Ordering) → SortedArray a → Maybe Int
findIndex f = findIndex' false f

findLastIndex ∷ forall a. (a → Ordering) → SortedArray a → Maybe Int 
findLastIndex f = findIndex' true f

findIndex' ∷ forall a. Boolean → (a → Ordering) → SortedArray a → Maybe Int
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

deleteAt ∷ ∀ a. Int → SortedArray a → Maybe (SortedArray a)
deleteAt idx = map SortedArray <<< Array.deleteAt idx <<< unwrap

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

partition ∷ ∀ a. (a → Boolean) → SortedArray a → { yes ∷ SortedArray a, no ∷ SortedArray a }
partition f = (\res → { yes: SortedArray res.yes, no: SortedArray res.no }) <<< Array.partition f <<< unwrap

map' ∷ ∀ a b. (a → b) → SortedArray a → Array b
map' f = map f <<< unwrap

mapWithIndex' ∷ ∀ a b. (Int → a → b) → SortedArray a → Array b
mapWithIndex' f = Array.mapWithIndex f <<< unwrap

sort ∷ ∀ a. Ord a ⇒ Array a → SortedArray a
sort = SortedArray <<< Array.sort

sortBy ∷ ∀ a. (a → a → Ordering) → Array a → SortedArray a
sortBy f = SortedArray <<< Array.sortBy f

sortWith ∷ ∀ a b. Ord b ⇒ (a → b) → Array a → SortedArray a
sortWith f = SortedArray <<< Array.sortWith f

slice ∷ ∀ a. Int → Int → SortedArray a → SortedArray a
slice start end = SortedArray <<< Array.slice start end <<< unwrap

take ∷ ∀ a. Int → SortedArray a → SortedArray a
take n = SortedArray <<< Array.take n <<< unwrap

takeWhile ∷ ∀ a. (a → Boolean) → SortedArray a → SortedArray a
takeWhile pred = SortedArray <<< Array.takeWhile pred <<< unwrap

drop ∷ ∀ a. Int → SortedArray a → SortedArray a
drop n = SortedArray <<< Array.drop n <<< unwrap

dropWhile ∷ ∀ a. (a → Boolean) → SortedArray a → SortedArray a
dropWhile pred = SortedArray <<< Array.dropWhile pred <<< unwrap

span ∷ ∀ a. (a → Boolean) → SortedArray a → { init ∷ SortedArray a, rest ∷ SortedArray a }
span pred = (\res → { init: SortedArray res.init, rest: SortedArray res.rest}) <<< Array.span pred <<< unwrap

nub ∷ ∀ a. Eq a ⇒ SortedArray a → SortedArray a
nub = SortedArray <<< Array.nub <<< unwrap

nubBy ∷ ∀ a. (a → a → Boolean) → SortedArray a → SortedArray a
nubBy pred = SortedArray <<< Array.nubBy pred <<< unwrap
