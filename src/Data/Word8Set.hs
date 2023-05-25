{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeFamilies          #-}
-- | The 'Word8Set' type represents a set of elements of type 'Word8'.
--
-- The interface of this module mimics the "Data.Set" and/or "Data.IntSet"
-- module interfaces from @containers package.
--
-- These module is intended to be imported qualified, to avoid name clashes with Prelude functions, e.g.
--
-- @
-- import Data.Word8Set (Word8Set)
-- import qualified Data.Word8Set as W8S
-- @
--
-- == Implementation
-- The implementation is based on 'Word256' type. 'Word8Set' is 256 bits.
--
module Data.Word8Set (
    -- * Set type
    Word8Set,
    Key,

    -- * Construction
    empty,
    full,
    singleton,
    range,

    -- * Insertion
    insert,

    -- * Deletion
    delete,

    -- * Generalized insertion/deletion
    alterF,

    -- * Query
    member,
    notMember,
    lookupLT,
    lookupGT,
    lookupLE,
    lookupGE,
    null,
    isFull,
    isSingleton,
    isRange,
    size,
    isSubsetOf,
    isProperSubsetOf,
    disjoint,

    -- * Combine
    union,
    unions,
    difference,
    symmetricDifference,
    (\\),
    intersection,
    complement,

    -- * Filter
    filter,
    partition,

    -- * Map
    map,

    -- * Folds
    foldr,
    foldl,

    -- ** Strict folds
    foldr',
    foldl',

    -- * Min\/Max
    findMin,
    findMax,
    deleteMin,
    deleteMax,
    maxView,
    minView,

    -- * Conversion
    -- ** to/fromList
    elems,
    toList,
    fromList,
    fromFoldable,

    -- ** to/from Word256
    toWord256,
    fromWord256,

    -- ** to/from ASCII Strings
    toASCII,
    fromASCII,
) where

import Prelude
       (Bool (..), Eq ((==)), Functor (fmap), Int, Maybe (..), Ord, Show (showsPrec), String, fromIntegral, fst, maybe, negate, not,
       otherwise, return, showParen, showString, snd, ($), (&&), (+), (-), (.), (/=), (<=), (>))

import Algebra.Lattice       (BoundedJoinSemiLattice (..), BoundedMeetSemiLattice (..), Lattice (..))
import Algebra.Heyting (Heyting (..))
import Algebra.PartialOrd (PartialOrd (..))
import Control.DeepSeq       (NFData (..))
import Data.Bits             ((.&.), (.|.))
import Data.Char             (chr, ord)
import Data.Monoid           (Monoid (..))
import Data.Semigroup        (Semigroup (..))
import Data.String           (IsString (..))
import Data.WideWord.Word256 (Word256 (..))
import Data.Word             (Word8)
import Test.QuickCheck       (Arbitrary (..), CoArbitrary (..), Function (..), functionMap)

import Language.Haskell.TH.Syntax (Lift (..))

import qualified Data.Bits     as Bits
import qualified Data.Foldable as F
import qualified GHC.Exts

-- $setup
-- >>> import Prelude (even, (+), flip)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | A set of 'Word8' numbers.
--
-- Implemented using 'Word256'.
newtype Word8Set = W8S Word256
  deriving (Eq, Ord)

-- | Key of 'Word8Set' is 'Word8'.
type Key = Word8

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Show Word8Set where
    showsPrec d xs = showParen (d > 10) $ showString "fromList " . showsPrec 11 (toList xs)

instance NFData Word8Set where
    rnf (W8S xs) = rnf xs

instance Lift Word8Set where
    lift (W8S (Word256 a b c d)) =
        [| W8S (Word256 a b c d) |]

#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped (W8S (Word256 a b c d)) =
        [|| W8S (Word256 a b c d) ||]
#endif

instance Semigroup Word8Set where
    (<>) = union

instance Monoid Word8Set where
    mempty = empty
    mappend = (<>)

instance Arbitrary Word8Set where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return (W8S (Word256 a b c d))

    shrink xs = fmap fromList (shrink (toList xs)) where

instance CoArbitrary Word8Set where
    coarbitrary (W8S (Word256 a b c d)) = coarbitrary (a,b,c,d)

instance Function Word8Set where
    function = functionMap from to where
        from (W8S (Word256 a b c d)) = (a, b, c, d)
        to (a,b,c,d) = W8S (Word256 a b c d)

instance Lattice Word8Set where
    (\/) = union
    (/\) = intersection

instance BoundedJoinSemiLattice Word8Set where
    bottom = empty

instance BoundedMeetSemiLattice Word8Set where
    top = full

-- |
--
-- @since 0.1.1
instance Heyting Word8Set where
    a ==> b = neg a \/ b 
    neg     = complement
    a <=> b = complement (symmetricDifference a b)

-- | @'leq' = 'isSubsetOf'@
--
-- @since 0.1.1
instance PartialOrd Word8Set where
    leq = isSubsetOf

instance GHC.Exts.IsList Word8Set where
  type Item Word8Set = Key
  fromList = fromList
  toList   = toList

-- | @'fromString' = 'fromASCII'@
--
-- @since 0.1.1
instance IsString Word8Set where
    fromString = fromASCII

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | The empty set.
--
-- >>> empty
-- fromList []
--
empty :: Word8Set
empty = W8S Bits.zeroBits

-- | The full set.
--
-- >>> full
-- fromList [0,1,2,3,4,5,6,7,8,9,10,11,12,...,255]
--
full :: Word8Set
full = W8S ones

ones :: Word256
ones = Bits.complement Bits.zeroBits

-- | A set of one element.
--
-- >>> singleton 127
-- fromList [127]
--
singleton :: Key -> Word8Set
singleton x = W8S (Bits.bit (fromIntegral x))

-- | A set of inclusive range.
--
-- >>> range 10 20
-- fromList [10,11,12,13,14,15,16,17,18,19,20]
--
range :: Key -> Key -> Word8Set
range mi ma
    | mi <= ma  = W8S $ Bits.shiftL (Bits.shiftR ones (fromIntegral (negate (1 + ma - mi)))) (fromIntegral mi)
    | otherwise = empty

-------------------------------------------------------------------------------
-- Insertion
-------------------------------------------------------------------------------

-- | Add a value to the set.
--
-- >>> insert 10 (range 15 20)
-- fromList [10,15,16,17,18,19,20]
--
-- >>> insert 10 (range 10 15)
-- fromList [10,11,12,13,14,15]
--
insert :: Key -> Word8Set -> Word8Set
insert x (W8S xs) = W8S (Bits.setBit xs (fromIntegral x))

-------------------------------------------------------------------------------
-- Deletion
-------------------------------------------------------------------------------

-- | Delete a value in the set. Returns the original set when the value was not present.
--
-- >>> delete 10 (range 5 15)
-- fromList [5,6,7,8,9,11,12,13,14,15]
--
-- >>> delete 10 (range 1 10)
-- fromList [1,2,3,4,5,6,7,8,9]
--
delete :: Key -> Word8Set -> Word8Set
delete x (W8S xs) = W8S (Bits.clearBit xs (fromIntegral x))

-------------------------------------------------------------------------------
-- Generalized insertion/deletion
-------------------------------------------------------------------------------

-- | Generalized insetion deletion.
alterF :: Functor f => (Bool -> f Bool) -> Key -> Word8Set -> f Word8Set
alterF f x xs
    | member x xs = fmap (\b -> if b then xs else delete x xs) (f True)
    | otherwise   = fmap (\b -> if b then insert x xs else xs) (f False)

-------------------------------------------------------------------------------
-- Query
-------------------------------------------------------------------------------

-- | Is the set empty?
--
-- >>> null empty
-- True
--
-- >>> null (range 10 20)
-- False
--
null :: Word8Set -> Bool
null (W8S xs) = xs == Bits.zeroBits

-- | Is the set full?
--
-- >>> isFull full
-- True
--
-- >>> isFull (range 1 255)
-- False
--
isFull :: Word8Set -> Bool
isFull (W8S xs) = xs == ones

-- | Cardinality of the set.
--
-- >>> size empty
-- 0
--
-- >>> size (union (range 10 20) (range 30 40))
-- 22
--
size :: Word8Set -> Int
size (W8S xs) = Bits.popCount xs

-- | Is the value a member of the set?
--
-- >>> member 5 (range 10 20)
-- False
--
-- >>> member 15 (range 10 20)
-- True
--
member :: Key -> Word8Set -> Bool
member x (W8S xs) = Bits.testBit xs (fromIntegral x)

-- | Is the element not in the set?
--
-- >>> notMember 5 (range 10 20)
-- True
--
-- >>> notMember 15 (range 10 20)
-- False
--
notMember :: Key -> Word8Set -> Bool
notMember x xs = not (member x xs)

-- | Is this a subset? @(s1 `isSubsetOf` s2)@ tells whether s1 is a subset of s2.
--
-- >>> isSubsetOf (range 10 20) (range 5 25)
-- True
--
-- >>> isSubsetOf (range 10 20) (range 10 20)
-- True
--
-- >>> isSubsetOf (range 5 25) (range 10 20)
-- False
--
isSubsetOf :: Word8Set -> Word8Set -> Bool
isSubsetOf a b = b == union a b

-- | Is this a proper subset? Is this a proper subset? (ie. a subset but not equal).
--
-- >>> isProperSubsetOf (range 10 20) (range 5 25)
-- True
--
-- >>> isProperSubsetOf (range 10 20) (range 10 20)
-- False
--
-- >>> isProperSubsetOf (range 5 25) (range 10 20)
-- False
--
isProperSubsetOf :: Word8Set -> Word8Set -> Bool
isProperSubsetOf a b = b == union a b && a /= b

-- | Is set singleton?
--
-- >>> isSingleton empty
-- Nothing
--
-- >>> isSingleton full
-- Nothing
--
-- >>> isSingleton (singleton 5)
-- Just 5
--
-- >>> isSingleton (fromList [3, 5])
-- Nothing
--
isSingleton :: Word8Set -> Maybe Key
isSingleton (W8S ws)
    | Bits.popCount ws /= 1 = Nothing
    | otherwise             = Just (fromIntegral (ctz ws))

-- | Is set of the form @'range' l r@?
--
-- >>> isRange empty
-- Nothing
--
-- >>> isRange full
-- Just (0,255)
--
-- >>> isRange (singleton 5)
-- Just (5,5)
--
-- >>> isRange (range 10 20)
-- Just (10,20)
--
-- >>> isRange (fromList [3, 5])
-- Nothing
--
isRange :: Word8Set -> Maybe (Key, Key)
isRange (W8S 0)  = Nothing
isRange (W8S ws) = if Bits.popCount ws + l + r == 256 then Just (fromIntegral l, fromIntegral (255 - r)) else Nothing
  where
    r = clz ws
    l = ctz ws

-- | Check whether two sets are disjoint (i.e. their intersection is empty).
--
-- >>> disjoint (range 11 20) (range 21 30)
-- True
--
disjoint :: Word8Set -> Word8Set -> Bool
disjoint xs ys = intersection xs ys == empty

-- | Find largest element smaller than the given one.
--
-- >>> lookupLT 3 (fromList [3, 5])
-- Nothing
-- >>> lookupLT 5 (fromList [3, 5])
-- Just 3
--
-- >>> lookupLT 0 full
-- Nothing
lookupLT :: Key -> Word8Set -> Maybe Key
lookupLT 0 _  = Nothing
lookupLT x xs = lookupLE (x - 1) xs

-- | Find smallest element greater than the given one.
--
-- >>> lookupGT 4 (fromList [3, 5])
-- Just 5
--
-- >>> lookupGT 5 (fromList [3, 5])
-- Nothing
--
-- >>> lookupGT 255 full
-- Nothing
--
lookupGT :: Key -> Word8Set -> Maybe Key
lookupGT 255 _  = Nothing
lookupGT x   xs = lookupGE (x + 1) xs

-- | Find largest element smaller or equal to the given one.
--
-- >>> lookupLE 2 (fromList [3, 5])
-- Nothing
--
-- >>> lookupLE 4 (fromList [3, 5])
-- Just 3
--
-- >>> lookupLE 5 (fromList [3, 5])
-- Just 5
--
lookupLE :: Key -> Word8Set -> Maybe Key
lookupLE x xs = fmap fst (maxView (intersection xs (range 0 x)))

-- | Find smallest element greater or equal to the given one.
--
-- >>> lookupGE 3 (fromList [3, 5])
-- Just 3
--
-- >>> lookupGE 4 (fromList [3, 5])
-- Just 5
--
-- >>> lookupGE 6 (fromList [3, 5])
-- Nothing
--
lookupGE :: Key -> Word8Set -> Maybe Key
lookupGE x xs = fmap fst (minView (intersection xs (range x 255)))

-------------------------------------------------------------------------------
-- Combine
-------------------------------------------------------------------------------

-- | The complement of the set.
complement :: Word8Set -> Word8Set
complement (W8S xs) = W8S (Bits.complement xs)

-- | The union of two sets.
union :: Word8Set -> Word8Set -> Word8Set
union (W8S xs) (W8S ys) = W8S (xs .|. ys)

-- | The union of a list of sets.
unions :: F.Foldable f => f Word8Set -> Word8Set
unions xs = F.foldl' union empty xs

-- | The intersection between two sets.
intersection :: Word8Set -> Word8Set -> Word8Set
intersection (W8S xs) (W8S ys) = W8S (xs .&. ys)

-- | Difference between two sets.
difference :: Word8Set -> Word8Set -> Word8Set
difference xs ys = intersection xs (complement ys)

-- | Symmetric difference between two sets
--
-- @
-- 'symmetricDifference' xs ys = 'difference' ('union' xs ys) ('intersection' xs ys)
-- @
--
-- >>> symmetricDifference (range 10 20) (range 15 25)
-- fromList [10,11,12,13,14,21,22,23,24,25]
--
-- @since 0.1.1
--
symmetricDifference :: Word8Set -> Word8Set -> Word8Set
symmetricDifference (W8S xs) (W8S ys) = W8S (Bits.xor xs ys)

-- | See 'difference'.
(\\) :: Word8Set -> Word8Set -> Word8Set
m1 \\ m2 = difference m1 m2

-------------------------------------------------------------------------------
-- Filter
-------------------------------------------------------------------------------

-- | Filter all elements that satisfy some predicate.
--
-- >>> filter even (range 10 20)
-- fromList [10,12,14,16,18,20]
--
filter :: (Key -> Bool) -> Word8Set -> Word8Set
filter p = foldl' (\acc x -> if p x then insert x acc else acc) empty

-- | Partition the set according to some predicate.
--
-- >>> partition even (range 10 20)
-- (fromList [10,12,14,16,18,20],fromList [11,13,15,17,19])
--
partition :: (Key -> Bool) -> Word8Set -> (Word8Set,Word8Set)
partition p = foldl' (\(l, r) x -> if p x then (insert x l, r) else (l, insert x r)) (empty, empty)

-------------------------------------------------------------------------------
-- Map
-------------------------------------------------------------------------------

-- |  @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- >>> map (+ 1) (fromList [0, 10, 250, 255])
-- fromList [0,1,11,251]
--
map :: (Key -> Key) -> Word8Set -> Word8Set
map f = foldl' (\acc k -> insert (f k) acc) empty

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- | Lazy right fold.
--
-- >>> foldr (:) [] (unions [range 10 20, range 70 73, range 130 132, range 200 202, singleton 255])
-- [10,11,12,13,14,15,16,17,18,19,20,70,71,72,73,130,131,132,200,201,202,255]
--
foldr :: (Key -> b -> b) -> b -> Word8Set -> b
foldr f z0 (W8S (Word256 a0 b0 c0 d0)) = go0 a0 b0 c0 d0 z0 where
    go0 0 !b !c !d acc = go1 b c d acc
    go0 a  b  c  d acc = let !x = clz a in go0 (Bits.clearBit a (63 - x)) b c d (f (255 - fromIntegral x) acc)

    go1 0 !c d acc = go2 c d acc
    go1 b  c d acc = let !x = clz b in go1 (Bits.clearBit b (63 - x)) c d (f (191 - fromIntegral x) acc)

    go2 0 !d acc = go3 d acc
    go2 c  d acc = let !x = clz c in go2 (Bits.clearBit c (63 - x)) d (f (127 - fromIntegral x) acc)

    go3 0 acc = acc
    go3 a acc = let !x = clz a in go3 (Bits.clearBit a (63 - x)) (f (63 - fromIntegral x) acc)

-- | Strict right fold.
--
foldr' :: (Key -> b -> b) -> b -> Word8Set -> b
foldr' f z0 (W8S (Word256 a0 b0 c0 d0)) = go0 a0 b0 c0 d0 z0 where
    go0 0 !b !c !d !acc = go1 b c d acc
    go0 a  b  c  d  acc = let !x = clz a in go0 (Bits.clearBit a (63 - x)) b c d (f (255 - fromIntegral x) acc)

    go1 0 !c d !acc = go2 c d acc
    go1 b  c d  acc = let !x = clz b in go1 (Bits.clearBit b (63 - x)) c d (f (191 - fromIntegral x) acc)

    go2 0 !d !acc = go3 d acc
    go2 c  d  acc = let !x = clz c in go2 (Bits.clearBit c (63 - x)) d (f (127 - fromIntegral x) acc)

    go3 0 !acc = acc
    go3 a  acc = let !x = clz a in go3 (Bits.clearBit a (63 - x)) (f (63 - fromIntegral x) acc)

-- | Lazy left fold.
--
-- >>> foldl (flip (:)) [] (unions [range 10 20, range 70 73, range 130 132, range 200 202, singleton 255])
-- [255,202,201,200,132,131,130,73,72,71,70,20,19,18,17,16,15,14,13,12,11,10]
--
foldl :: (a -> Key -> a) -> a -> Word8Set -> a
foldl f z0 (W8S (Word256 a0 b0 c0 d0)) = go0 a0 b0 c0 d0 z0 where
    go0 !a !b !c 0 acc = go1 a b c acc
    go0  a  b  c d acc = let !x = ctz d in go0 a b c (Bits.clearBit d x) (f acc (fromIntegral x))

    go1 !a !b 0 acc = go2 a b acc
    go1  a  b c acc = let !x = ctz c in go1 a b (Bits.clearBit c x) (f acc (fromIntegral x + 64))

    go2 !a 0 acc = go3 a acc
    go2  a b acc = let !x = ctz b in go2 a (Bits.clearBit b x) (f acc (fromIntegral x + 128))

    go3 0 acc = acc
    go3 a acc = let !x = ctz a in go3 (Bits.clearBit a x) (f acc (fromIntegral x + 192))

-- | Strict left fold.
--
foldl' :: (a -> Key -> a) -> a -> Word8Set -> a
foldl' f z0 (W8S (Word256 a0 b0 c0 d0)) = go0 a0 b0 c0 d0 z0 where
    go0 !a !b !c 0 !acc = go1 a b c acc
    go0  a  b  c d  acc = let !x = ctz d in go0 a b c (Bits.clearBit d x) (f acc (fromIntegral x))

    go1 !a !b 0 !acc = go2 a b acc
    go1  a  b c  acc = let !x = ctz c in go1 a b (Bits.clearBit c x) (f acc (fromIntegral x + 64))

    go2 !a 0 !acc = go3 a acc
    go2  a b  acc = let !x = ctz b in go2 a (Bits.clearBit b x) (f acc (fromIntegral x + 128))

    go3 0 !acc = acc
    go3 a  acc = let !x = ctz a in go3 (Bits.clearBit a x) (f acc (fromIntegral x + 192))

-------------------------------------------------------------------------------
-- Min/Max
-------------------------------------------------------------------------------

-- | The minimal element of the set.
--
-- >>> findMin (fromList [3, 5])
-- 3
--
-- Returns 0 for empty set.
--
-- >>> findMin empty
-- 0
--
findMin :: Word8Set -> Word8
findMin (W8S xs) = fromIntegral (ctz xs)

-- | The maximal element of the set.
--
-- >>> findMax (fromList [3, 5])
-- 5
--
-- Returns 255 for empty set.
--
-- >>> findMax empty
-- 255
--
findMax :: Word8Set -> Word8
findMax (W8S xs) = fromIntegral (255 - clz xs)

-- | Delete the minimal element. Returns an empty set if the set is empty.
deleteMin :: Word8Set -> Word8Set
deleteMin = maybe empty snd . minView

-- | Delete the maximal element. Returns an empty set if the set is empty.
deleteMax :: Word8Set -> Word8Set
deleteMax = maybe empty snd . minView

-- | Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
--
-- >>> maxView (fromList [3, 5])
-- Just (5,fromList [3])
--
-- >>> maxView empty
-- Nothing
--
maxView :: Word8Set -> Maybe (Key, Word8Set)
maxView xs
    | null xs   = Nothing
    | otherwise = let !x = findMax xs in Just (x, delete x xs)

-- | Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
--
-- >>> minView (fromList [3, 5])
-- Just (3,fromList [5])
--
-- >>> minView empty
-- Nothing
--
minView :: Word8Set -> Maybe (Key, Word8Set)
minView xs
    | null xs   = Nothing
    | otherwise = let !x = findMin xs in Just (x, delete x xs)

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

-- | The elements of a set in ascending order.
elems :: Word8Set -> [Key]
elems = toList

-- | The elements of a set in ascending order.
toList :: Word8Set -> [Key]
toList xs = GHC.Exts.build (\c n -> foldr c n xs)

-- | Create a set from a list of 'Word8's.
fromList :: [Key] -> Word8Set
fromList w8s = F.foldl' (\acc x -> insert x acc) empty w8s

-- | Create a set from a foldable of 'Word8's.
fromFoldable :: F.Foldable f => f Key -> Word8Set
#if MIN_VERSION_base(4,13,0)
fromFoldable = F.foldMap' singleton
#else
fromFoldable = F.foldl' (\acc x -> insert x acc) empty
#endif

-------------------------------------------------------------------------------
-- Word256
-------------------------------------------------------------------------------

-- | Convert set to 'Word256'.
toWord256 :: Word8Set -> Word256
toWord256 (W8S xs) = xs

-- | Create set from 'Word256'.
fromWord256 :: Word256 -> Word8Set
fromWord256 = W8S

-------------------------------------------------------------------------------
-- ASCII
-------------------------------------------------------------------------------

-- | Create ASCII string from a set.
--
-- >>> toASCII (range 100 120)
-- "defghijklmnopqrstuvwx"
--
toASCII :: Word8Set -> String
toASCII = fmap (chr . fromIntegral) . toList

-- | Create set from ASCII string.
--
-- >>> fromASCII "foobar"
-- fromList [97,98,102,111,114]
--
-- Non-ASCII codepoints are truncated:
--
-- >>> fromASCII "\1000"
-- fromList [232]
--
fromASCII :: String -> Word8Set
fromASCII = fromList .  fmap (fromIntegral . ord)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

clz :: Bits.FiniteBits a => a -> Int
clz = Bits.countLeadingZeros
{-# INLINE clz #-}

ctz :: Bits.FiniteBits a => a -> Int
ctz = Bits.countTrailingZeros
{-# INLINE ctz #-}
