module Main (main) where

import Algebra.Heyting       (Heyting (..))
import Algebra.Lattice       (bottom, top, (/\))
import Data.Word8Set         (Word8Set)
import Test.QuickCheck       (Property, (===))
import Test.Tasty            (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = defaultMain $ testGroup "word8set"
    [ testProperty "heyting1" heyting1
    , testProperty "heyting2" heyting2
    , testProperty "heyting3" heyting3
    , testProperty "heyting4" heyting4
    , testProperty "heytingNeg" heytingNeg
    , testProperty "heytingEquiv" heytingEquiv
    ]

-------------------------------------------------------------------------------
-- Heyting
--
-- x ==> x        ≡ top
-- x /\ (x ==> y) ≡ x /\ y
-- y /\ (x ==> y) ≡ y
-- x ==> (y /\ z) ≡ (x ==> y) /\ (x ==> z)
--
-------------------------------------------------------------------------------

heyting1 :: Word8Set -> Property
heyting1 x = (x ==> x) === top

heyting2 :: Word8Set -> Word8Set -> Property
heyting2 x y = (x /\ (x ==> y)) === x /\ y

heyting3 :: Word8Set -> Word8Set -> Property
heyting3 x y = (y /\ (x ==> y)) === y

heyting4 :: Word8Set -> Word8Set -> Word8Set -> Property
heyting4 x y z = (x ==> (y /\ z)) === (x ==> y) /\ (x ==> z)

heytingNeg :: Word8Set -> Property
heytingNeg x = neg x === x ==> bottom

heytingEquiv :: Word8Set -> Word8Set -> Property
heytingEquiv x y = x <=> y === (x ==> y) /\ (y ==> x)
