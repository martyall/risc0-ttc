module Cycle
  ( Cycle
  , mkCycle
  , toUnfoldable
  , applyCycle
  , applyCycles
  ) where

import Prelude

import Data.Array (any, fold, length, null, (..))
import Data.List.Lazy as L
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Newtype (un)
import Data.Unfoldable as U

newtype Cycle a = Cycle
  { length :: Int
  , list :: L.List a
  }

mkCycle :: forall a. Array a -> Maybe (Cycle a)
mkCycle as
  | null as = Nothing
  | otherwise = pure $
      Cycle
        { length: length as
        , list: L.cycle $ L.fromFoldable as
        }

instance Show a => Show (Cycle a) where
  show (Cycle c) = show $ L.take c.length c.list

instance Eq a => Eq (Cycle a) where
  eq (Cycle c1) (Cycle c2)
    | c1.length /= c2.length = false
    | otherwise =
        flip any (0 .. (c1.length - 1)) $ \n ->
          L.take c1.length c1.list == L.take c1.length (L.drop n c2.list)

toUnfoldable :: forall f. U.Unfoldable f => Cycle ~> f
toUnfoldable (Cycle c) =
  L.toUnfoldable $ L.take c.length c.list

applyCycle :: forall a. Eq a => Cycle a -> a -> Maybe a
applyCycle (Cycle c) a = go c.length c.list
  where
  go n xs
    | n == 0 = Nothing
    | otherwise = L.uncons xs >>= \{ head, tail } ->
        if head == a then L.head tail else go (n - 1) tail

applyCycles :: forall a. Eq a => Array (Cycle a) -> a -> Maybe a
applyCycles cs a = un First $ fold $ map (\c -> First $ applyCycle c a) cs
