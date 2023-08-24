module Matrix (formatMatrix) where

import Prelude

import Data.Array (cons, fold, foldl, index, intercalate, length, snoc)
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.String as String
import Data.Unfoldable (replicate)
import Partial.Unsafe (unsafePartial)

formatMatrix
  :: forall a
   . Show a
  => { header :: Array a, matrix :: Array (Array a) }
  -> String
formatMatrix { header, matrix } =
  prettyPrintMatrix header
    $ transpose
    $
      map (padArray (length header)) matrix

prettyPrintMatrix
  :: forall a
   . Show a
  => Array a
  -> Array (Array (Maybe a))
  -> String
prettyPrintMatrix header m =
  let
    m' = map (maybe "X" show) <$> m
    maxEntryLength = unsafePartial
      $ fromJust <<< maximum
      $
        fromJust <<< maximum <$>
          (map String.length <$> cons (show <$> header) m')
    wellSpaced = map (leftPad maxEntryLength) <$> m'
    header' = intercalate "| " $ map (leftPad maxEntryLength <<< show) header
    rowOf s = fold (replicate (String.length header') s)
  in
    intercalate "\n"
      [ header'
      , rowOf "*"
      , intercalate ("\n" <> rowOf "-" <> "\n") $
          map (intercalate "| ") wellSpaced
      ]
  where
  leftPad ∷ Int → String → String
  leftPad x s =
    fold (replicate (x - (String.length s)) " ") <> s

-- vendored from https://github.com/purescript/purescript-arrays/blob/v7.2.1/src/Data/Array.purs#L833C1-L844C84
transpose :: forall a. Array (Array a) -> Array (Array a)
transpose xs = go 0 []
  where
  go :: Int -> Array (Array a) -> Array (Array a)
  go idx allArrays = case buildNext idx of
    Nothing -> allArrays
    Just next -> go (idx + 1) (snoc allArrays next)

  buildNext :: Int -> Maybe (Array a)
  buildNext idx = do
    xs # flip foldl Nothing \acc nextArr -> do
      maybe acc (\el -> Just $ maybe [ el ] (flip snoc el) acc) $ index nextArr idx

padArray :: forall a. Int -> Array a -> Array (Maybe a)
padArray n as =
  if length as < n then map Just as <> replicate (n - length as) Nothing
  else map Just as
