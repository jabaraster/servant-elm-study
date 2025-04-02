module Entity.Helper (
  fieldModifier,
) where

import Data.Char (toLower)

fieldModifier :: Int -> String -> String
fieldModifier n s = case drop n s of
  [] -> []
  (x : xs) -> (toLower x) : xs
