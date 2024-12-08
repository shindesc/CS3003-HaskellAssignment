module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"

findFirst :: Eq a => (a -> Bool) -> [a] -> Found
findFirst predicate xs = go 0 xs
  where
    go _ [] = NoMatch
    go idx (y:ys)
      | predicate y = Match idx
      | otherwise = go (idx + 1) ys

------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome xs = xs == reverse xs

