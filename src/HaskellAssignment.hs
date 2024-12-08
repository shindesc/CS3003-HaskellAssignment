module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq

instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"

findFirst :: (a -> Bool) -> [a] -> Found
findFirst needle haystack = go 0 haystack
  where
    go _ [] = NoMatch
    go index (x:xs)
      | needle x  = Match index
      | otherwise = go (index + 1) xs

------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome candidate = candidate == reverse candidate

