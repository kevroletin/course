{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams pattern fileName = (fastAnagrams' pattern) <$> lines <$> readFile fileName

fastAnagrams' ::
  Chars
  -> List Chars
  -> List Chars
fastAnagrams' pattern xs = filter (\x -> S.member x perms) xs
  where
    perms = S.fromList $ hlist (permutations pattern)

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
