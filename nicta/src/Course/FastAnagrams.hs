{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S
import qualified Prelude as P(fmap, return, (>>=))


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams word filename = 
  let wordsInFile = P.fmap (map NoCaseString) $ P.fmap lines $ readFile filename in
  P.fmap (\w -> map ncString $ (intersectBy (==) w ana)) wordsInFile
  where ana = map NoCaseString $ permutations word

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
