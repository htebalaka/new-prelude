
{-# LANGUAGE NoImplicitPrelude #-}

module NewPrelude.Utils
  ( partitionM
  , splitOn
  , whenM
  , unlessM
  , putStr'
  , hPutStr'
  , elems
  , (>>$), (.:), (..:), (...:)
  ) where

import NewPrelude
import System.IO (hFlush, hPutStr, stdout)

putStr' :: String -> IO ()
putStr' msg = putStr msg >> hFlush stdout

hPutStr' :: Handle -> String -> IO ()
hPutStr' handle msg = hPutStr handle msg >> hFlush handle

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p xs = foldlM lam ([], []) xs
  where 
    lam (a, b) x = bool (a, x:b) (x:a, b) `liftM` p x

elems :: (Eq a) => [a] -> [a] -> Bool
elems subset set = any (isPrefixOf subset) (tails set)

-- taken from i don't remember
splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn e l =
    f : splitOn e (rest ls)
        where
          (f,ls) = span (/=e) l
          rest [] = []
          rest s = tail s

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM cond action = cond >>= (`when` action)

unlessM :: (Functor m, Monad m) => m Bool -> m () -> m ()
unlessM cond action = whenM (not <$> cond) action

-- post-composition with a pure function, useful when used with `>=>`
(>>$) :: (Functor f) => (a -> f b) -> (b -> c) -> (a -> f c)
f >>$ g = \x -> g <$> f x

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .: g = (fmap.fmap) f g

(..:) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
f ..: g = (fmap.fmap.fmap) f g

(...:) :: (e -> f) -> (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> f)
f ...: g = (fmap.fmap.fmap.fmap) f g
