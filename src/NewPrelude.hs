
{-# LANGUAGE Safe, LambdaCase #-}

module NewPrelude
  ( module Control.Applicative
  , module Control.Category
  , module Control.Exception
  , module Control.Monad
  , module Control.Monad.ST.Safe
  , module Data.Bits
  , module Data.Bool
  , module Data.Complex
  , module Data.Data
  , module Data.Either
  , module Data.Fixed
  , module Data.Foldable
  , module Data.Function
  , module Data.Int
  , module Data.IORef
  , module Data.Ix
  , module Data.List
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Ord
  , module Data.Ratio
  , module Data.STRef
  , module Data.String
  , module Data.Traversable
  , module Data.Tuple
  , module Data.Word
  , module GHC.Generics
  , module GHC.IO.Exception
  , module Prelude
  , module System.Exit
  , module System.IO
  , module System.IO.Error
  , module System.Mem
  , module System.Mem.StableName
  , module System.Timeout
  , module Text.Read
--  , bool
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Exception
import Control.Monad hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Monad.ST.Safe
import Data.Bits
import Data.Bool
import Data.Complex
import Data.Data
import Data.Either
import Data.Fixed
import Data.Foldable
import Data.Function hiding ((.), id)
import Data.Int
import Data.IORef
import Data.Ix
import Data.List hiding (concat, foldr, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, find, maximumBy, minimumBy, mapAccumL, mapAccumR, foldl')
import Data.Maybe
import Data.Monoid
import Data.Ord (Down(..))
import Data.Ratio
import Data.STRef
import Data.String
import Data.Traversable
import Data.Tuple
import Data.Word
import GHC.Generics (Generic)
import GHC.IO.Exception
import Prelude hiding (concat, foldr, mapM_, sequence_, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, mapM, sequence, FilePath, id, (.))
import System.Exit
import System.IO (Handle, hClose)
import System.IO.Error
import System.Mem
import System.Mem.StableName
import System.Timeout
import Text.Read (readMaybe, readEither)

--bool :: a -> a -> Bool -> a
--bool f t = \case { False -> f ; True -> t }
