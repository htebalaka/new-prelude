
module NewPrelude.Unsafe
( module Debug.Trace
, module GHC.Conc
, module GHC.Exts
, module System.IO.Unsafe
, module Unsafe.Coerce
--, traceShowId
--, traceM
--, traceShowM
) where

import Control.Monad.ST ()
import Debug.Trace
import GHC.Conc
import GHC.Exts (lazy, inline)
import System.IO.Unsafe
import Unsafe.Coerce

--traceShowId :: (Show a) => a -> a
--traceShowId a = trace (show a) a

--traceM :: (Monad m) => String -> m ()
--traceM string = trace string $ return ()

--traceShowM :: (Show a, Monad m) => a -> m ()
--traceShowM = traceM . show

