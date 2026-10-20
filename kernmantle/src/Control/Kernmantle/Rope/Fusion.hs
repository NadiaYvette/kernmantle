{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Control.Kernmantle.Rope.Fusion
Description : Coyoneda arrow pipeline fusion for Kernmantle ropes
Copyright   : (c) 2026 The Kernmantle Authors
License     : BSD-3-Clause
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides lazy Coyoneda pipeline fusion for 'Rope' arrow computations.

Sequential pure post-processing transformations attached via 'Control.Arrow.arr'
can lead to extraneous arrow wrapper allocations and composition steps.
'Control.Kernmantle.Rope.Fusion' accumulates successive transformations into standard
Haskell function composition using the Coyoneda embedding, lowering them into a
single arrow composition step in \(O(1)\).
-}
module Control.Kernmantle.Rope.Fusion
  ( -- * Coyoneda Pipeline Fusion
    Coyoneda(..)
  , liftCoyoneda
  , lowerCoyoneda
  , coyoneda
  , fuseMap
  , runFused
    -- * Fused Rope Pipelines
  , FusedRope(..)
  , fuseRope
  , fuseRopeMap
  , fuseRopeWith
  ) where

import Control.Arrow (Arrow, arr, (<<<))
import Data.Functor.Coyoneda (Coyoneda(..), lowerCoyoneda, liftCoyoneda)
import Control.Kernmantle.Rope.Internal (Rope)

-- | Smart constructor for 'Coyoneda' applying a function to a carrier.
coyoneda :: (a -> b) -> f a -> Coyoneda f b
coyoneda = Coyoneda
{-# INLINE coyoneda #-}

-- | Fuse an additional transformation onto a Coyoneda pipeline.
-- All mappings are accumulated in pure function composition without
-- executing intermediate passes or allocating intermediate arrows.
fuseMap :: (b -> c) -> Coyoneda f b -> Coyoneda f c
fuseMap = fmap
{-# INLINE fuseMap #-}

-- | Materialize the accumulated Coyoneda pipeline over an arbitrary functor carrier.
runFused :: Functor f => Coyoneda f a -> f a
runFused = lowerCoyoneda
{-# INLINE runFused #-}

-- | A wrapper for a 'Rope' computation whose post-processing transformations
-- are deferred and fused via 'Coyoneda'.
newtype FusedRope record mantle core a b = FusedRope
  { unFusedRope :: Coyoneda (Rope record mantle core a) b
  }

-- | Fuse and materialize a Coyoneda-wrapped 'Rope' arrow into a standard 'Rope' pipeline
-- in a single composition step.
fuseRope :: Arrow core => Coyoneda (Rope record mantle core a) b -> Rope record mantle core a b
fuseRope (Coyoneda f r) = arr f <<< r
{-# INLINE fuseRope #-}

-- | Chain a pure post-processing function onto a 'Rope' pipeline using Coyoneda fusion.
fuseRopeMap :: (b -> c) -> Coyoneda (Rope record mantle core a) b -> Coyoneda (Rope record mantle core a) c
fuseRopeMap = fmap
{-# INLINE fuseRopeMap #-}

-- | Helper to wrap and immediately fuse a pure post-processing transformation.
fuseRopeWith :: Arrow core => (b -> c) -> Rope record mantle core a b -> Rope record mantle core a c
fuseRopeWith f r = fuseRope (fuseRopeMap f (liftCoyoneda r))
{-# INLINE fuseRopeWith #-}
