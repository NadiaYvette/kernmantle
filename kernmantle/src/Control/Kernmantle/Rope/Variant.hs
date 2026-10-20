{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Control.Kernmantle.Rope.Variant
Description : O(1) jump table routing and multi-branch choice for Kernmantle ropes over Polytropos open variants
Copyright   : (c) 2026 The Kernmantle Authors
License     : BSD-3-Clause
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides constant-time \(O(1)\) jump table dispatch and extensible multi-branch choice
for 'Rope' arrow computations over 'Polytropos.Variant' open sums.

In traditional Kernmantle architectures, branching requires 'Control.Arrow.ArrowChoice',
which operates over binary 'Either' trees (\((|||)\), \((+++)\)). For an \(N\)-way branch,
this induces \(O(N)\) linear branch checking and intermediate constructor boxing.

'Control.Kernmantle.Rope.Variant' solves this by coupling Kernmantle 'Rope' with
'Polytropos' open variants:
* 'VariantRouter': An \(O(1)\) array jump table dispatching open variants directly
  via unboxed constructor tags.
* 'routeVariantWith': Type-safe variant routing using a 'Polytropos.Record' of arrow
  handlers with compile-time exhaustiveness verification.
* 'branchRope': Native multi-way branching for 'Rope' computations.
-}
module Control.Kernmantle.Rope.Variant
  ( -- * O(1) Variant Router Jump Table
    VariantRouter(..)
  , makeRouter
  , routeVariant
  , routeVariantRope
  , routerSize
    -- * Type-Safe Variant Routing & Multi-Way Branching
  , routeVariantWith
  , branchRope
    -- * Payload Extraction Helpers
  , getSlot
  , getSlotValue
    -- * Re-exports from Polytropos
  , Variant
  , Record
  , Field(..)
  , HandlerRecordRow
  , HandlerRecordRowM
  ) where

import Data.Functor.Identity (Identity(..))
import Data.Primitive.SmallArray
  ( SmallArray
  , sizeofSmallArray
  , indexSmallArray
  , smallArrayFromList
  )
import Unsafe.Coerce (unsafeCoerce)

import Control.Kernmantle.Rope.Internal (Rope, mkRope)
import Polytropos
  ( Variant
  , Record
  , Field(..)
  , HandlerRecordRow
  , HandlerRecordRowM
  , eliminateUniform
  )
import Polytropos.Core (Rep, RowRep(..), repTag, repVals)

-- | A constant-time O(1) jump table routing open variant effect requests
-- directly to core arrow handlers indexed by their unboxed constructor tags.
newtype VariantRouter core a b = VariantRouter
  { routerTable :: SmallArray (Rep Identity () -> core a b)
  }

-- | Construct a 'VariantRouter' from an ordered list of branch handlers corresponding
-- to consecutive constructor tags (0, 1, ..., n-1) of the sum row.
makeRouter :: [Rep Identity () -> core a b] -> VariantRouter core a b
makeRouter handlers = VariantRouter (smallArrayFromList handlers)
{-# INLINE makeRouter #-}

-- | Dispatch an open variant effect request through the jump table in O(1) time.
routeVariant :: VariantRouter core a b -> Variant s -> core a b
routeVariant (VariantRouter table) (RowRep rep) =
  let idx = fromIntegral (repTag rep)
  in if idx >= 0 && idx < sizeofSmallArray table
       then indexSmallArray table idx rep
       else error ("routeVariant: unhandled variant tag " ++ show idx)
{-# INLINE routeVariant #-}

-- | Route an open variant effect into a Kernmantle 'Rope' arrow computation.
routeVariantRope :: VariantRouter core a b -> Variant s -> Rope record mantle core a b
routeVariantRope router var = mkRope \_ -> routeVariant router var
{-# INLINE routeVariantRope #-}

-- | Route an open variant effect into a 'Rope' computation using a statically-typed
-- 'Polytropos.Record' of core arrow handlers. The compiler verifies full row exhaustiveness.
routeVariantWith :: Record (HandlerRecordRow s (core a b)) -> Variant s -> Rope record mantle core a b
routeVariantWith handlers var = mkRope \_ -> eliminateUniform handlers var
{-# INLINE routeVariantWith #-}

-- | Multi-way branch combinator for 'Rope' arrows over an open variant in O(1) time.
-- Eliminates the need for binary 'Control.Arrow.ArrowChoice' cascades over nested 'Either's.
branchRope :: Record (HandlerRecordRow s (Rope record mantle core a b)) -> Variant s -> Rope record mantle core a b
branchRope handlers var = eliminateUniform handlers var
{-# INLINE branchRope #-}

-- | Number of registered branches in the jump table.
routerSize :: VariantRouter core a b -> Int
routerSize (VariantRouter table) = sizeofSmallArray table
{-# INLINE routerSize #-}

-- | Safely extract a typed value from the i-th payload slot of a variant's active constructor.
getSlot :: forall x. Int -> Rep Identity () -> x
getSlot idx rep =
  let vals = repVals rep
  in if idx >= 0 && idx < sizeofSmallArray vals
       then unsafeCoerce (runIdentity (indexSmallArray vals idx))
       else error ("getSlot: index out of bounds " ++ show idx)
{-# INLINE getSlot #-}

-- | Convenience helper for extracting a payload slot.
getSlotValue :: forall x. Int -> Rep Identity () -> x
getSlotValue = getSlot
{-# INLINE getSlotValue #-}
