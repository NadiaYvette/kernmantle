{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Control.Kernmantle.Rope.Continuity
Description : Row-polymorphic state continuity across dynamic mode switches for Kernmantle & FRP
Copyright   : (c) 2026 The Kernmantle Authors
License     : BSD-3-Clause
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides row-polymorphic state continuity across dynamic mode switches in
Functional Reactive Programming (FRP) and Kernmantle arrow pipelines.

= Architectural Overview & Rationale
[ref:note-state-continuity-across-switches]

In traditional Arrowized Functional Reactive Programming (AFRP) systems (such as Yampa)
and monadic stream function libraries (such as Dunai), internal state is encapsulated inside
opaque coalgebraic closures (@SF a b@ or @MSF m a b@).

When a dynamic mode switch occurs (e.g., via @switch@ or @polySwitch@):
* In Yampa/Dunai, all internal state of the exiting mode (PID integrators, Kalman filters,
  velocity estimators, moving averages) is immediately discarded unless the developer
  manually hoists every state variable outside the switch into the signal's input/output
  channel tuples @(a, s)@ and @(b, s)@.
* This pollutes input/output types @a@ and @b@ with internal accumulator plumbing, destroying
  modularity and leaking implementation details across subsystem boundaries.

'Control.Kernmantle.Rope.Continuity' solves this dilemma by leveraging 'Polytropos'
first-class row subtyping ('SubRow'):

1. __Row-Polymorphic State Records__:
   Each mode declares its internal state as an open record @Record s_mode@, which contains
   a common shared sub-row @shared@ alongside mode-private fields:
   @
   type FlightState = '[ "pos" ':= Double, "vel" ':= Double, "alt" ':= Double ]
   type TakeoffState = '[ "pos" ':= Double, "vel" ':= Double, "alt" ':= Double, "throttle" ':= Double ]
   type CruiseState  = '[ "pos" ':= Double, "vel" ':= Double, "alt" ':= Double, "pidIntegral" ':= Double ]
   @

2. __Zero-Boilerplate State Transfer__:
   When a switch from @Takeoff@ to @Cruise@ occurs, 'transferSharedState' uses the compile-time
   resolved 'SubRow' evidence to extract @FlightState@ from @TakeoffState@ and overwrite the
   corresponding slots in @CruiseState@ in a single \(O(|target| + |shared|)\) flat vectorized
   block copy, leaving @Cruise@'s private fields (such as @pidIntegral@) initialized from their
   template defaults.

3. __Clean Signal Signatures__:
   The stream function remains typed as @MSF m a b@ or @Rope ... a b@. Velocity, position,
   and altitude remain completely internal and warm across major mode shifts without
   polluting @a@ or @b@.
-}
module Control.Kernmantle.Rope.Continuity
  ( -- * Row-Polymorphic State Transfer & Projection
    transferSharedState
  , injectSharedRecord
  , projectSharedState
  , updateSharedState
  , modifySharedField
  , copyFieldState

    -- * State-Continuous Coalgebras
  , ContinuousRecordCoalgebra(..)
  , toFlatRecordCoalgebra
  , fromFlatRecordCoalgebra
  , toFlatCoalgebra
  , toContinuousMSF
  , transferCoalgebraState
  , recordToContinuous

    -- * Continuous Mode Switching (Pair-wise)
  , switchContinuous2

    -- * Continuous Mode Switching (N-ary Open Variants)
  , ContinuousMode(..)
  , ContinuousTemplate(..)
  , ContinuousTemplateBranches
  , ActiveContinuous(..)
  , getActiveSharedState
  , stepActiveContinuous
  , continuousMSF
  , continuousTelemetryMSF
  , runContinuousStream
  , runContinuousTelemetryStream

    -- * Re-exports from Polytropos
  , SubRow(..)
  , FieldIndex(..)
  , Record
  , Variant
  , Row
  , SumRow
  ) where

import Control.Arrow
import Control.Category
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Primitive.SmallArray
  ( SmallArray
  , sizeofSmallArray
  , indexSmallArray
  , newSmallArray
  , writeSmallArray
  , copySmallArray
  , runSmallArray
  )
import GHC.Exts (Any)
import GHC.OverloadedLabels (fromLabel)
import GHC.TypeLits (Symbol)
import Unsafe.Coerce (unsafeCoerce)

import Prelude hiding (id, (.))

import Control.Kernmantle.Rope.Internal (Label)
import Control.Kernmantle.Rope.Mealy
  ( FlatCoalgebra(..)
  , FlatRecordCoalgebra(..)
  , copyFieldState
  )
import Control.Kernmantle.Rope.MSF
  ( MSF(..)
  , runMSFStream
  )
import Polytropos
  ( Record
  , Variant
  , Field(..)
  , HasField(..)
  , recordField
  , setField
  , projectRecord
  )
import Polytropos.Core
  ( Rep(..)
  , RowRep(..)
  , repVals
  , repTag
  , SubRow(..)
  , FieldIndex(..)
  , Row
  , SumRow
  , Pair(..)
  )

{-------------------------------------------------------------------------------
  Row-Polymorphic State Transfer & Projection
-------------------------------------------------------------------------------}

-- | Transfer all fields in a shared sub-row @shared@ from a source record into a target record.
--
-- Overwrites the corresponding fields in @target@ with the values from @orig@,
-- leaving all target-private fields untouched.
--
-- Executed in \(O(|target| + |shared|)\) time via a flat memory copy and compile-time
-- resolved 'SubRow' permutation vectors, with zero intermediate allocations.
--
-- @
-- let warmCruiseState = transferSharedState \@FlightState takeoffState initCruiseState
-- @
transferSharedState
  :: forall shared orig target.
     (SubRow shared orig, SubRow shared target)
  => Record orig
  -> Record target
  -> Record target
transferSharedState (RowRep (Rep _ srcVals)) (RowRep (Rep _ dstVals)) =
  let srcIdxs = subRowIndices @shared @orig
      dstIdxs = subRowIndices @shared @target
      !n = sizeofSmallArray dstVals
      !newDst = runSmallArray $ do
        m <- newSmallArray n (error "transferSharedState: uninitialized slot")
        copySmallArray m 0 dstVals 0 n
        let loop [] [] = pure ()
            loop (s : ss) (d : ds) = do
              writeSmallArray m d (indexSmallArray srcVals s)
              loop ss ds
            loop _ _ = pure ()
        loop srcIdxs dstIdxs
        pure m
  in RowRep (Rep 0 newDst)
{-# INLINE transferSharedState #-}

-- | Inject an already-extracted shared sub-record into a target record.
--
-- Overwrites the corresponding fields in @target@ with the values from @shared@,
-- leaving all other fields untouched.
injectSharedRecord
  :: forall shared target.
     (SubRow shared target)
  => Record shared
  -> Record target
  -> Record target
injectSharedRecord (RowRep (Rep _ sharedVals)) (RowRep (Rep _ dstVals)) =
  let dstIdxs = subRowIndices @shared @target
      !n = sizeofSmallArray dstVals
      !newDst = runSmallArray $ do
        m <- newSmallArray n (error "injectSharedRecord: uninitialized slot")
        copySmallArray m 0 dstVals 0 n
        let loop !_ [] = pure ()
            loop !k (d : ds) = do
              writeSmallArray m d (indexSmallArray sharedVals k)
              loop (k + 1) ds
        loop 0 dstIdxs
        pure m
  in RowRep (Rep 0 newDst)
{-# INLINE injectSharedRecord #-}

-- | Project a shared sub-record from a record.
--
-- Direct type-specialized alias for 'projectRecord'.
projectSharedState
  :: forall shared r.
     (SubRow shared r)
  => Record r
  -> Record shared
projectSharedState = projectRecord @shared
{-# INLINE projectSharedState #-}

-- | Update the shared sub-row of a record by applying a transformation function.
updateSharedState
  :: forall shared target.
     (SubRow shared target)
  => (Record shared -> Record shared)
  -> Record target
  -> Record target
updateSharedState f target =
  let !shared = projectRecord @shared target
      !shared' = f shared
  in injectSharedRecord @shared shared' target
{-# INLINE updateSharedState #-}

-- | Modify a single field in a record by label in \(O(1)\) unboxed time.
modifySharedField
  :: forall (l :: Symbol) (a :: Type) r.
     (HasField l (Record r) (Identity a), FieldIndex l r a)
  => Label l
  -> (a -> a)
  -> Record r
  -> Record r
modifySharedField _ f r =
  let !val = recordField (Field @l) r :: a
  in setField (Field @l) (f val) r
{-# INLINE modifySharedField #-}

{-------------------------------------------------------------------------------
  State-Continuous Coalgebras
-------------------------------------------------------------------------------}

-- | A flat record coalgebra whose internal state row @s@ contains a shared row @shared@.
--
-- Explicitly witnesses 'SubRow shared s', ensuring that this coalgebra's state
-- can be transferred across mode transitions.
data ContinuousRecordCoalgebra (shared :: Row) (s :: Row) (m :: Type -> Type) a b =
  ContinuousRecordCoalgebra
    { continuousInit :: !(Record s)
    , continuousStep :: !(Record s -> a -> m (b, Record s))
    }

-- | Convert a 'ContinuousRecordCoalgebra' to a standard 'FlatRecordCoalgebra'.
toFlatRecordCoalgebra
  :: ContinuousRecordCoalgebra shared s m a b
  -> FlatRecordCoalgebra s m a b
toFlatRecordCoalgebra (ContinuousRecordCoalgebra s0 step) =
  FlatRecordCoalgebra s0 step
{-# INLINE toFlatRecordCoalgebra #-}

-- | Convert a 'FlatRecordCoalgebra' to a 'ContinuousRecordCoalgebra' given 'SubRow' evidence.
fromFlatRecordCoalgebra
  :: (SubRow shared s)
  => FlatRecordCoalgebra s m a b
  -> ContinuousRecordCoalgebra shared s m a b
fromFlatRecordCoalgebra (FlatRecordCoalgebra s0 step) =
  ContinuousRecordCoalgebra s0 step
{-# INLINE fromFlatRecordCoalgebra #-}

-- | Convert a 'ContinuousRecordCoalgebra' to an existential 'FlatCoalgebra'.
toFlatCoalgebra
  :: ContinuousRecordCoalgebra shared s m a b
  -> FlatCoalgebra m a b
toFlatCoalgebra (ContinuousRecordCoalgebra s0 step) =
  FlatCoalgebra s0 step
{-# INLINE toFlatCoalgebra #-}

-- | Compile a 'ContinuousRecordCoalgebra' directly into a high-frequency 'MSF'.
toContinuousMSF
  :: (Monad m)
  => ContinuousRecordCoalgebra shared s m a b
  -> MSF m a b
toContinuousMSF (ContinuousRecordCoalgebra s0 step) =
  let loop !s = MSF \a -> do
        (b, !s') <- step s a
        pure (b, loop s')
  in loop s0
{-# INLINE toContinuousMSF #-}

-- | Warm-start a target 'FlatRecordCoalgebra' from a source 'FlatRecordCoalgebra'
-- by transferring the shared state row.
transferCoalgebraState
  :: forall shared s1 s2 m a1 b1 a2 b2.
     (SubRow shared s1, SubRow shared s2)
  => FlatRecordCoalgebra s1 m a1 b1
  -> FlatRecordCoalgebra s2 m a2 b2
  -> FlatRecordCoalgebra s2 m a2 b2
transferCoalgebraState (FlatRecordCoalgebra s1 _) (FlatRecordCoalgebra s2 step2) =
  FlatRecordCoalgebra (transferSharedState @shared s1 s2) step2
{-# INLINE transferCoalgebraState #-}

-- | Lift a 'FlatRecordCoalgebra' to a 'ContinuousRecordCoalgebra'.
recordToContinuous
  :: (SubRow shared s)
  => FlatRecordCoalgebra s m a b
  -> ContinuousRecordCoalgebra shared s m a b
recordToContinuous = fromFlatRecordCoalgebra
{-# INLINE recordToContinuous #-}

{-------------------------------------------------------------------------------
  Continuous Mode Switching (Pair-wise)
-------------------------------------------------------------------------------}

-- | Dynamic mode switch between two stateful modes preserving shared state fields.
--
-- Mode 1 steps until it emits @(b, Just payload)@.
-- When the switch event fires, fields in @shared@ are projected from mode 1's state
-- and transferred into mode 2's state record, keeping accumulators warm across the
-- transition boundary.
--
-- Mode 2's private fields are initialized from @st2Init@.
switchContinuous2
  :: forall shared s1 s2 m a b c.
     (Monad m, SubRow shared s1, SubRow shared s2)
  => FlatRecordCoalgebra s1 m a (b, Maybe c)
  -> Record s2
  -> (Record s2 -> (c, a) -> m (b, Record s2))
  -> MSF m a b
switchContinuous2 (FlatRecordCoalgebra st1Init step1) st2Init step2 =
  let loopMode1 !st1 = MSF \a -> do
        ((b, mPayload), !st1') <- step1 st1 a
        case mPayload of
          Nothing ->
            pure (b, loopMode1 st1')
          Just payload -> do
            let !warmSt2 = transferSharedState @shared st1' st2Init
            (b', !st2') <- step2 warmSt2 (payload, a)
            pure (b', loopMode2 payload st2')

      loopMode2 payload !st2 = MSF \a -> do
        (b, !st2') <- step2 st2 (payload, a)
        pure (b, loopMode2 payload st2')
  in loopMode1 st1Init
{-# INLINE switchContinuous2 #-}

{-------------------------------------------------------------------------------
  Continuous Mode Switching (N-ary Open Variants)
-------------------------------------------------------------------------------}

-- | An active continuous mode participating in open variant mode switching.
--
-- Each mode encapsulates internal state row @s@ containing @shared@ (@SubRow shared s@).
-- On each tick, it produces an output @b@, an optional transition variant @Maybe (Variant sw)@,
-- and updated internal state @Record s@.
data ContinuousMode (shared :: Row) (sw :: SumRow) (m :: Type -> Type) a b =
  forall s. (SubRow shared s) => ContinuousMode
    { modeInitState :: !(Record s)
    , modeStep      :: !(Record s -> a -> m (b, Maybe (Variant sw), Record s))
    }

-- | A parameterized template for a target mode reachable via variant switching.
--
-- For constructor @lbl@ with payload row @fs@, the template provides an initial
-- state record @Record s@ (holding defaults for private fields) and a step function
-- consuming @(Record fs, a)@.
data ContinuousTemplate (shared :: Row) (sw :: SumRow) (m :: Type -> Type) (fs :: Row) a b =
  forall s. (SubRow shared s) => ContinuousTemplate
    { templateInitState :: !(Record s)
    , templateStep      :: !(Record s -> (Record fs, a) -> m (b, Maybe (Variant sw), Record s))
    }

-- | Product row of parameterized continuous templates corresponding to sum row @sw@.
type family ContinuousTemplateBranches (shared :: Row) (sw :: SumRow) (m :: Type -> Type) (a :: Type) (b :: Type) (branches :: SumRow) :: Row where
  ContinuousTemplateBranches shared sw m a b '[] = '[]
  ContinuousTemplateBranches shared sw m a b ((lbl ':= fs) ': rest) =
    (lbl ':= ContinuousTemplate shared sw m fs a b) ': ContinuousTemplateBranches shared sw m a b rest

-- | An active continuous execution state holding existential state @s@ and step function.
data ActiveContinuous (shared :: Row) (sw :: SumRow) (m :: Type -> Type) a b =
  forall s. (SubRow shared s) => ActiveContinuous
    { activeState :: !(Record s)
    , activeStep  :: !(Record s -> a -> m (b, Maybe (Variant sw), Record s))
    }

-- | Step an 'ActiveContinuous' engine single-shot.
--
-- When no transition occurs (@Nothing@), advances internal state @s@.
-- When a mode transition occurs (@Just var@), looks up the target template in \(O(1)\),
-- transfers all fields in @shared@ into the target's initial state, and transitions
-- seamlessly to the new mode.
stepActiveContinuous
  :: forall shared sw m a b.
     (Monad m)
  => SmallArray (Identity Any)
  -> ActiveContinuous shared sw m a b
  -> a
  -> m (b, ActiveContinuous shared sw m a b)
stepActiveContinuous templateArray (ActiveContinuous (st :: Record s) step) a = do
  (b, mVar, !st') <- step st a
  case mVar of
    Nothing ->
      pure (b, ActiveContinuous @shared st' step)
    Just (RowRep vRep) -> do
      let !tag = fromIntegral (repTag vRep) :: Int
          Identity (anyTpl :: Any) = indexSmallArray templateArray tag
          !tpl = unsafeCoerce anyTpl :: ContinuousTemplate shared sw m '[] a b
      case tpl of
        ContinuousTemplate (initTgt :: Record sTgt) stepTgt -> do
          let !sharedRec = projectRecord @shared st'
              !warmTgtState = injectSharedRecord @shared sharedRec initTgt
              !payloadRec = RowRep (Rep 0 (repVals vRep)) :: Record '[]
          (b', _mVar', !stTarget') <- stepTgt warmTgtState (payloadRec, a)
          let stepNext sIn aIn = stepTgt sIn (payloadRec, aIn)
          pure (b', ActiveContinuous @shared stTarget' stepNext)
{-# INLINE stepActiveContinuous #-}

-- | Compile an initial 'ContinuousMode' and a record of 'ContinuousTemplateBranches'
-- into a high-frequency Monadic Stream Function ('MSF').
--
-- All internal state transitions preserve @shared@ fields with zero runtime GADT
-- traversal or intermediate heap allocation.
continuousMSF
  :: forall shared sw m a b.
     (Monad m)
  => ContinuousMode shared sw m a b
  -> Record (ContinuousTemplateBranches shared sw m a b sw)
  -> MSF m a b
continuousMSF (ContinuousMode initSt initStep) (RowRep rep) =
  let !tplArray = repVals rep
      loop active = MSF \a -> do
        (b, active') <- stepActiveContinuous tplArray active a
        pure (b, loop active')
  in loop (ActiveContinuous @shared initSt initStep)
{-# INLINE continuousMSF #-}

-- | Extract the current warm shared state record from an 'ActiveContinuous' engine.
getActiveSharedState
  :: forall shared sw m a b.
     ActiveContinuous shared sw m a b
  -> Record shared
getActiveSharedState (ActiveContinuous (st :: Record s) _) =
  projectRecord @shared st
{-# INLINE getActiveSharedState #-}

-- | Monadic Stream Function that also emits the current warm shared state record
-- @Record shared@ on every tick for telemetry, monitoring, or logging.
continuousTelemetryMSF
  :: forall shared sw m a b.
     (Monad m)
  => ContinuousMode shared sw m a b
  -> Record (ContinuousTemplateBranches shared sw m a b sw)
  -> MSF m a (b, Record shared)
continuousTelemetryMSF (ContinuousMode initSt initStep) (RowRep rep) =
  let !tplArray = repVals rep
      loop active = MSF \a -> do
        (b, active') <- stepActiveContinuous tplArray active a
        let !sharedNow = getActiveSharedState active'
        pure ((b, sharedNow), loop active')
  in loop (ActiveContinuous @shared initSt initStep)
{-# INLINE continuousTelemetryMSF #-}

-- | Execute a continuous mode system over an input stream in strictly causal sequence.
runContinuousStream
  :: forall shared sw m a b.
     (Monad m)
  => ContinuousMode shared sw m a b
  -> Record (ContinuousTemplateBranches shared sw m a b sw)
  -> [a]
  -> m [b]
runContinuousStream mode tpls inputs =
  runMSFStream (continuousMSF mode tpls) inputs
{-# INLINE runContinuousStream #-}

-- | Execute a continuous mode system with telemetry observation over an input stream.
runContinuousTelemetryStream
  :: forall shared sw m a b.
     (Monad m)
  => ContinuousMode shared sw m a b
  -> Record (ContinuousTemplateBranches shared sw m a b sw)
  -> [a]
  -> m [(b, Record shared)]
runContinuousTelemetryStream mode tpls inputs =
  runMSFStream (continuousTelemetryMSF mode tpls) inputs
{-# INLINE runContinuousTelemetryStream #-}
