{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Control.Kernmantle.Rope.Mealy
Description : Existential Mealy machine strand, flat coalgebra compilation, and state-lens extraction
Copyright   : (c) 2026 The Kernmantle Authors
License     : BSD-3-Clause
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides an existential Mealy machine binary effect strand ('MealyEff'), a compiled
flat coalgebra arrow engine ('FlatCoalgebra'), and existential state-lens extraction
combinators for 'Rope' computations.

= Architectural Overview & Rationale
[ref:note-existential-state-lens-extraction]

In continuous and high-frequency reactive systems (e.g., 1000 Hz robotics, avionics,
audio DSP, and physics simulations), rebuilding or traversing a deep 'Rope' GADT tree
on every discrete tick introduces unacceptable garbage collector allocation and pointer
indirection overhead.

'Control.Kernmantle.Rope.Mealy' resolves the __state persistence problem__ via:

1. __Coalgebraic Compilation__:
   During pipeline elaboration / weaving ('entwine'), individual stateful strands ('MealyEff')
   are compiled into a flat coalgebra ('FlatCoalgebra m a b'). The 'Rope' GADT is inspected
   and traversed __only once__ at startup. Execution at runtime steps a monolithic transition
   function @s -> a -> m (b, s)@ directly, completely bypassing the 'Rope' AST.

2. __Existential State-Lens Extraction ('StateLens')__:
   Rather than hardcoding monolithic state representations into arrow boundaries, each strand's
   state @s@ is focused through a first-class 'StateLens'. Sequential (@(.)@) and parallel (@(***)@)
   arrow composition fuses states into composite product pairs @(s1, s2)@, while Polytropos
   record state registers ('FlatRecordCoalgebra') allow \(O(1)\) unboxed slot indexing via 'FieldIndex'.

3. __Row-Polymorphic State Continuity across Switches__:
   Using Polytropos 'SubRow' evidence ('projectRecord' / 'copyFieldState'), shared state fields
   (such as Kalman filter estimates or PID accumulators) are preserved across discrete mode
   transitions without polluting top-level input/output signals @a@ and @b@.
-}
module Control.Kernmantle.Rope.Mealy
  ( -- * Existential State-Lens Extraction
    StateLens(..)
  , lensModify
  , idLens
  , fstLens
  , sndLens
  , composeLens
  , recordFieldLens
  , liftStepWithLens
  , coalgWithLens

    -- * Existential Mealy Machine Strand
  , MealyEff(..)
  , mealyStrand
  , mealyPure

    -- * Compiled Flat Coalgebra Arrow Engine
  , FlatCoalgebra(..)
  , stepCoalgebra
  , stepPureCoalgebra
  , runCoalgebraStream
  , foldCoalgebra

    -- * Polytropos Record Register Bank Coalgebra
  , FlatRecordCoalgebra(..)
  , stepRecordCoalgebra
  , recordCoalgebraToFlat
  , preserveSharedState
  , copyFieldState

    -- * Interpreters & Conversion Bridges
  , extractMealy
  , interpMealyIO
  , interpCoalgebraIO
  , feedbackToCoalgebra
  , coalgebraToFeedback
  , mealyToFeedback
  , feedbackToMealy
  ) where

import Control.Arrow
import Control.Category
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor.Identity (Identity(..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Profunctor
import GHC.TypeLits (Symbol)

import Prelude hiding (id, (.))

import GHC.Records (HasField(..))

import Polytropos
  ( Record
  , Field(..)
  , SubRow
  , recordField
  , setField
  , projectRecord
  )
import Polytropos.Core
  ( FieldIndex(..)
  )

import Control.Kernmantle.Rope.Internal
  ( Rope
  , LooseRope
  , TightRope
  , strand
  , InRope
  , Strand
  , BinEff
  , Label
  )
import Control.Kernmantle.Rope.Feedback
  ( FeedbackEff(..)
  )

{-------------------------------------------------------------------------------
  Existential State-Lens Extraction
-------------------------------------------------------------------------------}

-- | A first-class lens focusing on a component of state @s@ within a larger state @bigS@.
data StateLens s bigS = StateLens
  { lensGet :: !(bigS -> s)
  , lensSet :: !(s -> bigS -> bigS)
  }

-- | Functionally update the focused state component.
lensModify :: StateLens s bigS -> (s -> s) -> bigS -> bigS
lensModify l f bigS = lensSet l (f (lensGet l bigS)) bigS
{-# INLINE lensModify #-}

-- | Identity state lens.
idLens :: StateLens s s
idLens = StateLens id (\new _ -> new)
{-# INLINE idLens #-}

-- | Left component lens for product state pairs @(s1, s2)@.
fstLens :: StateLens s1 (s1, s2)
fstLens = StateLens fst (\s1' (_, s2) -> (s1', s2))
{-# INLINE fstLens #-}

-- | Right component lens for product state pairs @(s1, s2)@.
sndLens :: StateLens s2 (s1, s2)
sndLens = StateLens snd (\s2' (s1, _) -> (s1, s2'))
{-# INLINE sndLens #-}

-- | Compose two lenses: focus into a sub-component of a nested state.
composeLens :: StateLens s mid -> StateLens mid bigS -> StateLens s bigS
composeLens inner outer = StateLens
  { lensGet = lensGet inner . lensGet outer
  , lensSet = \s' bigS ->
      let mid = lensGet outer bigS
          mid' = lensSet inner s' mid
      in lensSet outer mid' bigS
  }
{-# INLINE composeLens #-}

-- | Construct a 'StateLens' targeting a named field in a Polytropos 'Record' register bank.
--
-- Achieves \(O(1)\) unboxed slot indexing via 'FieldIndex'.
recordFieldLens
  :: forall l a r. (HasField l (Record r) (Identity a), FieldIndex l r a)
  => Label l
  -> StateLens a (Record r)
recordFieldLens _ = StateLens
  { lensGet = recordField (Field @l)
  , lensSet = \val record -> setField (Field @l) val record
  }
{-# INLINE recordFieldLens #-}

-- | Lift a local step function @s -> a -> m (b, s)@ into a step function over composite state @bigS@.
liftStepWithLens
  :: (Monad m)
  => StateLens s bigS
  -> (s -> a -> m (b, s))
  -> (bigS -> a -> m (b, bigS))
liftStepWithLens lens step bigS a = do
  let !s = lensGet lens bigS
  (b, !s') <- step s a
  let !bigS' = lensSet lens s' bigS
  pure (b, bigS')
{-# INLINE liftStepWithLens #-}

-- | Construct a 'FlatCoalgebra' over composite state @bigS@ by lifting a local step function via a 'StateLens'.
coalgWithLens
  :: (Monad m)
  => StateLens s bigS
  -> bigS
  -> s
  -> (s -> a -> m (b, s))
  -> FlatCoalgebra m a b
coalgWithLens lens bigS0 s0 step =
  let !initBigS = lensSet lens s0 bigS0
  in FlatCoalgebra initBigS (liftStepWithLens lens step)
{-# INLINE coalgWithLens #-}

{-------------------------------------------------------------------------------
  Existential Mealy Machine Strand
-------------------------------------------------------------------------------}

-- | A binary effect strand carrying encapsulated local state @s@
-- and a transition function @s -> a -> m (b, s)@.
data MealyEff (m :: Type -> Type) a b where
  MealyStrand
    :: !s
    -> !(s -> a -> m (b, s))
    -> MealyEff m a b

-- | Inject a 'MealyEff' computation into a 'Rope'.
mealyStrand
  :: (InRope l (MealyEff m) (Rope record mantle core))
  => Label l
  -> s
  -> (s -> a -> m (b, s))
  -> Rope record mantle core a b
mealyStrand lbl s0 step = strand lbl (MealyStrand s0 step)
{-# INLINE mealyStrand #-}

-- | Construct a pure 'MealyEff' where the transition does not require effectful actions.
mealyPure
  :: (Applicative m)
  => s
  -> (s -> a -> (b, s))
  -> MealyEff m a b
mealyPure s0 step = MealyStrand s0 (\s a -> pure (step s a))
{-# INLINE mealyPure #-}

{-------------------------------------------------------------------------------
  Compiled Flat Coalgebra Arrow Engine
-------------------------------------------------------------------------------}

-- | A compiled flat coalgebra carrying existential internal state @s@
-- and a monolithic step function.
--
-- Forms a complete 'Arrow' and 'ArrowChoice'. During tick loops, the step function
-- is called directly without traversing or interpreting any 'Rope' ASTs.
data FlatCoalgebra (m :: Type -> Type) a b = forall s. FlatCoalgebra
  { coalgInit :: !s
  , coalgStep :: !(s -> a -> m (b, s))
  }

instance (Monad m) => Category (FlatCoalgebra m) where
  id = FlatCoalgebra () (\() a -> pure (a, ()))
  {-# INLINE id #-}

  FlatCoalgebra s2 g . FlatCoalgebra s1 f = FlatCoalgebra (s1, s2) $ \(st1, st2) a -> do
    (b, !st1') <- f st1 a
    (c, !st2') <- g st2 b
    pure (c, (st1', st2'))
  {-# INLINE (.) #-}

instance (Monad m) => Arrow (FlatCoalgebra m) where
  arr fn = FlatCoalgebra () (\() a -> pure (fn a, ()))
  {-# INLINE arr #-}

  first (FlatCoalgebra s f) = FlatCoalgebra s $ \st (b, d) -> do
    (c, !st') <- f st b
    pure ((c, d), st')
  {-# INLINE first #-}

  second (FlatCoalgebra s f) = FlatCoalgebra s $ \st (d, b) -> do
    (c, !st') <- f st b
    pure ((d, c), st')
  {-# INLINE second #-}

  FlatCoalgebra s1 f *** FlatCoalgebra s2 g = FlatCoalgebra (s1, s2) $ \(st1, st2) (b, b') -> do
    (c, !st1') <- f st1 b
    (c', !st2') <- g st2 b'
    pure ((c, c'), (st1', st2'))
  {-# INLINE (***) #-}

instance (Monad m) => ArrowChoice (FlatCoalgebra m) where
  left (FlatCoalgebra s f) = FlatCoalgebra s $ \st -> \case
    Left b -> do
      (c, !st') <- f st b
      pure (Left c, st')
    Right d ->
      pure (Right d, st)
  {-# INLINE left #-}

  right (FlatCoalgebra s f) = FlatCoalgebra s $ \st -> \case
    Left d ->
      pure (Left d, st)
    Right b -> do
      (c, !st') <- f st b
      pure (Right c, st')
  {-# INLINE right #-}

  FlatCoalgebra s1 f +++ FlatCoalgebra s2 g = FlatCoalgebra (s1, s2) $ \(st1, st2) -> \case
    Left b -> do
      (c, !st1') <- f st1 b
      pure (Left c, (st1', st2))
    Right b' -> do
      (c', !st2') <- g st2 b'
      pure (Right c', (st1, st2'))
  {-# INLINE (+++) #-}

instance (Monad m) => Profunctor (FlatCoalgebra m) where
  dimap l r (FlatCoalgebra s f) = FlatCoalgebra s $ \st a -> do
    (b, !st') <- f st (l a)
    pure (r b, st')
  {-# INLINE dimap #-}

instance (Monad m) => Strong (FlatCoalgebra m) where
  first' = first
  {-# INLINE first' #-}
  second' = second
  {-# INLINE second' #-}

instance (Monad m) => Choice (FlatCoalgebra m) where
  left' = left
  {-# INLINE left' #-}
  right' = right
  {-# INLINE right' #-}

instance (Monad m) => Functor (FlatCoalgebra m a) where
  fmap = rmap
  {-# INLINE fmap #-}

-- | Execute a single discrete step on a 'FlatCoalgebra', returning the computed output
-- and the next 'FlatCoalgebra' holding the updated internal state.
stepCoalgebra
  :: (Monad m)
  => FlatCoalgebra m a b
  -> a
  -> m (b, FlatCoalgebra m a b)
stepCoalgebra (FlatCoalgebra s step) a = do
  (b, !s') <- step s a
  pure (b, FlatCoalgebra s' step)
{-# INLINE stepCoalgebra #-}

-- | Execute a single discrete step on a pure 'FlatCoalgebra'.
stepPureCoalgebra
  :: FlatCoalgebra Identity a b
  -> a
  -> (b, FlatCoalgebra Identity a b)
stepPureCoalgebra (FlatCoalgebra s step) a =
  let Identity (b, !s') = step s a
  in (b, FlatCoalgebra s' step)
{-# INLINE stepPureCoalgebra #-}

-- | Execute a compiled 'FlatCoalgebra' over an input list in strictly causal sequence.
runCoalgebraStream
  :: (Monad m)
  => FlatCoalgebra m a b
  -> [a]
  -> m [b]
runCoalgebraStream (FlatCoalgebra s0 step) inputs = loop s0 inputs
  where
    loop !_ [] = pure []
    loop !s (x:xs) = do
      (y, !s') <- step s x
      ys <- loop s' xs
      pure (y : ys)
{-# INLINE runCoalgebraStream #-}

-- | Fold a 'FlatCoalgebra' over a list of inputs, returning the last output (if any)
-- and the final 'FlatCoalgebra'.
foldCoalgebra
  :: (Monad m)
  => FlatCoalgebra m a b
  -> [a]
  -> m (Maybe b, FlatCoalgebra m a b)
foldCoalgebra (FlatCoalgebra s0 step) inputs = loop s0 Nothing inputs
  where
    loop !s mLast [] = pure (mLast, FlatCoalgebra s step)
    loop !s _ (x:xs) = do
      (y, !s') <- step s x
      loop s' (Just y) xs
{-# INLINE foldCoalgebra #-}

{-------------------------------------------------------------------------------
  Polytropos Record Register Bank Coalgebra
-------------------------------------------------------------------------------}

-- | A flat coalgebra whose internal state is stored in a Polytropos 'Record' register bank.
--
-- Each individual stateful strand accesses its assigned slot in \(O(1)\) unboxed time
-- via compile-time resolved 'FieldIndex' offsets.
data FlatRecordCoalgebra s_row m a b = FlatRecordCoalgebra
  { recordCoalgInit :: !(Record s_row)
  , recordCoalgStep :: !(Record s_row -> a -> m (b, Record s_row))
  }

-- | Execute a single discrete step on a 'FlatRecordCoalgebra'.
stepRecordCoalgebra
  :: (Monad m)
  => FlatRecordCoalgebra s_row m a b
  -> a
  -> m (b, FlatRecordCoalgebra s_row m a b)
stepRecordCoalgebra (FlatRecordCoalgebra st step) a = do
  (b, !st') <- step st a
  pure (b, FlatRecordCoalgebra st' step)
{-# INLINE stepRecordCoalgebra #-}

-- | Convert a 'FlatRecordCoalgebra' into an existential 'FlatCoalgebra'.
recordCoalgebraToFlat
  :: FlatRecordCoalgebra s_row m a b
  -> FlatCoalgebra m a b
recordCoalgebraToFlat (FlatRecordCoalgebra st step) =
  FlatCoalgebra st step
{-# INLINE recordCoalgebraToFlat #-}

-- | Project shared state fields from an exiting record when switching modes.
--
-- Allows PID controllers, Kalman filters, and moving average accumulators to remain
-- warm across mode transitions without leaking into input/output types.
preserveSharedState
  :: forall shared orig target.
     (SubRow shared orig)
  => (Record shared -> Record target -> Record target)
  -> Record orig
  -> Record target
  -> Record target
preserveSharedState updateTarget orig target =
  let shared = projectRecord @shared orig
  in updateTarget shared target
{-# INLINE preserveSharedState #-}

-- | Copy a single named state field from a source record to a target record in \(O(1)\).
copyFieldState
  :: forall (l :: Symbol) (a :: Type) r1 r2.
     (HasField l (Record r1) (Identity a), FieldIndex l r2 a)
  => Label l
  -> Record r1
  -> Record r2
  -> Record r2
copyFieldState _ src dst =
  let !val = recordField (Field @l) src :: a
  in setField (Field @l) val dst
{-# INLINE copyFieldState #-}

{-------------------------------------------------------------------------------
  Interpreters & Conversion Bridges
-------------------------------------------------------------------------------}

-- | Extract a 'MealyEff' strand directly into a compiled 'FlatCoalgebra'.
extractMealy :: MealyEff m a b -> FlatCoalgebra m a b
extractMealy (MealyStrand s0 step) = FlatCoalgebra s0 step
{-# INLINE extractMealy #-}

-- | Interpret a 'MealyEff' strand into an effectful 'Kleisli' arrow in 'IO'.
--
-- Allocates a dedicated 'IORef' for the strand's local state. The state is strictly
-- updated on every tick without 'Control.Monad.Fix.MonadFix' or '<<loop>>' blackholes.
interpMealyIO
  :: (MonadIO m)
  => MealyEff m a b
  -> IO (Kleisli m a b)
interpMealyIO (MealyStrand s0 step) = do
  ref <- liftIO (newIORef s0)
  pure $ Kleisli \a -> do
    s <- liftIO (readIORef ref)
    (b, !s') <- step s a
    liftIO (writeIORef ref s')
    pure b

-- | Interpret a 'FlatCoalgebra' into a stateful 'Kleisli' arrow in 'IO'.
interpCoalgebraIO
  :: (MonadIO m)
  => FlatCoalgebra m a b
  -> IO (Kleisli m a b)
interpCoalgebraIO (FlatCoalgebra s0 step) = do
  ref <- liftIO (newIORef s0)
  pure $ Kleisli \a -> do
    s <- liftIO (readIORef ref)
    (b, !s') <- step s a
    liftIO (writeIORef ref s')
    pure b

-- | Convert a 'FeedbackEff' strand into a 'FlatCoalgebra'.
feedbackToCoalgebra
  :: (Monad m)
  => FeedbackEff (Kleisli m) a b
  -> FlatCoalgebra m a b
feedbackToCoalgebra (Feedback s0 (Kleisli step)) =
  FlatCoalgebra s0 (\s a -> step (a, s))
{-# INLINE feedbackToCoalgebra #-}

-- | Convert a 'FlatCoalgebra' into a 'FeedbackEff' strand.
coalgebraToFeedback
  :: (Monad m)
  => FlatCoalgebra m a b
  -> FeedbackEff (Kleisli m) a b
coalgebraToFeedback (FlatCoalgebra s0 step) =
  Feedback s0 (Kleisli (\(a, s) -> step s a))
{-# INLINE coalgebraToFeedback #-}

-- | Convert a 'MealyEff' strand into a 'FeedbackEff' strand.
mealyToFeedback
  :: (Monad m)
  => MealyEff m a b
  -> FeedbackEff (Kleisli m) a b
mealyToFeedback (MealyStrand s0 step) =
  Feedback s0 (Kleisli (\(a, s) -> step s a))
{-# INLINE mealyToFeedback #-}

-- | Convert a 'FeedbackEff' strand into a 'MealyEff' strand.
feedbackToMealy
  :: (Monad m)
  => FeedbackEff (Kleisli m) a b
  -> MealyEff m a b
feedbackToMealy (Feedback s0 (Kleisli step)) =
  MealyStrand s0 (\s a -> step (a, s))
{-# INLINE feedbackToMealy #-}
