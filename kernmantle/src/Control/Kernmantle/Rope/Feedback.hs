{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Control.Kernmantle.Rope.Feedback
Description : Causal feedback wire and first-class Mealy arrow combinators for Kernmantle ropes
Copyright   : (c) 2026 The Kernmantle Authors
License     : BSD-3-Clause
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides a first-class causal feedback strand ('FeedbackEff') and stateful arrow transformer
('Causal') for 'Rope' arrow computations.

= Architectural Overview & Rationale
[ref:note-causal-feedback-wire]

In standard Haskell arrow architectures, feedback is expressed via 'Control.Arrow.ArrowLoop':

@
loop :: arr (a, d) (b, d) -> arr a b
@

When arrows are interpreted into monadic effect carrier stacks ('Kleisli m'), 'Control.Arrow.ArrowLoop'
requires 'Control.Monad.Fix.MonadFix' (@mfix@) to tie a knot over the feedback variable @d@:

@
loop (Kleisli f) = Kleisli \\a -> do
  rec (b, d) <- f (a, d)
  pure b
@

This pattern is fundamentally incompatible with strict effects, exceptions, and discrete reactive stepping:
1. __The Blackhole / Bottom Hazard__: If @f@ evaluates @d@ strictly (as in PID controllers, Kalman filters,
   or moving averages), @mfix@ diverges immediately with @<<loop>>@ (thread blocked indefinitely).
2. __Opaque Knot Tying in Static Analysis__: Pre-flight whole-graph analyzers (such as @SomeBinEff f w m a b@)
   cannot statically inspect arrows inside fixed-point recursions without risking infinite loops.

'Control.Kernmantle.Rope.Feedback' solves this by recognizing that in well-formed physical and reactive
systems, feedback is **strictly causal**: the feedback signal is never instantaneous, but always delayed
by one discrete sample tick (\(t-1 \to t\)).

By modeling causal feedback as a first-class binary effect strand ('FeedbackEff') or arrow transformer ('Causal'):
* __Zero 'Control.Monad.Fix.MonadFix'__: Input state \(s_t\) is already determined from tick \(t-1\).
  The computation executes strictly left-to-right without lazy tuple knots or blackholes.
* __Linear Static Analysis__: Pre-flight analyzers (@SomeBinEff@) inspect the inner transition arrow
  @r (a, s) (b, s)@ once in a single linear pass.
* __Warm State Accumulation across Modes__: Internal state is cleanly encapsulated and can be composed
  in parallel (@(***)@) or choice (@(+++)@) without polluting top-level input/output types.
-}
module Control.Kernmantle.Rope.Feedback
  ( -- * Causal Feedback Binary Effect Strand
    FeedbackEff(Feedback, FeedbackEff)
  , causalFeedback

    -- * First-Class Causal Arrow Transformer
  , Causal(..)
  , toCausal
  , fromCausal

    -- * Streaming and Looping Combinators
  , causalLoopA
  , causalLoopRope
  , causalScan
  , causalFold

    -- * Step Execution
  , stepCausal
  , stepCausalM

    -- * Effectful IO Interpretation
  , interpFeedbackIO
  , newCausalIO
  ) where

import Control.Arrow
import Control.Category
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Profunctor
import GHC.TypeLits (Symbol)

import Prelude hiding (id, (.))

import Control.Kernmantle.Rope.Internal
  ( Rope
  , LooseRope
  , TightRope
  , Weaver
  , mkRope
  , runRope
  , strand
  , InRope
  , Strand
  , BinEff
  , Label
  )

{-------------------------------------------------------------------------------
  Causal Feedback Binary Effect Strand
-------------------------------------------------------------------------------}

-- | A binary effect strand carrying encapsulated internal state @s@
-- and a causal transition arrow.
--
-- Unlike 'Control.Arrow.ArrowLoop', 'FeedbackEff' represents a causal step where
-- input state @s@ is already known from the previous tick or initialization.
data FeedbackEff (r :: BinEff) a b where
  Feedback :: !s -> r (a, s) (b, s) -> FeedbackEff r a b

-- | Pattern synonym alias for 'Feedback'.
pattern FeedbackEff :: s -> r (a, s) (b, s) -> FeedbackEff r a b
pattern FeedbackEff s step = Feedback s step

-- | Inject a causal feedback computation into a 'Rope' containing a 'FeedbackEff' strand.
causalFeedback
  :: (InRope l (FeedbackEff (Rope record mantle core)) (Rope record mantle core))
  => Label l
  -> s
  -> Rope record mantle core (a, s) (b, s)
  -> Rope record mantle core a b
causalFeedback lbl s0 step = strand lbl (Feedback s0 step)
{-# INLINE causalFeedback #-}

{-------------------------------------------------------------------------------
  First-Class Causal Arrow Transformer
-------------------------------------------------------------------------------}

-- | A first-class causal Mealy arrow transformer carrying internal state @s@.
--
-- Sequentially composing two 'Causal' arrows fuses their internal states into a
-- strict product pair @(s1, s2)@. Branching via 'Control.Arrow.ArrowChoice'
-- selectively advances the active branch's state while preserving the inactive
-- branch's warm accumulator untouched.
data Causal (arr :: BinEff) a b where
  Causal :: !s -> arr (a, s) (b, s) -> Causal arr a b

-- | Convert a 'FeedbackEff' strand into a first-class 'Causal' arrow.
toCausal :: FeedbackEff arr a b -> Causal arr a b
toCausal (Feedback s step) = Causal s step
{-# INLINE toCausal #-}

-- | Construct a 'FeedbackEff' from a 'Causal' arrow.
fromCausal :: Causal arr a b -> FeedbackEff arr a b
fromCausal (Causal s step) = Feedback s step
{-# INLINE fromCausal #-}

instance (Arrow arr) => Category (Causal arr) where
  id = Causal () (arr (\(a, ()) -> (a, ())))
  {-# INLINE id #-}

  Causal s2 g . Causal s1 f = Causal (s1, s2) $
    arr (\(a, (st1, st2)) -> ((a, st1), st2))
      >>> first f
      >>> arr (\((b, st1'), st2) -> ((b, st2), st1'))
      >>> first g
      >>> arr (\((c, st2'), st1') -> (c, (st1', st2')))
  {-# INLINE (.) #-}

instance (ArrowChoice arr) => Arrow (Causal arr) where
  arr f = Causal () (arr (\(a, ()) -> (f a, ())))
  {-# INLINE arr #-}

  first (Causal s f) = Causal s $
    arr (\((b, d), st) -> ((b, st), d))
      >>> first f
      >>> arr (\((c, st'), d) -> ((c, d), st'))
  {-# INLINE first #-}

  second (Causal s f) = Causal s $
    arr (\((d, b), st) -> ((b, st), d))
      >>> first f
      >>> arr (\((c, st'), d) -> ((d, c), st'))
  {-# INLINE second #-}

  Causal s1 f *** Causal s2 g = Causal (s1, s2) $
    arr (\((b, b'), (st1, st2)) -> ((b, st1), (b', st2)))
      >>> (f *** g)
      >>> arr (\((c, st1'), (c', st2')) -> ((c, c'), (st1', st2')))
  {-# INLINE (***) #-}

instance (ArrowChoice arr) => ArrowChoice (Causal arr) where
  left (Causal s f) = Causal s $
    arr (\case
           (Left b, st)  -> Left (b, st)
           (Right d, st) -> Right (d, st))
      >>> (f +++ arr (\(d, st) -> (d, st)))
      >>> arr (\case
            Left (c, st')  -> (Left c, st')
            Right (d, st') -> (Right d, st'))
  {-# INLINE left #-}

  right (Causal s f) = Causal s $
    arr (\case
           (Left d, st)  -> Left (d, st)
           (Right b, st) -> Right (b, st))
      >>> (arr (\(d, st) -> (d, st)) +++ f)
      >>> arr (\case
            Left (d, st')  -> (Left d, st')
            Right (c, st') -> (Right c, st'))
  {-# INLINE right #-}

  Causal s1 f +++ Causal s2 g = Causal (s1, s2) $
    arr (\case
           (Left b, (st1, st2))   -> Left ((b, st1), st2)
           (Right b', (st1, st2)) -> Right ((b', st2), st1))
      >>> (first f +++ first g)
      >>> arr (\case
            Left ((c, st1'), st2)   -> (Left c, (st1', st2))
            Right ((c', st2'), st1) -> (Right c', (st1, st2'))
          )
  {-# INLINE (+++) #-}

instance (ArrowChoice arr) => Profunctor (Causal arr) where
  dimap l r (Causal s f) = Causal s $
    arr (\(a', st) -> (l a', st))
      >>> f
      >>> arr (\(b', st') -> (r b', st'))
  {-# INLINE dimap #-}

instance (ArrowChoice arr) => Strong (Causal arr) where
  first' = first
  {-# INLINE first' #-}
  second' = second
  {-# INLINE second' #-}

instance (ArrowChoice arr) => Choice (Causal arr) where
  left' = left
  {-# INLINE left' #-}
  right' = right
  {-# INLINE right' #-}

instance (ArrowChoice arr) => Functor (Causal arr a) where
  fmap = rmap
  {-# INLINE fmap #-}

{-------------------------------------------------------------------------------
  Streaming and Looping Combinators
-------------------------------------------------------------------------------}

-- | Execute a causal step arrow repeatedly over an input list in strictly causal order.
--
-- Passes state strictly from element to element without 'Control.Arrow.ArrowLoop' or 'Control.Monad.Fix.MonadFix'.
causalLoopA
  :: (ArrowChoice core)
  => s
  -> core (a, s) (b, s)
  -> core [a] [b]
causalLoopA s0 step =
  arr (\inputs -> (inputs, s0))
    >>> causalStepLoop step
    >>> arr fst
  where
    causalStepLoop st = proc (inputs, s) -> case inputs of
      [] -> returnA -< ([], s)
      (x:xs) -> do
        (y, !s') <- st -< (x, s)
        (ys, !s'') <- causalStepLoop st -< (xs, s')
        returnA -< (y:ys, s'')

-- | Weave causal feedback over input streams/lists directly in a 'Rope' computation.
causalLoopRope
  :: (ArrowChoice core)
  => s
  -> Rope record mantle core (a, s) (b, s)
  -> Rope record mantle core [a] [b]
causalLoopRope s0 stepRope = mkRope $ \weavers ->
  let !stepCore = runRope stepRope weavers
  in causalLoopA s0 stepCore
{-# INLINE causalLoopRope #-}

-- | Synonym for 'causalLoopA' emphasizing running scan accumulators.
causalScan
  :: (ArrowChoice core)
  => s
  -> core (a, s) (b, s)
  -> core [a] [b]
causalScan = causalLoopA
{-# INLINE causalScan #-}

-- | Execute a causal feedback loop over a list of inputs, returning the last output
-- (if any) and the final accumulated state.
causalFold
  :: (ArrowChoice core)
  => s
  -> core (a, s) (b, s)
  -> core [a] (Maybe b, s)
causalFold s0 step =
  arr (\inputs -> (inputs, s0, Nothing))
    >>> causalFoldLoop step
  where
    causalFoldLoop st = proc (inputs, s, mLast) -> case inputs of
      [] -> returnA -< (mLast, s)
      (x:xs) -> do
        (y, !s') <- st -< (x, s)
        causalFoldLoop st -< (xs, s', Just y)

{-------------------------------------------------------------------------------
  Step Execution
-------------------------------------------------------------------------------}

-- | Execute a pure 'Causal' arrow on a single input, returning the computed output
-- and the next 'Causal' arrow containing the updated state.
stepCausal
  :: Causal (->) a b
  -> a
  -> (b, Causal (->) a b)
stepCausal (Causal s f) a =
  let (b, !s') = f (a, s)
  in (b, Causal s' f)
{-# INLINE stepCausal #-}

-- | Execute an effectful 'Causal' arrow in a monad on a single input, returning
-- the computed output and the next 'Causal' arrow containing the updated state.
stepCausalM
  :: (Monad m)
  => Causal (Kleisli m) a b
  -> a
  -> m (b, Causal (Kleisli m) a b)
stepCausalM (Causal s (Kleisli f)) a = do
  (b, !s') <- f (a, s)
  pure (b, Causal s' (Kleisli f))
{-# INLINE stepCausalM #-}

{-------------------------------------------------------------------------------
  Effectful IO Interpretation
-------------------------------------------------------------------------------}

-- | Interpret a 'FeedbackEff' strand into an effectful 'Kleisli' arrow in 'IO'
-- backed by a dedicated 'Data.IORef.IORef'.
--
-- The state ref is allocated once at initialization. Each invocation of the returned
-- arrow reads the current state from the ref, evaluates the transition strictly, writes
-- the new state back, and yields the output.
--
-- Completely circumvents 'Control.Monad.Fix.MonadFix' and prevents '<<loop>>' thread blackholing.
interpFeedbackIO
  :: (MonadIO m)
  => (r :-> Kleisli m)
  -> FeedbackEff r a b
  -> IO (Kleisli m a b)
interpFeedbackIO runInner (Feedback s0 stepR) = do
  ref <- liftIO (newIORef s0)
  let !step = runInner stepR
  pure $ Kleisli \a -> do
    s <- liftIO (readIORef ref)
    (b, !s') <- runKleisli step (a, s)
    liftIO (writeIORef ref s')
    pure b

-- | Initialize a 'Causal' arrow into a stateful 'Kleisli' arrow in 'IO'.
newCausalIO
  :: (MonadIO m)
  => Causal (Kleisli m) a b
  -> IO (Kleisli m a b)
newCausalIO (Causal s0 step) = do
  ref <- liftIO (newIORef s0)
  pure $ Kleisli \a -> do
    s <- liftIO (readIORef ref)
    (b, !s') <- runKleisli step (a, s)
    liftIO (writeIORef ref s')
    pure b
