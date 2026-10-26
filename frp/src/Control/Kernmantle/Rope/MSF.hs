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
Module      : Control.Kernmantle.Rope.MSF
Description : Dunai / Rhine Monadic Stream Function adapter for Kernmantle ropes
Copyright   : (c) 2026 The Kernmantle Authors
License     : BSD-3-Clause
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides a first-class Monadic Stream Function ('MSF') representation compatible with
Dunai and Rhine, high-performance compilation from 'Rope' pipelines, and continuous
time-domain signal processing combinators.

= Architectural Overview & Rationale
[ref:note-dunai-rhine-adapter]

In the 3-timescale separation of functional reactive programming:
1. __Pipeline Elaboration (Startup / Mode Change)__: 'Rope' provides static analysis
   (@SomeBinEff@), effect decoupling, and loose/tight weaving.
2. __Discrete Mode Switching (Epoch Transitions)__: Mode transitions occur when a
   discrete event fires, pre-compiling or swapping subgraphs via 'polySwitch'.
3. __Continuous Sample Stepping (1000 Hz Steady State)__: Between discrete events,
   the signal processing loop runs as an 'MSF m a b'—a raw coalgebra stepping at
   zero GADT traversal cost.

'Control.Kernmantle.Rope.MSF' acts as the bridge:
* Compiles entwined ropes ('compileRopeToMSF', 'compileCausalRopeToMSF') into high-frequency stream functions.
* Converts flat coalgebras ('coalgToMSF') and feedback strands ('feedbackToMSF', 'mealyToMSF') directly to 'MSF'.
* Integrates with 'ArrowSwitch' so that 'polySwitch' routes open variant events across 'MSF' mode branches in \(O(1)\).
* Provides Rhine/BearRiver time-domain primitives ('TimedMSF', 'integrate', 'derivative', 'lowPass').
-}
module Control.Kernmantle.Rope.MSF
  ( -- * Core Monadic Stream Function
    MSF(..)
  , arrM
  , arrConst
  , feedbackMSF

    -- * Execution & Stepping
  , stepMSF
  , stepPureMSF
  , runMSFStream
  , foldMSF
  , newMSFIO

    -- * Rope & Coalgebra Compilation Bridges
  , compileRopeToMSF
  , compileCausalRopeToMSF
  , compileMealyRopeToMSF
  , coalgToMSF
  , msfToCoalgebra
  , causalToMSF
  , feedbackToMSF
  , mealyToMSF
  , recordCoalgToMSF

    -- * Epoch-Staged Switching
  , epochSwitch
  , polySwitchMSF

    -- * Continuous Time-Domain & Rhine Primitives
  , TimedMSF
  , integrate
  , derivative
  , lowPass
  ) where

import Control.Arrow
import Control.Category
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Profunctor
import GHC.TypeLits (Symbol)

import Prelude hiding (id, (.))

import Polytropos
  ( Record
  , Variant
  )

import Control.Kernmantle.Rope.Internal
  ( Rope
  , LooseRope
  , TightRope
  , pattern RNil
  , runRope
  )
import Control.Kernmantle.Rope.Feedback
  ( FeedbackEff(..)
  , Causal(..)
  , toCausal
  )
import Control.Kernmantle.Rope.Mealy
  ( FlatCoalgebra(..)
  , FlatRecordCoalgebra(..)
  , MealyEff(..)
  , extractMealy
  , recordCoalgebraToFlat
  )
import Control.Kernmantle.Rope.Switch
  ( ArrowSwitch(..)
  , TemplateBranchesCore
  , VariantRouter(..)
  , makeTemplateRouter
  , routeVariant
  )

{-------------------------------------------------------------------------------
  Core Monadic Stream Function
-------------------------------------------------------------------------------}

-- | A monadic stream function representing a causal signal processor stepping in monad @m@.
--
-- Compatible with Dunai's @MSF@.
newtype MSF (m :: Type -> Type) a b = MSF
  { unMSF :: a -> m (b, MSF m a b) }

instance (Monad m) => Category (MSF m) where
  id = let msf = MSF \a -> pure (a, msf) in msf
  {-# INLINE id #-}

  MSF g . MSF f = MSF \a -> do
    (b, f') <- f a
    (c, g') <- g b
    pure (c, g' . f')
  {-# INLINE (.) #-}

instance (Monad m) => Arrow (MSF m) where
  arr fn = let msf = MSF \a -> pure (fn a, msf) in msf
  {-# INLINE arr #-}

  first (MSF f) = MSF \(b, d) -> do
    (c, f') <- f b
    pure ((c, d), first f')
  {-# INLINE first #-}

  second (MSF f) = MSF \(d, b) -> do
    (c, f') <- f b
    pure ((d, c), second f')
  {-# INLINE second #-}

  MSF f *** MSF g = MSF \(b, b') -> do
    (c, f') <- f b
    (c', g') <- g b'
    pure ((c, c'), f' *** g')
  {-# INLINE (***) #-}

instance (Monad m) => ArrowChoice (MSF m) where
  left (MSF f) = MSF \case
    Left b -> do
      (c, f') <- f b
      pure (Left c, left f')
    Right d ->
      pure (Right d, left (MSF f))
  {-# INLINE left #-}

  right (MSF f) = MSF \case
    Left d ->
      pure (Left d, right (MSF f))
    Right b -> do
      (c, f') <- f b
      pure (Right c, right f')
  {-# INLINE right #-}

  MSF f +++ MSF g = MSF \case
    Left b -> do
      (c, f') <- f b
      pure (Left c, f' +++ MSF g)
    Right b' -> do
      (c', g') <- g b'
      pure (Right c', MSF f +++ g')
  {-# INLINE (+++) #-}

instance (Monad m) => ArrowSwitch (MSF m) where
  coreSwitch (MSF sf) router = MSF \a -> do
    ((b, mVar), sf') <- sf a
    case mVar of
      Nothing  -> pure (b, coreSwitch sf' router)
      Just var -> do
        let targetMSF = routeVariant router var
        unMSF targetMSF a
  {-# INLINE coreSwitch #-}

instance (Monad m) => Profunctor (MSF m) where
  dimap l r (MSF f) = MSF \a -> do
    (b, f') <- f (l a)
    pure (r b, dimap l r f')
  {-# INLINE dimap #-}

instance (Monad m) => Strong (MSF m) where
  first' = first
  {-# INLINE first' #-}
  second' = second
  {-# INLINE second' #-}

instance (Monad m) => Choice (MSF m) where
  left' = left
  {-# INLINE left' #-}
  right' = right
  {-# INLINE right' #-}

instance (Monad m) => Functor (MSF m a) where
  fmap = rmap
  {-# INLINE fmap #-}

-- | Lift an effectful action @a -> m b@ into an 'MSF'.
arrM :: (Monad m) => (a -> m b) -> MSF m a b
arrM f = let msf = MSF \a -> do b <- f a; pure (b, msf) in msf
{-# INLINE arrM #-}

-- | Constant stream function emitting @b@ on every tick.
arrConst :: (Monad m) => b -> MSF m a b
arrConst b = let msf = MSF \_ -> pure (b, msf) in msf
{-# INLINE arrConst #-}

-- | Causal feedback loop passing state @s@ strictly from tick to tick.
--
-- Completely avoids 'Control.Arrow.ArrowLoop' and 'Control.Monad.Fix.MonadFix',
-- ensuring zero '<<loop>>' bottom blackholing.
feedbackMSF :: (Monad m) => s -> MSF m (a, s) (b, s) -> MSF m a b
feedbackMSF s0 (MSF f) = MSF \a -> do
  ((b, !s'), f') <- f (a, s0)
  pure (b, feedbackMSF s' f')
{-# INLINE feedbackMSF #-}

{-------------------------------------------------------------------------------
  Execution & Stepping
-------------------------------------------------------------------------------}

-- | Execute a single discrete step on an 'MSF', returning the output and the next 'MSF'.
stepMSF :: (Monad m) => MSF m a b -> a -> m (b, MSF m a b)
stepMSF (MSF f) a = f a
{-# INLINE stepMSF #-}

-- | Execute a single discrete step on a pure 'MSF' in 'Identity'.
stepPureMSF :: MSF Identity a b -> a -> (b, MSF Identity a b)
stepPureMSF (MSF f) a = runIdentity (f a)
{-# INLINE stepPureMSF #-}

-- | Execute an 'MSF' over an input list in strictly causal order.
runMSFStream :: (Monad m) => MSF m a b -> [a] -> m [b]
runMSFStream _ [] = pure []
runMSFStream (MSF f) (x:xs) = do
  (y, f') <- f x
  ys <- runMSFStream f' xs
  pure (y : ys)
{-# INLINE runMSFStream #-}

-- | Fold an 'MSF' over an input list, returning the last output (if any) and the next 'MSF'.
foldMSF :: (Monad m) => MSF m a b -> [a] -> m (Maybe b, MSF m a b)
foldMSF msf [] = pure (Nothing, msf)
foldMSF (MSF f) (x:xs) = do
  (y, f') <- f x
  loop y f' xs
  where
    loop lastY cur [] = pure (Just lastY, cur)
    loop _ (MSF step) (z:zs) = do
      (nextY, nextF) <- step z
      loop nextY nextF zs
{-# INLINE foldMSF #-}

-- | Interpret an 'MSF' in 'IO' into an effectful 'Kleisli' arrow backed by an 'IORef'.
newMSFIO :: (MonadIO m) => MSF m a b -> IO (Kleisli m a b)
newMSFIO msf0 = do
  ref <- liftIO (newIORef msf0)
  pure $ Kleisli \a -> do
    cur <- liftIO (readIORef ref)
    (b, next) <- stepMSF cur a
    liftIO (writeIORef ref next)
    pure b

{-------------------------------------------------------------------------------
  Rope & Coalgebra Compilation Bridges
-------------------------------------------------------------------------------}

-- | Compile an entwined, stateless 'TightRope' directly into a high-frequency 'MSF'.
compileRopeToMSF
  :: (Monad m)
  => TightRope '[] (Kleisli m) a b
  -> MSF m a b
compileRopeToMSF rope =
  let (Kleisli step) = runRope rope RNil
  in arrM step
{-# INLINE compileRopeToMSF #-}

-- | Compile an entwined causal 'TightRope' carrying state @s@ directly into an 'MSF'.
compileCausalRopeToMSF
  :: (Monad m)
  => s
  -> TightRope '[] (Kleisli m) (a, s) (b, s)
  -> MSF m a b
compileCausalRopeToMSF s0 stepRope =
  let (Kleisli step) = runRope stepRope RNil
      loop !s = MSF \a -> do
        (b, !s') <- step (a, s)
        pure (b, loop s')
  in loop s0
{-# INLINE compileCausalRopeToMSF #-}

-- | Compile a stateful transition arrow into an 'MSF'.
compileMealyRopeToMSF
  :: (Monad m)
  => s
  -> (s -> a -> m (b, s))
  -> MSF m a b
compileMealyRopeToMSF s0 step =
  let loop !s = MSF \a -> do
        (b, !s') <- step s a
        pure (b, loop s')
  in loop s0
{-# INLINE compileMealyRopeToMSF #-}

-- | Convert a compiled 'FlatCoalgebra' directly into an 'MSF'.
coalgToMSF :: (Monad m) => FlatCoalgebra m a b -> MSF m a b
coalgToMSF (FlatCoalgebra s0 step) =
  let loop !s = MSF \a -> do
        (b, !s') <- step s a
        pure (b, loop s')
  in loop s0
{-# INLINE coalgToMSF #-}

-- | Convert an 'MSF' into an existential 'FlatCoalgebra'.
msfToCoalgebra :: (Monad m) => MSF m a b -> FlatCoalgebra m a b
msfToCoalgebra msf0 = FlatCoalgebra msf0 \msf a -> do
  (b, msf') <- unMSF msf a
  pure (b, msf')
{-# INLINE msfToCoalgebra #-}

-- | Convert a 'Causal' arrow into an 'MSF'.
causalToMSF :: (Monad m) => Causal (Kleisli m) a b -> MSF m a b
causalToMSF (Causal s0 (Kleisli step)) =
  let loop !s = MSF \a -> do
        (b, !s') <- step (a, s)
        pure (b, loop s')
  in loop s0
{-# INLINE causalToMSF #-}

-- | Convert a 'FeedbackEff' strand into an 'MSF'.
feedbackToMSF :: (Monad m) => FeedbackEff (Kleisli m) a b -> MSF m a b
feedbackToMSF = causalToMSF . toCausal
{-# INLINE feedbackToMSF #-}

-- | Convert a 'MealyEff' strand into an 'MSF'.
mealyToMSF :: (Monad m) => MealyEff m a b -> MSF m a b
mealyToMSF = coalgToMSF . extractMealy
{-# INLINE mealyToMSF #-}

-- | Convert a Polytropos 'FlatRecordCoalgebra' into an 'MSF'.
recordCoalgToMSF :: (Monad m) => FlatRecordCoalgebra s_row m a b -> MSF m a b
recordCoalgToMSF = coalgToMSF . recordCoalgebraToFlat
{-# INLINE recordCoalgToMSF #-}

{-------------------------------------------------------------------------------
  Epoch-Staged Switching
-------------------------------------------------------------------------------}

-- | Dunai-style monadic stream function event switching.
--
-- When the first stream function produces 'Just ev', the transition continuation
-- is invoked with 'ev' to construct the stream function for subsequent ticks.
epochSwitch
  :: (Monad m)
  => MSF m a (b, Maybe ev)
  -> (ev -> MSF m a b)
  -> MSF m a b
epochSwitch (MSF sf) cont = MSF \a -> do
  ((b, mEv), sf') <- sf a
  case mEv of
    Nothing -> pure (b, epochSwitch sf' cont)
    Just ev -> do
      let target = cont ev
      pure (b, target)
{-# INLINE epochSwitch #-}

-- | Switch between 'MSF' stream templates indexed by Polytropos open variants.
--
-- Pre-compiles the template table into an \(O(1)\) unboxed jump table.
polySwitchMSF
  :: forall sw m a b.
     (Monad m)
  => MSF m a (b, Maybe (Variant sw))
  -> Record (TemplateBranchesCore (MSF m) a b sw)
  -> MSF m a b
polySwitchMSF initMSF templates =
  coreSwitch initMSF (makeTemplateRouter @sw templates)
{-# INLINE polySwitchMSF #-}

{-------------------------------------------------------------------------------
  Continuous Time-Domain & Rhine Primitives
-------------------------------------------------------------------------------}

-- | A signal function with explicit sample interval time deltas (@dt@).
--
-- Isomorphic to Rhine's @ClSF m (Millisecond dt) a b@ and BearRiver's @SF@.
type TimedMSF m dt a b = MSF m (a, dt) b

-- | Numerically integrate an incoming signal over time deltas using Euler integration:
--
-- \[
-- s_{t+1} = s_t + x \cdot dt
-- \]
integrate
  :: (Monad m, Fractional s)
  => s
  -- ^ Initial state accumulator
  -> TimedMSF m s s s
integrate s0 = MSF \(x, dt) ->
  let !s' = s0 + x * dt
  in pure (s', integrate s')
{-# INLINE integrate #-}

-- | Estimate numerical continuous derivative using backward finite differences:
--
-- \[
-- \frac{dx}{dt} \approx \frac{x_t - x_{t-1}}{dt}
-- \]
derivative
  :: (Monad m, Fractional s)
  => s
  -- ^ Initial value of \(x_0\)
  -> TimedMSF m s s s
derivative x0 = MSF \(x, dt) ->
  let !dx = (x - x0) / dt
  in pure (dx, derivative x)
{-# INLINE derivative #-}

-- | First-order exponential low-pass filter with time constant \(\tau\):
--
-- \[
-- \alpha = \frac{dt}{\tau + dt}, \quad y_{t} = y_{t-1} + \alpha (x_t - y_{t-1})
-- \]
lowPass
  :: (Monad m, Fractional s)
  => s
  -- ^ Filter time constant \(\tau > 0\)
  -> s
  -- ^ Initial filter state \(y_0\)
  -> TimedMSF m s s s
lowPass tau y0 = MSF \(x, dt) ->
  let !alpha = dt / (tau + dt)
      !y' = y0 + alpha * (x - y0)
  in pure (y', lowPass tau y')
{-# INLINE lowPass #-}
