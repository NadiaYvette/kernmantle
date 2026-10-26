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
Module      : Control.Kernmantle.Rope.Analysis
Description : Pre-flight static resource analysis with SomeBinEff & Polytropos template folding
Copyright   : (c) 2026 The Kernmantle Authors
License     : BSD-3-Clause
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides ahead-of-time, pre-flight static resource analysis for Kernmantle arrows
and Functional Reactive Programming (FRP) signal networks over 'Polytropos' open rows.

= Architectural Overview & Rationale
[ref:note-static-analysis-with-somebineff]

In traditional Arrowized Functional Reactive Programming (AFRP) systems (such as Yampa),
dynamic mode switching is parameterized by opaque Haskell functions:

@
switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
@

Because the continuation @c -> SF a b@ buries future topologies inside an uninspectable
closure waiting for a runtime event value @c@, ahead-of-time static analysis
(such as Kernmantle's @SomeBinEff f w m a b@) cannot traverse inside the switched branch
before execution begins. Hardware requirements, sensor bindings, and multi-rate clock
domains cannot be validated ahead of time.

'Control.Kernmantle.Rope.Analysis' resolves this dilemma by exploiting the duality between
'Polytropos' open variant sum rows and template product records:

1. __Concrete SmallArray# Template Storage__:
   Template branches @Record (TemplateBranchesCore core a b sw)@ are backed by a contiguous,
   unboxed 'Data.Primitive.SmallArray.SmallArray'.
2. __100% Whole-Graph Static Inspection__:
   Pre-flight static analyzers ('SomeBinEff' or 'AnalyzedArrow') fold their 'Applicative'
   functor @f@ across the template array in \(\mathcal{O}(N)\) time without executing or
   stepping the reactive loop.
3. __Pre-Flight Domain Verifiers__:
   * __Sensor / Actuator I/O Analysis__: Accumulates required physical hardware ports across
     all reachable modes and validates them against the physical motherboard topology.
   * __Rhine Multi-Rate Nyquist Verification__: Validates that multi-rate sampling rates
     satisfy the Nyquist-Shannon bound (\(f_s \ge 2 \cdot f_{max}\)) before reactive
     initialization.
   * __Resource Budget Constraints__: Enforces upper limits on memory footprint and active
     subsystems.
4. __Zero Runtime Penalty__:
   Once pre-flight validation succeeds, 'stripAnalysis' and 'verifyAndCompileSwitchMSF'
   compile the verified graph directly into a bare 'Control.Kernmantle.Rope.MSF.MSF'
   operating with zero analysis overhead at runtime.
-}
module Control.Kernmantle.Rope.Analysis
  ( -- * Decomposed Functorial Binary Effects (SomeBinEff)
    SomeBinEff(..)
  , PureSomeBinEff
  , liftSomeBinEff
  , mapSomeBinEffMeta
  , runSomeBinEffPure

    -- * Static Metadata Paired with Arrows (AnalyzedArrow)
  , AnalyzedArrow(..)
  , withAnalysis
  , getAnalysis
  , stripAnalysis
  , analyzedToMSF

    -- * Whole-Graph Switch Verification & Template Folding
  , foldTemplateAnalysis
  , foldSomeBinEffAnalysis
  , foldPairedSomeBinEffAnalysis
  , analyzeSwitchGraph
  , analyzeSomeBinEffGraph
  , makeAnalyzedTemplateRouter
  , stripTemplateAnalysis
  , verifyAndCompileSwitchMSF

    -- * Domain Verifier: Hardware I/O & Sensor Allocation
  , PortDirection(..)
  , DeviceType(..)
  , HardwarePort(..)
  , HardwareAnalysis(..)
  , requireHardware
  , verifyHardwareTopology

    -- * Domain Verifier: Rhine Multi-Rate Clock & Nyquist Bounds
  , PhysicalSignal(..)
  , ClockDomain(..)
  , ClockVerification(..)
  , declareClockDomain
  , verifyClockDomains

    -- * Domain Verifier: Resource Budget Checks
  , BudgetLimit(..)
  , BudgetReport(..)
  , verifyResourceBudget

    -- * Re-exports
  , ArrowSwitch(..)
  , Record
  , Variant
  , SumRow
  , Row
  ) where

import Control.Arrow
import Control.Category
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Primitive.SmallArray
  ( SmallArray
  , sizeofSmallArray
  , indexSmallArray
  , newSmallArray
  , writeSmallArray
  , runSmallArray
  )
import Data.Profunctor (Profunctor(..), Strong(..), Choice(..))
import qualified Data.Set as Set
import Data.Set (Set)
import Unsafe.Coerce (unsafeCoerce)

import Prelude hiding (id, (.))

import Control.Kernmantle.Rope.Internal (BinEff)
import Control.Kernmantle.Rope.MSF
  ( MSF(..)
  , runMSFStream
  )
import Control.Kernmantle.Rope.Switch
  ( ArrowSwitch(..)
  , TemplateBranchesCore
  , VariantRouter(..)
  , makeTemplateRouter
  )
import Polytropos
  ( Record
  , Variant
  )
import Polytropos.Core
  ( Rep(..)
  , RowRep(..)
  , repTag
  , repVals
  , SumRow
  , Row
  )

{-------------------------------------------------------------------------------
  Decomposed Functorial Binary Effects (SomeBinEff)
-------------------------------------------------------------------------------}

-- | Decomposed binary effect carrying static metadata @f@, input comonad @w@, and execution monad @m@.
--
-- Decomposes an arrow @a -> b@ into @f (w a -> m b)@:
-- * @f@: Applicative static analysis functor collecting metadata (hardware ports, clocks, dependencies)
--        before execution starts.
-- * @w@: Comonad for context/input extraction (defaults to 'Identity').
-- * @m@: Monad for runtime effect evaluation (e.g. 'IO', 'Identity').
newtype SomeBinEff (f :: Type -> Type) (w :: Type -> Type) (m :: Type -> Type) a b = SomeBinEff
  { unSomeBinEff :: f (w a -> m b) }

-- | Specialized alias when the input comonad is 'Identity'.
type PureSomeBinEff f m a b = SomeBinEff f Identity m a b

-- | Lift an effectful action into 'SomeBinEff' with trivial static metadata.
liftSomeBinEff
  :: (Applicative f)
  => (w a -> m b)
  -> SomeBinEff f w m a b
liftSomeBinEff step = SomeBinEff (pure step)
{-# INLINE liftSomeBinEff #-}

-- | Map over the static analysis functor of a 'SomeBinEff'.
mapSomeBinEffMeta
  :: (f (w a -> m b) -> f' (w' a -> m' b))
  -> SomeBinEff f w m a b
  -> SomeBinEff f' w' m' a b
mapSomeBinEffMeta g (SomeBinEff act) = SomeBinEff (g act)
{-# INLINE mapSomeBinEffMeta #-}

-- | Execute a pure 'SomeBinEff' when @f@ and @m@ are 'Identity'.
runSomeBinEffPure :: SomeBinEff Identity Identity Identity a b -> a -> b
runSomeBinEffPure (SomeBinEff (Identity step)) a =
  let Identity b = step (Identity a)
  in b
{-# INLINE runSomeBinEffPure #-}

instance (Applicative f, Monad m) => Category (SomeBinEff f Identity m) where
  id = SomeBinEff $ pure (\(Identity a) -> pure a)
  {-# INLINE id #-}

  SomeBinEff g . SomeBinEff f = SomeBinEff $
    (\fStep gStep (Identity a) -> do
        b <- fStep (Identity a)
        gStep (Identity b))
    <$> f <*> g
  {-# INLINE (.) #-}

instance (Applicative f, Monad m) => Arrow (SomeBinEff f Identity m) where
  arr fn = SomeBinEff $ pure (\(Identity a) -> pure (fn a))
  {-# INLINE arr #-}

  first (SomeBinEff f) = SomeBinEff $
    (\fStep (Identity (b, d)) -> do
        c <- fStep (Identity b)
        pure (c, d))
    <$> f
  {-# INLINE first #-}

  second (SomeBinEff f) = SomeBinEff $
    (\fStep (Identity (d, b)) -> do
        c <- fStep (Identity b)
        pure (d, c))
    <$> f
  {-# INLINE second #-}

  SomeBinEff f *** SomeBinEff g = SomeBinEff $
    (\fStep gStep (Identity (b, d)) -> do
        c <- fStep (Identity b)
        c' <- gStep (Identity d)
        pure (c, c'))
    <$> f <*> g
  {-# INLINE (***) #-}

instance (Applicative f, Monad m) => ArrowChoice (SomeBinEff f Identity m) where
  left (SomeBinEff f) = SomeBinEff $
    (\fStep (Identity inp) -> case inp of
        Left b  -> Left <$> fStep (Identity b)
        Right d -> pure (Right d))
    <$> f
  {-# INLINE left #-}

  right (SomeBinEff f) = SomeBinEff $
    (\fStep (Identity inp) -> case inp of
        Left d  -> pure (Left d)
        Right b -> Right <$> fStep (Identity b))
    <$> f
  {-# INLINE right #-}

  SomeBinEff f +++ SomeBinEff g = SomeBinEff $
    (\fStep gStep (Identity inp) -> case inp of
        Left b  -> Left <$> fStep (Identity b)
        Right d -> Right <$> gStep (Identity d))
    <$> f <*> g
  {-# INLINE (+++) #-}

instance (Applicative f, Monad m) => Profunctor (SomeBinEff f Identity m) where
  dimap l r (SomeBinEff f) = SomeBinEff $
    (\step (Identity a) -> r <$> step (Identity (l a)))
    <$> f
  {-# INLINE dimap #-}

instance (Applicative f, Monad m) => Strong (SomeBinEff f Identity m) where
  first' = first
  {-# INLINE first' #-}
  second' = second
  {-# INLINE second' #-}

instance (Applicative f, Monad m) => Choice (SomeBinEff f Identity m) where
  left' = left
  {-# INLINE left' #-}
  right' = right
  {-# INLINE right' #-}

instance (Applicative f, Monad m) => ArrowSwitch (SomeBinEff f Identity m) where
  coreSwitch (SomeBinEff initF) (VariantRouter _table) = SomeBinEff $
    (\initStep (Identity a) -> do
        (b, mVar) <- initStep (Identity a)
        case mVar of
          Nothing -> pure b
          Just (RowRep vRep) -> do
            let !tag = fromIntegral (repTag vRep) :: Int
            error ("ArrowSwitch (SomeBinEff): dynamic mode switch to tag " ++ show tag ++ " requires runtime compilation"))
    <$> initF
  {-# INLINE coreSwitch #-}

{-------------------------------------------------------------------------------
  Static Metadata Paired with Core Arrows (AnalyzedArrow)
-------------------------------------------------------------------------------}

-- | A binary effect arrow paired with static monoidal analysis metadata @res@.
--
-- Retains static metadata @res@ for pre-flight verification while delegating
-- runtime execution to @core a b@.
data AnalyzedArrow res (core :: BinEff) a b = AnalyzedArrow
  { arrowAnalysis :: !res
  , arrowCore     :: !(core a b)
  }

-- | Annotate an arrow with static analysis metadata.
withAnalysis :: res -> core a b -> AnalyzedArrow res core a b
withAnalysis = AnalyzedArrow
{-# INLINE withAnalysis #-}

-- | Extract the static analysis metadata from an 'AnalyzedArrow'.
getAnalysis :: AnalyzedArrow res core a b -> res
getAnalysis = arrowAnalysis
{-# INLINE getAnalysis #-}

-- | Strip static analysis metadata, returning the bare core arrow.
stripAnalysis :: AnalyzedArrow res core a b -> core a b
stripAnalysis = arrowCore
{-# INLINE stripAnalysis #-}

-- | Specialized conversion of an 'AnalyzedArrow' backed by 'MSF' to 'MSF'.
analyzedToMSF :: AnalyzedArrow res (MSF m) a b -> MSF m a b
analyzedToMSF = arrowCore
{-# INLINE analyzedToMSF #-}

instance (Monoid res, Category core) => Category (AnalyzedArrow res core) where
  id = AnalyzedArrow mempty id
  {-# INLINE id #-}

  AnalyzedArrow r2 g . AnalyzedArrow r1 f = AnalyzedArrow (r1 <> r2) (g . f)
  {-# INLINE (.) #-}

instance (Monoid res, Arrow core) => Arrow (AnalyzedArrow res core) where
  arr fn = AnalyzedArrow mempty (arr fn)
  {-# INLINE arr #-}

  first (AnalyzedArrow r f) = AnalyzedArrow r (first f)
  {-# INLINE first #-}

  second (AnalyzedArrow r f) = AnalyzedArrow r (second f)
  {-# INLINE second #-}

  AnalyzedArrow r1 f *** AnalyzedArrow r2 g = AnalyzedArrow (r1 <> r2) (f *** g)
  {-# INLINE (***) #-}

instance (Monoid res, ArrowChoice core) => ArrowChoice (AnalyzedArrow res core) where
  left (AnalyzedArrow r f) = AnalyzedArrow r (left f)
  {-# INLINE left #-}

  right (AnalyzedArrow r f) = AnalyzedArrow r (right f)
  {-# INLINE right #-}

  AnalyzedArrow r1 f +++ AnalyzedArrow r2 g = AnalyzedArrow (r1 <> r2) (f +++ g)
  {-# INLINE (+++) #-}

instance (Monoid res, Profunctor core) => Profunctor (AnalyzedArrow res core) where
  dimap l r (AnalyzedArrow meta f) = AnalyzedArrow meta (dimap l r f)
  {-# INLINE dimap #-}

instance (Monoid res, Strong core) => Strong (AnalyzedArrow res core) where
  first' (AnalyzedArrow r f) = AnalyzedArrow r (first' f)
  {-# INLINE first' #-}
  second' (AnalyzedArrow r f) = AnalyzedArrow r (second' f)
  {-# INLINE second' #-}

instance (Monoid res, Choice core) => Choice (AnalyzedArrow res core) where
  left' (AnalyzedArrow r f) = AnalyzedArrow r (left' f)
  {-# INLINE left' #-}
  right' (AnalyzedArrow r f) = AnalyzedArrow r (right' f)
  {-# INLINE right' #-}

instance (Monoid res, ArrowSwitch core) => ArrowSwitch (AnalyzedArrow res core) where
  coreSwitch (AnalyzedArrow metaInit initCore) (VariantRouter table) =
    let !n = sizeofSmallArray table
        table' = runSmallArray $ do
          m <- newSmallArray n (error "coreSwitch AnalyzedArrow: uninitialized slot")
          let loop !i
                | i >= n    = pure ()
                | otherwise = do
                    let !h = indexSmallArray table i
                    writeSmallArray m i (\rep -> case h rep of AnalyzedArrow _ c -> c)
                    loop (i + 1)
          loop 0
          pure m
        !swappedCore = coreSwitch initCore (VariantRouter table')
    in AnalyzedArrow metaInit swappedCore
  {-# INLINE coreSwitch #-}

{-------------------------------------------------------------------------------
  Whole-Graph Switch Verification & Template Folding
-------------------------------------------------------------------------------}

-- | Fold static analysis metadata across all template branches in any record.
--
-- Evaluates the static metadata of every mode branch reachable via dynamic
-- variant switching in \(\mathcal{O}(N)\) array lookups with zero arrow execution.
foldTemplateAnalysis
  :: forall res branches.
     (Monoid res)
  => Record branches
  -> res
foldTemplateAnalysis (RowRep rep) =
  let tVals = repVals rep
      !n = sizeofSmallArray tVals
      loop !i !acc
        | i >= n    = acc
        | otherwise =
            let Identity templateAny = indexSmallArray tVals i
                AnalyzedArrow !r _ = unsafeCoerce templateAny :: AnalyzedArrow res (->) () ()
            in loop (i + 1) (acc <> r)
  in loop 0 mempty
{-# INLINE foldTemplateAnalysis #-}

-- | Fold static analysis across template branches for 'SomeBinEff' with 'Const'.
foldSomeBinEffAnalysis
  :: forall res branches.
     (Monoid res)
  => Record branches
  -> res
foldSomeBinEffAnalysis (RowRep rep) =
  let tVals = repVals rep
      !n = sizeofSmallArray tVals
      loop !i !acc
        | i >= n    = acc
        | otherwise =
            let Identity templateAny = indexSmallArray tVals i
                SomeBinEff (Const !r) = unsafeCoerce templateAny :: SomeBinEff (Const res) Identity Identity () ()
            in loop (i + 1) (acc <> r)
  in loop 0 mempty
{-# INLINE foldSomeBinEffAnalysis #-}

-- | Fold static analysis across template branches for 'SomeBinEff' with @(,) res@.
foldPairedSomeBinEffAnalysis
  :: forall res branches.
     (Monoid res)
  => Record branches
  -> res
foldPairedSomeBinEffAnalysis (RowRep rep) =
  let tVals = repVals rep
      !n = sizeofSmallArray tVals
      loop !i !acc
        | i >= n    = acc
        | otherwise =
            let Identity templateAny = indexSmallArray tVals i
                SomeBinEff (!r, _) = unsafeCoerce templateAny :: SomeBinEff ((,) res) Identity Identity () ()
            in loop (i + 1) (acc <> r)
  in loop 0 mempty
{-# INLINE foldPairedSomeBinEffAnalysis #-}

-- | Pre-flight whole-graph static analysis across a switch network with 'AnalyzedArrow'.
--
-- Inspects both the initial arrow and 100% of reachable template branches in the
-- switch table, returning the combined static metadata.
analyzeSwitchGraph
  :: forall sw res core a b.
     (Monoid res)
  => AnalyzedArrow res core a (b, Maybe (Variant sw))
  -> Record (TemplateBranchesCore (AnalyzedArrow res core) a b sw)
  -> res
analyzeSwitchGraph (AnalyzedArrow resInit _) templates =
  let !resBranches = foldTemplateAnalysis templates
  in resInit <> resBranches
{-# INLINE analyzeSwitchGraph #-}

-- | Pre-flight whole-graph static analysis across a switch network with 'SomeBinEff'.
analyzeSomeBinEffGraph
  :: forall sw res m a b.
     (Monoid res)
  => SomeBinEff (Const res) Identity m a (b, Maybe (Variant sw))
  -> Record (TemplateBranchesCore (SomeBinEff (Const res) Identity m) a b sw)
  -> res
analyzeSomeBinEffGraph (SomeBinEff (Const resInit)) templates =
  resInit <> foldSomeBinEffAnalysis templates
{-# INLINE analyzeSomeBinEffGraph #-}

-- | Construct an \(O(1)\) jump table 'VariantRouter' directly from a record of 'AnalyzedArrow' templates.
--
-- Strips the static analysis metadata and compiles each template arrow into a runtime handler
-- in a single array allocation pass.
makeAnalyzedTemplateRouter
  :: forall sw core a b branches.
     (Arrow core)
  => Record branches
  -> VariantRouter core a b
makeAnalyzedTemplateRouter (RowRep rep) =
  let tVals = repVals rep
      !n = sizeofSmallArray tVals
      handlers = runSmallArray $ do
        m <- newSmallArray n (error "makeAnalyzedTemplateRouter: uninitialized slot")
        let loop !i
              | i >= n    = pure ()
              | otherwise = do
                  let Identity templateAny = indexSmallArray tVals i
                      AnalyzedArrow _ !coreTemplate = unsafeCoerce templateAny :: AnalyzedArrow () core (Record '[], a) b
                      !handler = \vRep ->
                        let !payload = RowRep (Rep 0 (repVals vRep)) :: Record '[]
                        in arr (\x -> (payload, x)) >>> coreTemplate
                  writeSmallArray m i handler
                  loop (i + 1)
        loop 0
        pure m
  in VariantRouter handlers
{-# INLINE makeAnalyzedTemplateRouter #-}

-- | Strip static analysis metadata from all template branches in a record.
stripTemplateAnalysis
  :: forall branches strippedBranches.
     Record branches
  -> Record strippedBranches
stripTemplateAnalysis (RowRep rep) =
  let tVals = repVals rep
      !n = sizeofSmallArray tVals
      stripped = runSmallArray $ do
        m <- newSmallArray n (error "stripTemplateAnalysis: uninitialized slot")
        let loop !i
              | i >= n    = pure ()
              | otherwise = do
                  let Identity templateAny = indexSmallArray tVals i
                      AnalyzedArrow _ !c = unsafeCoerce templateAny :: AnalyzedArrow () (->) () ()
                  writeSmallArray m i (Identity (unsafeCoerce c))
                  loop (i + 1)
        loop 0
        pure m
  in RowRep (Rep 0 stripped)
{-# INLINE stripTemplateAnalysis #-}

-- | Perform pre-flight static verification on a switch network, then strip analysis
-- and compile to an executable 'MSF'.
--
-- If pre-flight verification fails, returns @Left errors@ before reactive initialization.
-- If pre-flight verification succeeds, compiles the templates into an \(\mathcal{O}(1)\)
-- unboxed jump table and returns @Right (MSF m a b)@ with zero analysis overhead.
verifyAndCompileSwitchMSF
  :: forall sw res m a b.
     (Monad m, Monoid res)
  => (res -> (Bool, [String])) -- ^ Pre-flight validation gate
  -> AnalyzedArrow res (MSF m) a (b, Maybe (Variant sw))
  -> Record (TemplateBranchesCore (AnalyzedArrow res (MSF m)) a b sw)
  -> Either [String] (MSF m a b)
verifyAndCompileSwitchMSF validator initAnalyzed templates =
  let !totalMeta = analyzeSwitchGraph @sw initAnalyzed templates
      (passed, errors) = validator totalMeta
  in if passed
       then
         let !initMSF = stripAnalysis initAnalyzed
             !routerMSF = makeAnalyzedTemplateRouter @sw templates
         in Right (coreSwitch initMSF routerMSF)
       else Left errors
{-# INLINE verifyAndCompileSwitchMSF #-}

{-------------------------------------------------------------------------------
  Domain Verifier: Hardware I/O & Sensor Allocation
-------------------------------------------------------------------------------}

-- | Direction of an I/O hardware port.
data PortDirection = PortInput | PortOutput | PortBidirectional
  deriving (Eq, Ord, Show)

-- | Physical device type connected to a hardware port.
data DeviceType = SensorDevice | ActuatorDevice | CommunicationBus
  deriving (Eq, Ord, Show)

-- | Physical hardware port descriptor.
data HardwarePort = HardwarePort
  { portName      :: !String
  , portDirection :: !PortDirection
  , portDevice    :: !DeviceType
  , portRateHz    :: !Double
  } deriving (Eq, Ord, Show)

-- | Static hardware resource analysis report.
data HardwareAnalysis = HardwareAnalysis
  { allocatedPorts  :: !(Set HardwarePort)
  , maxStateBytes   :: !Int
  , totalSubsystems :: !Int
  } deriving (Eq, Show)

instance Semigroup HardwareAnalysis where
  HardwareAnalysis p1 m1 s1 <> HardwareAnalysis p2 m2 s2 =
    HardwareAnalysis (p1 <> p2) (max m1 m2) (s1 + s2)

instance Monoid HardwareAnalysis where
  mempty = HardwareAnalysis Set.empty 0 0

-- | Declare a hardware port requirement and state footprint on an arrow.
requireHardware
  :: (Arrow core)
  => HardwarePort
  -> Int -- ^ State footprint in bytes
  -> AnalyzedArrow HardwareAnalysis core a a
requireHardware port memBytes =
  AnalyzedArrow (HardwareAnalysis (Set.singleton port) memBytes 1) id
{-# INLINE requireHardware #-}

-- | Pre-flight verification: check whether all required ports across the graph
-- are present in the physical motherboard specification.
verifyHardwareTopology
  :: Set HardwarePort -- ^ Available motherboard hardware ports
  -> HardwareAnalysis -- ^ Accumulated graph requirements across all modes
  -> (Bool, [String]) -- ^ (Success, Missing ports / diagnostics)
verifyHardwareTopology available (HardwareAnalysis required _ _) =
  let missing = Set.difference required available
  in if Set.null missing
       then (True, [])
       else (False, ["Hardware verification failed: Missing required hardware ports: " ++ show (Set.toList missing)])

{-------------------------------------------------------------------------------
  Domain Verifier: Rhine Multi-Rate Clock & Nyquist Bounds
-------------------------------------------------------------------------------}

-- | A physical signal with upper frequency bandwidth limit \(f_{max}\).
data PhysicalSignal = PhysicalSignal
  { signalIdentifier :: !String
  , maxFrequencyHz   :: !Double -- ^ Upper frequency bound (f_max)
  } deriving (Eq, Show)

-- | A multi-rate clock domain running at sample frequency \(f_s\).
data ClockDomain = ClockDomain
  { clockDomainName   :: !String
  , sampleFrequencyHz :: !Double -- ^ Sampling rate (f_s)
  , monitoredSignal   :: !(Maybe PhysicalSignal)
  } deriving (Eq, Show)

-- | Clock analysis report capturing declared multi-rate domains and Nyquist violations.
data ClockVerification = ClockVerification
  { verifiedDomains   :: ![ClockDomain]
  , nyquistViolations :: ![String]
  } deriving (Eq, Show)

instance Semigroup ClockVerification where
  ClockVerification d1 v1 <> ClockVerification d2 v2 =
    ClockVerification (d1 <> d2) (v1 <> v2)

instance Monoid ClockVerification where
  mempty = ClockVerification [] []

-- | Declare a multi-rate clock domain with Nyquist-Shannon bound checking.
--
-- Nyquist-Shannon Theorem: to reconstruct a continuous physical signal with bandwidth
-- \(f_{max}\), the sampling rate \(f_s\) must satisfy \(f_s \ge 2 \cdot f_{max}\).
declareClockDomain
  :: (Arrow core)
  => String                -- ^ Clock domain identifier
  -> Double                -- ^ Sample frequency in Hz (f_s)
  -> Maybe (String, Double) -- ^ Monitored physical signal (name, f_max)
  -> AnalyzedArrow ClockVerification core a a
declareClockDomain name rate mSig =
  let (sig, viols) = case mSig of
        Nothing -> (Nothing, [])
        Just (sName, fMax) ->
          let s = PhysicalSignal sName fMax
              v = if rate < 2.0 * fMax
                    then [ "Nyquist violation in clock domain '" ++ name
                        ++ "': sample frequency " ++ show rate ++ " Hz is below 2 * f_max ("
                        ++ show (2.0 * fMax) ++ " Hz) for signal '" ++ sName ++ "'!" ]
                    else []
          in (Just s, v)
  in AnalyzedArrow (ClockVerification [ClockDomain name rate sig] viols) id
{-# INLINE declareClockDomain #-}

-- | Verify that all clock domains across the whole graph satisfy Nyquist bounds.
verifyClockDomains :: ClockVerification -> (Bool, [String])
verifyClockDomains (ClockVerification _ viols) =
  if null viols
    then (True, [])
    else (False, viols)

{-------------------------------------------------------------------------------
  Domain Verifier: Resource Budget Checks
-------------------------------------------------------------------------------}

-- | Declared hardware budget limit.
data BudgetLimit = BudgetLimit
  { maxAllowedMemoryBytes :: !Int
  , maxAllowedPorts       :: !Int
  } deriving (Eq, Show)

-- | Resource budget verification report.
data BudgetReport = BudgetReport
  { budgetPassed    :: !Bool
  , memoryActual    :: !Int
  , memoryLimit     :: !Int
  , portCountActual :: !Int
  , portCountLimit  :: !Int
  , budgetWarnings  :: ![String]
  } deriving (Eq, Show)

-- | Verify that a whole-graph hardware analysis satisfies declared budget limits.
verifyResourceBudget :: BudgetLimit -> HardwareAnalysis -> BudgetReport
verifyResourceBudget (BudgetLimit maxMem maxPorts) (HardwareAnalysis ports mem _) =
  let portCount = Set.size ports
      memExceeded = mem > maxMem
      portsExceeded = portCount > maxPorts
      passed = not memExceeded && not portsExceeded
      warnings = [ "Memory budget exceeded: required " ++ show mem ++ " bytes > limit " ++ show maxMem | memExceeded ]
              ++ [ "Port budget exceeded: required " ++ show portCount ++ " ports > limit " ++ show maxPorts | portsExceeded ]
  in BudgetReport passed mem maxMem portCount maxPorts warnings
