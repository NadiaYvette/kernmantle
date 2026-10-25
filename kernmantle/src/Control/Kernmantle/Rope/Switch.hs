{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
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
Module      : Control.Kernmantle.Rope.Switch
Description : Parameterized open variant dynamic switching for Kernmantle ropes over Polytropos
Copyright   : (c) 2026 The Kernmantle Authors
License     : BSD-3-Clause
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides constant-time \(O(1)\) dynamic mode switching and multi-branch choice
for 'Rope' arrow computations over 'Polytropos.Variant' open sums with parameterized
templates.

= Architectural Overview & Rationale
[ref:note-parameterized-variant-switching]

Traditional Arrowized Functional Reactive Programming (AFRP) systems (such as Yampa)
rely on dynamic switching combinators parameterized by opaque Haskell functions:

@
switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
@

Because the continuation @(c -> SF a b)@ buries future topologies inside an uninspectable
closure awaiting a runtime event value @c@, ahead-of-time whole-graph static analysis
(such as Kernmantle's @SomeBinEff f w m a b@) cannot traverse inside the switched branch
before execution begins.

'Control.Kernmantle.Rope.Switch' solves this dilemma by establishing a **first-class row duality**
between 'Polytropos' open variants and open records:

1. __The Switching Event is an Open Variant__:
   A running arrow emits @(b, Maybe (Variant sw))@, where each constructor @lbl@ carries that
   mode's initialization payload in a typed record @Record fs@.
2. __The Switch Table is a Dual Open Record of Parameterized Templates__:
   The switch table is a 'Polytropos.Record' typed by 'TemplateBranches', where each field @#lbl@
   holds a parameterized arrow:

   @
   Rope record mantle core (Record fs, a) b
   @

3. __100% Compile-Time Static Analysis__:
   Because each branch is an explicit arrow value rather than a closure, pre-flight analyzers
   (@SomeBinEff@) can inspect 100% of reachable modes ahead of time by folding across the
   underlying 'Data.Primitive.SmallArray.SmallArray'.
4. __\(O(1)\) Jump Table Dispatch__:
   During pipeline initialization / entwining, each template arrow in the record is pre-compiled
   into an \(O(1)\) unboxed array jump table ('VariantRouter') indexed by the constructor tags
   of the sum row. When a mode switch occurs, branch dispatch is a single 'Data.Primitive.SmallArray.indexSmallArray'
   operation passing the payload record into the target's input channel.
-}
module Control.Kernmantle.Rope.Switch
  ( -- * Parameterized Template Branch Rows
    TemplateBranches
  , TightTemplateBranches
  , TemplateBranchesCore

    -- * Dynamic Mode Switching Combinators
  , polySwitch
  , polySwitchWith
  , polyBranch
  , templateSwitch
  , templateBranch
  , switchCore

    -- * O(1) Template Router Construction
  , makeTemplateRouter
  , makeRopeTemplateRouter

    -- * Core Arrow Switching Abstraction
  , ArrowSwitch(..)

    -- * Re-exports from Variant & Polytropos
  , VariantRouter(..)
  , routeVariant
  , Variant
  , Record
  , SumRow
  , Row
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
  , runSmallArray
  )
import Data.Profunctor.Cayley (Cayley(..))
import GHC.TypeLits (Symbol)
import Unsafe.Coerce (unsafeCoerce)

import Prelude hiding (id, (.))

import Control.Kernmantle.Arrow (AutoIdent(..))
import Control.Kernmantle.Rope.Internal
  ( Rope
  , TightRope
  , RopeRec
  , Weaver
  , mkRope
  , runRope
  , Strand
  , BinEff
  , Rec
  )
import Control.Kernmantle.Rope.Variant
  ( VariantRouter(..)
  , routeVariant
  )
import Polytropos
  ( Variant
  , Record
  )
import Polytropos.Core
  ( Rep(..)
  , RowRep(..)
  , repVals
  , SumRow
  , Row
  , Pair(..)
  )

{-------------------------------------------------------------------------------
  Parameterized Template Branch Rows
-------------------------------------------------------------------------------}

-- | Type family mapping a sum row 'sw' describing mode constructors and their parameter rows
-- into a product row of parameterized 'Rope' arrow templates.
--
-- For each constructor @(lbl ':= fs)@ in the sum row @sw@, the template record
-- carries a field @lbl@ of type @Rope record mantle core (Record fs, a) b@.
--
-- Mode parameters @Record fs@ are supplied through the arrow's input profunctor channel
-- @(Record fs, a)@ rather than an opaque closure @Record fs -> Rope ...@, ensuring full
-- static inspectability.
type family TemplateBranches (record :: RopeRec) (mantle :: [Strand]) (core :: BinEff) (a :: Type) (b :: Type) (sw :: SumRow) :: Row where
  TemplateBranches record mantle core a b '[] = '[]
  TemplateBranches record mantle core a b ((lbl ':= fs) ': rest) =
    (lbl ':= Rope record mantle core (Record fs, a) b) ': TemplateBranches record mantle core a b rest

-- | Specialization of 'TemplateBranches' for 'TightRope' (backed by Polytropos 'Rec').
type TightTemplateBranches mantle core a b sw = TemplateBranches Rec mantle core a b sw

-- | Type family mapping a sum row 'sw' to a product row of parameterized core arrow templates.
type family TemplateBranchesCore (core :: BinEff) (a :: Type) (b :: Type) (sw :: SumRow) :: Row where
  TemplateBranchesCore core a b '[] = '[]
  TemplateBranchesCore core a b ((lbl ':= fs) ': rest) =
    (lbl ':= core (Record fs, a) b) ': TemplateBranchesCore core a b rest

{-------------------------------------------------------------------------------
  Core Arrow Switching Abstraction
-------------------------------------------------------------------------------}

-- | Class of binary effect arrows that support constant-time \(O(1)\) dynamic switching
-- over Polytropos 'VariantRouter' jump tables.
class (Arrow core) => ArrowSwitch (core :: BinEff) where
  -- | Switch arrow behavior when the initial arrow emits a 'Variant' event.
  --
  -- When the initial arrow produces @(b, Nothing)@, the computation proceeds
  -- emitting @b@.
  -- When the initial arrow produces @(b, Just (Variant sw))@, the runtime looks up
  -- the target template in the router's \(O(1)\) jump table and executes it.
  coreSwitch
    :: core a (b, Maybe (Variant sw))
    -> VariantRouter core a b
    -> core a b

instance ArrowSwitch (->) where
  coreSwitch initFn router a =
    case initFn a of
      (b, Nothing)  -> b
      (_, Just var) -> routeVariant router var a
  {-# INLINE coreSwitch #-}

instance (Monad m) => ArrowSwitch (Kleisli m) where
  coreSwitch (Kleisli initFn) router = Kleisli \a -> do
    (b, mVar) <- initFn a
    case mVar of
      Nothing  -> pure b
      Just var -> runKleisli (routeVariant router var) a
  {-# INLINE coreSwitch #-}

instance (ArrowSwitch core) => ArrowSwitch (AutoIdent core) where
  coreSwitch (AutoIdent initFn) (VariantRouter table) = AutoIdent \aid ->
    let !n = sizeofSmallArray table
        table' = runSmallArray $ do
          m <- newSmallArray n (error "coreSwitch AutoIdent: uninitialized router slot")
          let loop !i
                | i >= n    = pure ()
                | otherwise = do
                    let !h = indexSmallArray table i
                    writeSmallArray m i (\rep -> case h rep of AutoIdent f -> f aid)
                    loop (i + 1)
          loop 0
          pure m
    in coreSwitch (initFn aid) (VariantRouter table')
  {-# INLINE coreSwitch #-}

instance (ArrowSwitch core) => ArrowSwitch (Cayley ((->) env) core) where
  coreSwitch (Cayley initFn) (VariantRouter table) = Cayley \env ->
    let !n = sizeofSmallArray table
        table' = runSmallArray $ do
          m <- newSmallArray n (error "coreSwitch Cayley: uninitialized router slot")
          let loop !i
                | i >= n    = pure ()
                | otherwise = do
                    let !h = indexSmallArray table i
                    writeSmallArray m i (\rep -> case h rep of Cayley f -> f env)
                    loop (i + 1)
          loop 0
          pure m
    in coreSwitch (initFn env) (VariantRouter table')
  {-# INLINE coreSwitch #-}

instance (ArrowSwitch core) => ArrowSwitch (Rope record mantle core) where
  coreSwitch initRope (VariantRouter table) = mkRope $ \weavers ->
    let !initCore = runRope initRope weavers
        !n = sizeofSmallArray table
        table' = runSmallArray $ do
          m <- newSmallArray n (error "coreSwitch Rope: uninitialized router slot")
          let loop !i
                | i >= n    = pure ()
                | otherwise = do
                    let !h = indexSmallArray table i
                    writeSmallArray m i (\rep -> runRope (h rep) weavers)
                    loop (i + 1)
          loop 0
          pure m
    in coreSwitch initCore (VariantRouter table')
  {-# INLINE coreSwitch #-}

{-------------------------------------------------------------------------------
  O(1) Template Router Construction
-------------------------------------------------------------------------------}

-- | Construct an \(O(1)\) jump table 'VariantRouter' from a record of parameterized core arrow templates.
--
-- Each template in the record is converted to a handler taking the unboxed variant representation
-- 'Rep Identity ()', which reconstructs the typed parameter record @Record fs@ and feeds @(payload, a)@
-- into the corresponding core arrow template via arrow precomposition.
makeTemplateRouter
  :: forall sw core a b.
     (Arrow core)
  => Record (TemplateBranchesCore core a b sw)
  -> VariantRouter core a b
makeTemplateRouter (RowRep rep) =
  let tVals = repVals rep
      !n = sizeofSmallArray tVals
      handlers = runSmallArray $ do
        m <- newSmallArray n (error "makeTemplateRouter: uninitialized slot")
        let loop !i
              | i >= n    = pure ()
              | otherwise = do
                  let Identity templateAny = indexSmallArray tVals i
                      !coreTemplate = unsafeCoerce templateAny :: core (Record '[], a) b
                      !handler = \vRep ->
                        let !payload = RowRep (Rep 0 (repVals vRep)) :: Record '[]
                        in arr (\x -> (payload, x)) >>> coreTemplate
                  writeSmallArray m i handler
                  loop (i + 1)
        loop 0
        pure m
  in VariantRouter handlers
{-# INLINE makeTemplateRouter #-}

-- | Construct an \(O(1)\) jump table 'VariantRouter' from a record of parameterized 'Rope' templates
-- and an entwined weaver bundle.
--
-- Evaluates 'runRope' across all templates in the record during initialization, compiling them
-- into a flat array of 'core a b' launchers.
makeRopeTemplateRouter
  :: forall sw record mantle core a b.
     (Arrow core)
  => Record (TemplateBranches record mantle core a b sw)
  -> record (Weaver core) mantle
  -> VariantRouter core a b
makeRopeTemplateRouter (RowRep rep) weavers =
  let tVals = repVals rep
      !n = sizeofSmallArray tVals
      handlers = runSmallArray $ do
        m <- newSmallArray n (error "makeRopeTemplateRouter: uninitialized slot")
        let loop !i
              | i >= n    = pure ()
              | otherwise = do
                  let Identity templateAny = indexSmallArray tVals i
                      !ropeTemplate = unsafeCoerce templateAny :: Rope record mantle core (Record '[], a) b
                      !coreTemplate = runRope ropeTemplate weavers
                      !handler = \vRep ->
                        let !payload = RowRep (Rep 0 (repVals vRep)) :: Record '[]
                        in arr (\x -> (payload, x)) >>> coreTemplate
                  writeSmallArray m i handler
                  loop (i + 1)
        loop 0
        pure m
  in VariantRouter handlers
{-# INLINE makeRopeTemplateRouter #-}

{-------------------------------------------------------------------------------
  Dynamic Mode Switching Combinators
-------------------------------------------------------------------------------}

-- | Dynamic mode switching combinator over Polytropos open variants using parameterized templates.
--
-- Given an initial arrow producing output @b@ and optionally emitting a switching event
-- @Maybe (Variant sw)@, and a dual record of parameterized templates:
--
-- * As long as the initial arrow emits @(b, Nothing)@, execution continues under the current mode
--   emitting @b@.
-- * When the switching event emits @(b, Just (Variant sw))@, the runtime looks up the corresponding
--   template in the pre-compiled jump table in \(O(1)\) constant time, injects the variant's payload
--   into the input channel @(Record fs, a)@, and transitions execution to that mode.
--
-- Static Analysis Advantage:
-- Unlike Yampa's @switch@ which accepts an opaque closure @c -> SF a b@ (blocking static inspection),
-- 'polySwitch' takes a concrete 'Polytropos.Record' of 'Rope' arrows. Pre-flight static analyzers
-- (@SomeBinEff@) can inspect 100% of reachable modes ahead of time by folding across the record.
polySwitch
  :: forall sw record mantle core a b.
     (ArrowSwitch core)
  => Rope record mantle core a (b, Maybe (Variant sw))
  -> Record (TemplateBranches record mantle core a b sw)
  -> Rope record mantle core a b
polySwitch initRope templates = mkRope $ \weavers ->
  let !initCore = runRope initRope weavers
      !router = makeRopeTemplateRouter @sw templates weavers
  in coreSwitch initCore router
{-# INLINE polySwitch #-}

-- | Switch using an explicitly supplied 'VariantRouter'.
polySwitchWith
  :: (ArrowSwitch core)
  => Rope record mantle core a (b, Maybe (Variant sw))
  -> VariantRouter core a b
  -> Rope record mantle core a b
polySwitchWith initRope router = mkRope $ \weavers ->
  let !initCore = runRope initRope weavers
  in coreSwitch initCore router
{-# INLINE polySwitchWith #-}

-- | Dynamic mode switching directly over core arrows without a 'Rope' wrapper.
switchCore
  :: forall sw core a b.
     (ArrowSwitch core)
  => core a (b, Maybe (Variant sw))
  -> Record (TemplateBranchesCore core a b sw)
  -> core a b
switchCore initCore templates =
  let !router = makeTemplateRouter @sw templates
  in coreSwitch initCore router
{-# INLINE switchCore #-}

-- | Multi-way branching over an open variant emitted in @Either b (Variant sw)@.
--
-- Evaluates the initial arrow: if it produces @Left b@, returns @b@; if it produces
-- @Right var@, dispatches to the corresponding template branch in \(O(1)\) time.
polyBranch
  :: forall sw record mantle core a b.
     (ArrowChoice core, ArrowSwitch core)
  => Rope record mantle core a (Either b (Variant sw))
  -> Record (TemplateBranches record mantle core a b sw)
  -> Rope record mantle core a b
polyBranch initRope templates = mkRope $ \weavers ->
  let !initCore = runRope initRope weavers
      !router = makeRopeTemplateRouter @sw templates weavers
      !switchCoreArrow = initCore >>> arr \case
        Left b    -> (b, Nothing)
        Right var -> (error "polyBranch: branch selected without default output", Just var)
  in coreSwitch switchCoreArrow router
{-# INLINE polyBranch #-}

-- | Parameterized single-mode arrow template switch.
--
-- Replaces Yampa's closure-based @switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b@
-- with an open input channel @(c, a)@.
--
-- Pre-flight static analyzers (@SomeBinEff@) can traverse both the initial graph and the
-- template arrow ahead of time without waiting for runtime value @c@.
templateSwitch
  :: (ArrowChoice core)
  => Rope record mantle core a (b, Maybe c)
  -> Rope record mantle core (c, a) b
  -> Rope record mantle core a b
templateSwitch initRope templateRope = mkRope $ \weavers ->
  let !initCore = runRope initRope weavers
      !templateCore = runRope templateRope weavers
  in (initCore &&& arr id)
       >>> arr (\((b, mc), a) -> maybe (Left b) (\c -> Right (c, a)) mc)
       >>> (arr id ||| templateCore)
{-# INLINE templateSwitch #-}

-- | Single-branch parameterized switch where the initial arrow produces @Either b c@.
templateBranch
  :: (ArrowChoice core)
  => Rope record mantle core a (Either b c)
  -> Rope record mantle core (c, a) b
  -> Rope record mantle core a b
templateBranch initRope templateRope = mkRope $ \weavers ->
  let !initCore = runRope initRope weavers
      !templateCore = runRope templateRope weavers
  in (initCore &&& arr id)
       >>> arr (\(res, a) -> case res of
             Left b  -> Left b
             Right c -> Right (c, a))
       >>> (arr id ||| templateCore)
{-# INLINE templateBranch #-}
