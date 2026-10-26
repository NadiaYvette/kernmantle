{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A 'Rope' connects together various effect 'Strand's that get interlaced
-- together.
--
-- A 'Strand' is an effect with a parameter and an output. No constraint is
-- placed on the 'Strand', but once combined in a 'Rope', that 'Rope' will be
-- an 'Arrow' and a 'Profunctor'. 'Strand's in a 'Rope' are named via labels to
-- limit ambiguity.
--
-- An action that targets some 'Strand' can be lifted to a 'Rope' that contains
-- that 'Strand' with the 'strand' function.

module Control.Kernmantle.Rope
  ( -- * Re-exports
    module Data.Profunctor.Trans
  , Cayley (..)
    -- * Rope
  , Rope
  , TightRope
  , LooseRope
  , tighten
  , loosen
  , mapRopeCore
  , mergeStrands
    -- * Binary Effects and Strands
  , BinEff
  , Strand
  , (:::)
  , StrandName
  , StrandEff
  , InRope
  , AllInMantle
  , strand
  , mapStrand
    -- * Rope Interpretation
  , weave, weave', weaveK
  , WeaveAll (..)
  , retwine
  , untwine
    -- * Predicates
  , AnyRopeWith
  , TightRopeWith
  , LooseRopeWith
  , Entwines
  , SatisfiesAll
    -- * Reexports
  , type (:->)
  , Label, fromLabel
  , (&)
  , mkRope
  , runRope
    -- * Polytropos Variant Routing, Switching, Feedback, Mealy, MSF, and Fusion
  , module Control.Kernmantle.Rope.Variant
  , module Control.Kernmantle.Rope.Switch
  , module Control.Kernmantle.Rope.Feedback
  , module Control.Kernmantle.Rope.Mealy
  , module Control.Kernmantle.Rope.MSF
  , module Control.Kernmantle.Rope.Continuity
  , module Control.Kernmantle.Rope.Analysis
  , module Control.Kernmantle.Rope.Fusion
  )
where

import Control.Category
import Control.Arrow
import Data.Bifunctor
import Data.Biapplicative
import Data.Bifunctor.Tannen
import Data.Function ((&))
import Data.Functor.Identity
import Data.Profunctor hiding (rmap)
import Data.Profunctor.Cayley (Cayley (..))
import Data.Profunctor.EffFunctor
import Data.Profunctor.Sieve
import Data.Profunctor.Trans
import Data.Profunctor.Traversing
import Data.Profunctor.Unsafe ((#.))
import GHC.Exts
import GHC.OverloadedLabels
import GHC.TypeLits

import Prelude hiding (id, (.))

import Control.Kernmantle.Arrow
import Control.Kernmantle.Error
import Control.Kernmantle.Rope.Analysis
import Control.Kernmantle.Rope.Continuity
import Control.Kernmantle.Rope.Feedback
import Control.Kernmantle.Rope.Fusion
import Control.Kernmantle.Rope.Internal
import Control.Kernmantle.Rope.Mealy
import Control.Kernmantle.Rope.MSF
import Control.Kernmantle.Rope.Switch
import Control.Kernmantle.Rope.Variant
import Polytropos.Core (FieldIndex (..), SubRow (..))

mapStrand :: forall l (eff :: BinEff) r m c. (InRope l eff (Rope r m c))
          => Label l -> (eff :-> eff) -> Rope r m c :-> Rope r m c
mapStrand l f rope =
  mkRope $ runRope rope . over (rlensfL l) (\(Weaver w) -> Weaver $ w . f)
  where
    over l' g = runIdentity #. l' (Identity #. g)
{-# INLINE mapStrand #-}

-- | Tells whether a collection of @strands@ is in a 'Rope'.
type family rope `Entwines` (strands::[Strand]) :: Constraint where
  rope `Entwines` '[] = ()
  rope `Entwines` ('(name, eff) ': strands ) = ( InRope name eff rope
                                               , rope `Entwines` strands )

-- | Tells whether a type satisfies all the given constraints.
type family (x::k) `SatisfiesAll` (csts::[k -> Constraint]) :: Constraint where
  x `SatisfiesAll` '[] = ()
  x `SatisfiesAll` (c1 ': cnsts) = ( c1 x, x `SatisfiesAll` cnsts)

-- | A @AnyRopeWith strands constraints@
type AnyRopeWith strands coreConstraints a b = forall mantle r core.
  (Rope r mantle core `Entwines` strands, core `SatisfiesAll` coreConstraints)
  => Rope r mantle core a b

type TightRopeWith strands coreConstraints a b = forall mantle core.
  (TightRope mantle core `Entwines` strands, core `SatisfiesAll` coreConstraints)
  => TightRope mantle core a b

type LooseRopeWith strands coreConstraints a b = forall mantle core.
  (LooseRope mantle core `Entwines` strands, core `SatisfiesAll` coreConstraints)
  => LooseRope mantle core a b

type AllInMantle strands mantle core =
  ( LooseRope mantle core `Entwines` strands
  , TightRope mantle core `Entwines` strands )

tighten :: LooseRope m core :-> TightRope m core
tighten = id
{-# INLINE tighten #-}

loosen :: TightRope m core :-> LooseRope m core
loosen = id
{-# INLINE loosen #-}

weave
  :: forall name binEff mantle core.
     Label name
  -> ((LooseRope ('(name,binEff) ': mantle) core :-> core) -> (binEff :-> core))
  -> (LooseRope ('(name,binEff) ': mantle) core :-> LooseRope mantle core)
weave _ interpFn rope = mkRope $ \r ->
  let runThatRope :: LooseRope ('(name,binEff) ': mantle) core :-> core
      runThatRope rope' =
        runRope rope' (Weaver (interpFn runThatRope) :& r)
  in runThatRope rope

weave'
  :: forall name binEff mantle core.
     Label name
  -> (binEff :-> core)
  -> (LooseRope ('(name,binEff) ': mantle) core :-> LooseRope mantle core)
weave' lbl interpFn = weave lbl (const interpFn)
{-# INLINE weave' #-}

weaveK :: (HasKleisli m core)
       => Label name
       -> (forall a b. binEff a b -> a -> m b)
       -> (LooseRope ('(name,binEff) ': mantle) core :-> LooseRope mantle core)
weaveK lbl interpFn = weave' lbl (liftKleisli . interpFn)
{-# INLINE weaveK #-}

-- | Supports 'weaveAll' over any 'Rope'
class WeaveAll mantle core where
  weaveAll :: (forall lbl eff. lbl -> eff :-> core)
           -> LooseRope mantle core
          :-> core

instance WeaveAll '[] core where
  weaveAll _ = untwine
  {-# INLINE weaveAll #-}

instance {-# OVERLAPPABLE #-} (WeaveAll mantle core, KnownSymbol name)
  => WeaveAll ('(name, eff) ': mantle) core where
  weaveAll interpFn = weaveAll interpFn
                    . weave' lbl (interpFn lbl)
    where lbl = fromLabel @name

retwine :: (RetwinableAs r strands core strands')
        => Rope r strands core
       :-> Rope r strands' core
retwine r = mkRope $ runRope r . rcast
{-# INLINE retwine #-}

mergeStrands :: Label n1
             -> Label n2
             -> LooseRope ( '(n1,binEff) ': '(n2,binEff) ': mantle ) core
            :-> LooseRope ( '(n1,binEff) ': mantle ) core
mergeStrands _ _ rope = mkRope $ \(r@(Weaver w) :& rest) ->
  runRope rope (r :& Weaver w :& rest)
{-# INLINE mergeStrands #-}

untwine :: LooseRope '[] core :-> core
untwine r = runRope r RNil
{-# INLINE untwine #-}
