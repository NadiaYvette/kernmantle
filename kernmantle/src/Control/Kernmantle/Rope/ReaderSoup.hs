{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Control.Kernmantle.Rope.ReaderSoup
-- Description : Composable multi-context Reader soup strand for Kernmantle ropes
-- Copyright   : (c) 2026, Nadia Yvette Chambers
-- License     : BSD-3-Clause
-- Maintainer  : nadia.yvette.chambers@ik.me
-- Stability   : experimental
--
-- A composable, extensible multi-context Reader soup strand for 'Rope' arrows.
--
-- In traditional Kernmantle architectures, applications requiring multiple distinct
-- reader capabilities (database connections, metrics handles, configuration records)
-- maintain a separate 'Strand' in the 'Rope' mantle for each individual context.
-- Each distinct strand must be individually woven with its own runner, adding
-- boilerplate and interpretation layers.
--
-- 'ReaderSoupEff' combines any number of labeled environments into a single flat
-- contiguous 'Record' backed by Polytropos's 'Polytropos.Core.Rep'.
--
-- Key Properties:
--
-- * __O(1) Direct Indexing__: Retrieval via 'askSoup' resolves at compile time
--   to a direct literal slot offset synthesized by the Polytropos type-checker plugin.
-- * __Single-Strand Consolidation__: A single @'ReaderSoupStrand' \"soup\" ctxs@ strand
--   supplies all contexts to the arrow program.
-- * __O(K) Sub-Soup Slicing__: Arrow subroutines needing only a subset of contexts
--   can declare a smaller @sub@ row and be woven via 'filteringSoup' using compile-time
--   'SubRow' evidence.
--
-- = Example
--
-- @
-- type AppSoup =
--   '[ \"db\" ':= String
--    , \"port\" ':= Int
--    ]
--
-- prog :: AnyRopeWith '[ \"soup\" ::: ReaderSoupEff AppSoup ] '[Arrow] () String
-- prog = proc () -> do
--   dbUrl <- askSoup #soup #db -< ()
--   p     <- askSoup #soup #port -< ()
--   returnA -< dbUrl ++ \":\" ++ show p
-- @
module Control.Kernmantle.Rope.ReaderSoup
  ( -- * Binary Effect
    ReaderSoupEff(..)
  , SoupEff
  , ReaderSoupStrand
  , SoupStrand

    -- * Query Arrow Combinators
  , askSoup
  , asksSoup
  , askFullSoup

    -- * Weaving & Interpretation
  , weaveSoup
  , weaveSoupK
  , filteringSoup
  , filteringSoupK

    -- * Re-exports from Polytropos
  , LookupContext
  , Field(..)
  , FieldIndex
  , SubRow
  , Record
  , Pair(..)
  , emptyRecord
  , insertField
  , projectRecord
  ) where

import Control.Arrow
import Control.Category
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Primitive.SmallArray (indexSmallArray)
import GHC.TypeLits (Symbol)
import Prelude hiding (id, (.))
import Unsafe.Coerce (unsafeCoerce)

import Control.Kernmantle.Rope
  ( BinEff, HasKleisli, InRope, Label, LooseRope, Rope, Strand, (:::), (:->), strand, weave', weaveK
  )
import Polytropos (Record, emptyRecord, insertField, projectRecord)
import Polytropos.Core
  ( Field(..), FieldIndex(..), Pair(..), Rep(..), RowRep(..)
  , SubRow
  )
import Polytropos.ReaderSoup (LookupContext)

-- | Fetch a field value from a 'Record' using its compile-time synthesized index.
getField :: forall n a r. FieldIndex n r a => Field n -> Record r -> a
getField _ (RowRep (Rep _ v)) =
  case indexSmallArray v (fieldIndex @_ @n @r @a) of
    Identity val -> unsafeCoerce val

-- | Binary effect representing a composite Reader soup arrow.
--
-- The input type parameter @a@ is ignored by 'AskSoup' and 'AsksSoup', allowing
-- context lookups to be placed anywhere in an arrow pipeline.
data ReaderSoupEff (ctxs :: [Pair Symbol Type]) a b where
  AskSoup     :: forall ctxs l c a. (c ~ LookupContext l ctxs, FieldIndex l ctxs c)
              => Field l
              -> ReaderSoupEff ctxs a c
  AsksSoup    :: forall ctxs l c b a. (c ~ LookupContext l ctxs, FieldIndex l ctxs c)
              => Field l
              -> (c -> b)
              -> ReaderSoupEff ctxs a b
  AskFullSoup :: forall ctxs a. ReaderSoupEff ctxs a (Record ctxs)

-- | Convenient alias for 'ReaderSoupEff'.
type SoupEff = ReaderSoupEff

-- | Named strand carrying a 'ReaderSoupEff'.
type ReaderSoupStrand (name :: Symbol) (ctxs :: [Pair Symbol Type]) = name ::: ReaderSoupEff ctxs

-- | Convenient alias for 'ReaderSoupStrand'.
type SoupStrand name ctxs = ReaderSoupStrand name ctxs

--------------------------------------------------------------------------------
-- Query Arrow Combinators
--------------------------------------------------------------------------------

-- | Retrieve context @l@ from a 'ReaderSoupEff' strand in the rope in $O(1)$ time.
--
-- @
-- dbUrl <- askSoup @AppSoup #soup #db -< ()
-- @
askSoup
  :: forall ctxs soup l c r m core a.
     ( InRope soup (ReaderSoupEff ctxs) (Rope r m core)
     , c ~ LookupContext l ctxs
     , FieldIndex l ctxs c
     )
  => Label soup
  -> Field l
  -> Rope r m core a c
askSoup soupLbl fld = strand soupLbl (AskSoup @ctxs @l fld)
{-# INLINE askSoup #-}

-- | Retrieve a projection of context @l@ from a 'ReaderSoupEff' strand in the rope.
--
-- @
-- portStr <- asksSoup @AppSoup #soup #port show -< ()
-- @
asksSoup
  :: forall ctxs soup l c b r m core a.
     ( InRope soup (ReaderSoupEff ctxs) (Rope r m core)
     , c ~ LookupContext l ctxs
     , FieldIndex l ctxs c
     )
  => Label soup
  -> Field l
  -> (c -> b)
  -> Rope r m core a b
asksSoup soupLbl fld f = strand soupLbl (AsksSoup @ctxs @l fld f)
{-# INLINE asksSoup #-}

-- | Retrieve the entire ambient 'Record' environment from the soup strand.
askFullSoup
  :: forall ctxs soup r m core a.
     ( InRope soup (ReaderSoupEff ctxs) (Rope r m core) )
  => Label soup
  -> Rope r m core a (Record ctxs)
askFullSoup soupLbl = strand soupLbl (AskFullSoup @ctxs)
{-# INLINE askFullSoup #-}

--------------------------------------------------------------------------------
-- Weaving & Interpretation
--------------------------------------------------------------------------------

-- | Weave a 'ReaderSoupEff' strand into any pure 'Arrow' core using a flat 'Record'.
--
-- Each context query evaluates in $O(1)$ time by indexing the underlying 'Record'
-- representation directly.
weaveSoup
  :: forall soup ctxs mantle core.
     (Arrow core)
  => Label soup
  -> Record ctxs
  -> LooseRope ('(soup, ReaderSoupEff ctxs) ': mantle) core
 :-> LooseRope mantle core
weaveSoup lbl rec = weave' lbl interp
  where
    interp :: ReaderSoupEff ctxs :-> core
    interp (AskSoup fld)    = arr (\_ -> getField fld rec)
    interp (AsksSoup fld f) = arr (\_ -> f (getField fld rec))
    interp AskFullSoup       = arr (\_ -> rec)
{-# INLINE weaveSoup #-}

-- | Weave a 'ReaderSoupEff' strand into a monadic Kleisli core using a flat 'Record'.
weaveSoupK
  :: forall soup ctxs mantle core m.
     (HasKleisli m core, Applicative m)
  => Label soup
  -> Record ctxs
  -> LooseRope ('(soup, ReaderSoupEff ctxs) ': mantle) core
 :-> LooseRope mantle core
weaveSoupK lbl rec = weaveK lbl interp
  where
    interp :: ReaderSoupEff ctxs a b -> a -> m b
    interp (AskSoup fld)    _ = pure (getField fld rec)
    interp (AsksSoup fld f) _ = pure (f (getField fld rec))
    interp AskFullSoup       _ = pure rec
{-# INLINE weaveSoupK #-}

-- | Narrow an ambient @super@ context 'Record' to a smaller @sub@ row in $O(K)$ time
-- and weave it into a pure 'Arrow' core.
filteringSoup
  :: forall sub super soup mantle core.
     (Arrow core, SubRow sub super)
  => Label soup
  -> Record super
  -> LooseRope ('(soup, ReaderSoupEff sub) ': mantle) core
 :-> LooseRope mantle core
filteringSoup lbl superRec = weaveSoup lbl (projectRecord @sub superRec)
{-# INLINE filteringSoup #-}

-- | Narrow an ambient @super@ context 'Record' to a smaller @sub@ row in $O(K)$ time
-- and weave it into a monadic Kleisli core.
filteringSoupK
  :: forall sub super soup mantle core m.
     (HasKleisli m core, Applicative m, SubRow sub super)
  => Label soup
  -> Record super
  -> LooseRope ('(soup, ReaderSoupEff sub) ': mantle) core
 :-> LooseRope mantle core
filteringSoupK lbl superRec = weaveSoupK lbl (projectRecord @sub superRec)
{-# INLINE filteringSoupK #-}
