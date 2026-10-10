{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Kernmantle.Rope.Internal where

import Control.Category
import Control.Arrow
import Control.Monad (forM_)
import Data.Profunctor hiding (rmap)
import Data.Bifunctor
import Data.Biapplicative
import Data.Bifunctor.Tannen
import Data.Kind
import Data.Primitive.SmallArray
import Data.Profunctor.Cayley
import Data.Profunctor.EffFunctor
import Data.Profunctor.Monad
import Data.Profunctor.Traversing
import Data.Profunctor.Trans
import GHC.Exts (Any)
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

import Prelude hiding (id, (.))

import Control.Kernmantle.Arrow
import Control.Kernmantle.Error
import Polytropos.Core (FieldIndex (..), SubRow (..))


-- | The kind for all binary effects. First param is usually an input
-- (contravariant) of the effect and second one an output (covariant).
type BinEff = Type -> Type -> Type

-- | The kind for unary effects
type UnaryEff = Type -> Type

-- | The kind for a named binary effect.
type Strand = (Symbol, BinEff)

type (:::) (name :: Symbol) (eff :: BinEff) = '(name, eff)
infixr 6 :::

type family StrandName (t :: Strand) :: Symbol where
  StrandName '(name, eff) = name

type family StrandEff (t :: Strand) :: BinEff where
  StrandEff '(name, eff) = eff

-- | Type-level list append
type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- | The kind for records that will contain 'Weaver's.
type RopeRec = (Strand -> Type) -> [Strand] -> Type

-- | Runs one @strand@ in an @interp@ effect.
newtype Weaver (interp :: BinEff) (strand :: Strand) = Weaver
  { weaveStrand :: StrandEff strand :-> interp }

mapWeaverInterp :: (interp :-> interp')
                -> Weaver interp strand
                -> Weaver interp' strand
mapWeaverInterp f (Weaver w) = Weaver $ f . w
{-# INLINE mapWeaverInterp #-}

-- | High-performance flat record backed by Polytropos SmallArray
newtype Rec (f :: Strand -> Type) (mantle :: [Strand]) = Rec
  { unRec :: SmallArray Any }

data RecView f rs where
  RecViewNil  :: RecView f '[]
  RecViewCons :: f x -> Rec f xs -> RecView f (x ': xs)

viewRec :: Rec f rs -> RecView f rs
viewRec (Rec arr)
  | sizeofSmallArray arr == 0 = unsafeCoerce RecViewNil
  | otherwise =
      let !x = unsafeCoerce (indexSmallArray arr 0)
          !xs = Rec (cloneSmallArray arr 1 (sizeofSmallArray arr - 1))
      in unsafeCoerce (RecViewCons x xs)
{-# INLINE viewRec #-}

consRec :: f x -> Rec f xs -> Rec f (x ': xs)
consRec x (Rec arr) =
  let n = sizeofSmallArray arr
  in Rec $ runSmallArray $ do
       m <- newSmallArray (n + 1) (unsafeCoerce x)
       copySmallArray m 1 arr 0 n
       pure m
{-# INLINE consRec #-}

pattern RNil :: () => (rs ~ '[]) => Rec f rs
pattern RNil <- (viewRec -> RecViewNil)
  where RNil = Rec (smallArrayFromList [])

pattern (:&) :: () => (rs ~ (x ': xs)) => f x -> Rec f xs -> Rec f rs
pattern x :& xs <- (viewRec -> RecViewCons x xs)
  where x :& xs = consRec x xs
infixr 5 :&
{-# COMPLETE RNil, (:&) #-}

-- | Read a field from a Rec in O(1) via Polytropos FieldIndex
rgetf :: forall l eff mantle f proxy. FieldIndex l mantle eff => proxy l -> Rec f mantle -> f '(l, eff)
rgetf _ (Rec arr) =
  let idx = fieldIndex @_ @l @mantle @eff
  in unsafeCoerce (indexSmallArray arr idx)
{-# INLINE rgetf #-}

-- | Update a field in a Rec via Polytropos FieldIndex
rlensfL :: forall l eff mantle f functor proxy. (FieldIndex l mantle eff, Functor functor)
        => proxy l
        -> (f '(l, eff) -> functor (f '(l, eff)))
        -> Rec f mantle
        -> functor (Rec f mantle)
rlensfL _ f (Rec arr) =
  let idx = fieldIndex @_ @l @mantle @eff
      oldVal = unsafeCoerce (indexSmallArray arr idx)
  in fmap (\newVal ->
       Rec $ runSmallArray $ do
         m <- thawSmallArray arr 0 (sizeofSmallArray arr)
         writeSmallArray m idx (unsafeCoerce newVal)
         pure m
     ) (f oldVal)
{-# INLINE rlensfL #-}

-- | Map over all fields in Rec
rmap :: (forall x. f x -> g x) -> Rec f m -> Rec g m
rmap f (Rec arr) =
  let n = sizeofSmallArray arr
  in Rec $ runSmallArray $ do
       m <- newSmallArray n (unsafeCoerce ())
       forM_ [0 .. n - 1] $ \i -> do
         let !x = unsafeCoerce (indexSmallArray arr i)
         writeSmallArray m i (unsafeCoerce (f x))
       pure m
{-# INLINE rmap #-}

-- | Append two records in a single flat array allocation
rappend :: Rec f m1 -> Rec f m2 -> Rec f (m1 ++ m2)
rappend (Rec a1) (Rec a2) =
  let n1 = sizeofSmallArray a1
      n2 = sizeofSmallArray a2
  in Rec $ runSmallArray $ do
       m <- newSmallArray (n1 + n2) (unsafeCoerce ())
       copySmallArray m 0 a1 0 n1
       copySmallArray m n1 a2 0 n2
       pure m
{-# INLINE rappend #-}

-- | Project a sub-record in O(1) bulk copy via Polytropos SubRow
rcast :: forall sub super f. SubRow sub super => Rec f super -> Rec f sub
rcast (Rec arr) =
  let indices = subRowIndices @sub @super
      n = length indices
  in Rec $ runSmallArray $ do
       m <- newSmallArray n (unsafeCoerce ())
       forM_ (zip [0..] indices) $ \(dst, src) ->
         writeSmallArray m dst (indexSmallArray arr src)
       pure m
{-# INLINE rcast #-}

-- | Tight record representation (same as Rec under Polytropos)
type ARec = Rec

toARec :: Rec f m -> ARec f m
toARec = id
{-# INLINE toARec #-}

fromARec :: ARec f m -> Rec f m
fromARec = id
{-# INLINE fromARec #-}

-- | Compatibility constraint synonyms
type RLength (m :: [k]) = ()
type NatToInt n = ()
type RecApplicative m = ()
type RPureConstrained c m = ()
type IndexableField m = ()
type ToARec m = ()
type RecElemFCtx record f = ()
type RMap (m :: [k]) = ()

-- | The internal implementation of a Rope.
newtype RopeRunner (record::RopeRec) (mantle::[Strand]) (interp::BinEff) (core::BinEff) a b =
  RopeRunner (record (Weaver interp) mantle -> core a b)
  
  deriving ( Category
           , Arrow, ArrowChoice, ArrowLoop, ArrowZero, ArrowPlus
           , Profunctor, Strong, Choice, Closed, Costrong, Cochoice
           , Mapping, Traversing
           , ThrowEffect ex, TryEffect ex
           , SieveTrans f, HasAutoIdent eff )
    via Cayley ((->) (record (Weaver interp) mantle)) core
  deriving (ProfunctorFunctor, ProfunctorMonad)
    via Cayley ((->) (record (Weaver interp) mantle))

  deriving (Bifunctor, Biapplicative)
    via Tannen ((->) (record (Weaver interp) mantle)) core
  deriving (EffFunctor, EffPointedFunctor)
    via Tannen ((->) (record (Weaver interp) mantle))

instance Functor (core a) => Functor (RopeRunner record mantle interop core a) where
    fmap f (RopeRunner run) = RopeRunner $ \rec -> fmap f (run rec)

instance EffProfunctor (RopeRunner Rec m) where
  effdimap f g (RopeRunner run) = RopeRunner $
    g . run . rmap (mapWeaverInterp f)
  {-# INLINE effdimap #-}

-- | Splits a 'RopeRunner' in two parts
splitRopeRunner :: RopeRunner Rec (mantle1 ++ mantle2) interp core
               :-> RopeRunner Rec mantle1 interp (RopeRunner Rec mantle2 interp core)
splitRopeRunner (RopeRunner f) = RopeRunner $ \r1 -> RopeRunner $ \r2 ->
  f $ r1 `rappend` r2

-- | Indicates that @strands@ can be reordered and considered a subset of @strands'@
type RetwinableAs record strands core strands' = (record ~ Rec, SubRow strands strands')

joinRopeRunner :: (RetwinableAs r mantle1 interp mantle
                  ,RetwinableAs r mantle2 interp mantle)
               => RopeRunner r mantle1 interp (RopeRunner r mantle2 interp core)
              :-> RopeRunner r mantle interp core
joinRopeRunner (RopeRunner f) = RopeRunner $ \r -> case f (rcast r) of
  RopeRunner f' -> f' (rcast r)

-- | Wrap each strand effect inside a type constructor
type family MapStrandEffs f mantle where
  MapStrandEffs f '[] = '[]
  MapStrandEffs f ( s ': strands ) = '(StrandName s, f (StrandEff s)) ': MapStrandEffs f strands

effmapRopeRec :: (EffFunctor f)
              => Rec (Weaver interp) strands
              -> Rec (Weaver (f interp)) (MapStrandEffs f strands)
effmapRopeRec RNil = RNil
effmapRopeRec (Weaver w :& rest) = Weaver (effrmap w) :& effmapRopeRec rest

unwrapRopeRunner :: (EffFunctor f)
                 => RopeRunner Rec (MapStrandEffs f strands) (f core) core
                :-> RopeRunner Rec strands core core
unwrapRopeRunner (RopeRunner f) = RopeRunner $ f . effmapRopeRec
{-# INLINE unwrapRopeRunner #-}

unwrapSomeStrands :: (EffFunctor f)
                  => (f core' :-> interp)
                  -> (RopeRunner Rec mantle2 interp core :-> core')
                  -> RopeRunner Rec (MapStrandEffs f mantle1 ++ mantle2) interp core
                 :-> RopeRunner Rec mantle1                              core'  core'
unwrapSomeStrands f g = unwrapRopeRunner . effdimap f g . splitRopeRunner
{-# INLINE unwrapSomeStrands #-}
