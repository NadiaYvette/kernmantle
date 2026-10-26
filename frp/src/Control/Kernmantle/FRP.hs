{-# LANGUAGE PolyKinds #-}

{-|
Module      : Control.Kernmantle.FRP
Description : Functional Reactive Programming, Coalgebras, and Pre-flight Verification for Kernmantle Ropes
Copyright   : (c) 2026 The Kernmantle Authors
License     : BSD-3-Clause
Maintainer  : nadia.yvette.chambers@ik.me
Stability   : experimental
Portability : non-portable (GHC extensions)

Dedicated FRP and coalgebraic stream execution suite for 'Control.Kernmantle.Rope'.

= Three-Timescale Architectural Separation

'Control.Kernmantle.FRP' structures reactive computation across three explicit,
non-interfering timescales:

1. __Design / Pre-Flight Time__:
   * Modeled by 'Control.Kernmantle.Rope.Analysis' ('SomeBinEff', 'AnalyzedArrow').
   * Static analyzers fold applicative metadata functors across 100% of reachable
     mode templates in \(\mathcal{O}(N)\) array lookups before the system begins ticking.
   * Enforces hardware port topologies, multi-rate Nyquist bounds, and resource budgets.

2. __Discrete Mode Switching Time (Epoch Boundaries)__:
   * Modeled by 'Control.Kernmantle.Rope.Continuity' ('continuousMSF') and
     'Control.Kernmantle.Rope.Switch' ('polySwitch').
   * Evaluates mode switching variants and vectors shared state across mode boundaries
     with \(\mathcal{O}(1)\) unboxed jump tables, preserving PID integrals and filter states.

3. __Continuous Sample Tick Time (Inner FRP Loop)__:
   * Modeled by 'Control.Kernmantle.Rope.MSF' ('MSF', Dunai/Rhine bridge).
   * Causal stream functions step pure or monadic values with zero GADT overhead,
     zero 'Control.Monad.Fix.MonadFix', and zero bottom blackhole risks.

= Re-exported Modules

* "Control.Kernmantle.Rope": Base Kernmantle rope primitives, strands, and weavers.
* "Control.Kernmantle.Rope.Mealy": Existential Mealy strands, state lenses, and flat coalgebras.
* "Control.Kernmantle.Rope.MSF": Dunai Monadic Stream Functions, time-domain primitives, and epoch switching.
* "Control.Kernmantle.Rope.Continuity": Row-polymorphic state continuity across dynamic mode switches.
* "Control.Kernmantle.Rope.Analysis": Pre-flight static resource analysis with 'SomeBinEff'.
-}
module Control.Kernmantle.FRP
  ( -- * Base Rope Re-exports
    module Control.Kernmantle.Rope

    -- * Coalgebraic Engine & State Lenses
  , module Control.Kernmantle.Rope.Mealy

    -- * Monadic Stream Functions & Continuous Time
  , module Control.Kernmantle.Rope.MSF

    -- * Row-Polymorphic State Continuity
  , module Control.Kernmantle.Rope.Continuity

    -- * Pre-Flight Static Resource Analysis
  , module Control.Kernmantle.Rope.Analysis
  ) where

import Control.Kernmantle.Rope
import Control.Kernmantle.Rope.Mealy
import Control.Kernmantle.Rope.MSF
import Control.Kernmantle.Rope.Continuity
import Control.Kernmantle.Rope.Analysis
