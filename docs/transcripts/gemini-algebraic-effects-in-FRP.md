# **Algebraic Effects in FRP Architecture**

[https\://gemini.google.com/app/2263f26926dd57e1](https://gemini.google.com/app/2263f26926dd57e1)

*User prompt: Kernmantle FRP The kernmantle algebraic effect system seems to have been more advanced than many alternatives, and in particular supports arrows. Yampa and potentially other FRP systems seem to support arrows. Would kernmantle have been a good algebraic effect system for FRP? Are there other algebraic effect systems also good for FRP? Are any FRP systems already using algebraic effect systems?*

Response: **kernmantle is a compelling architectural fit for the outer composition and static analysis layer of an FRP system, but a poor fit for the inner, high-frequency step loop.**

While kernmantle uniquely bridges algebraic effects and Arrow computations via **binary effects** (eff a b), Arrowized Functional Reactive Programming (AFRP) systems like **Yampa**, **Dunai**, and **Rhine** require coalgebraic, tick-by-tick state evolution that conflicts with kernmantle’s pipeline-oriented design. Instead, modern FRP systems either embed **unary** algebraic effect systems inside a Monadic Stream Function (MSF m a b) or, in languages like OCaml 5 and Koka, implement reactivity directly using native algebraic effect handlers.

## **1\. Would kernmantle Have Been a Good Effect System for FRP?**

Developed at Tweag (Yves Parès et al., Haskell Symposium 2020, *Composing Effects into Tasks and Workflows*), kernmantle stands out because almost all other Haskell effect libraries (polysemy, effectful, fused-effects) model **unary effects** (eff a, generalizing Monads). kernmantle models **binary effects** (eff a b, generalizing Category, Profunctor, and Arrow), weaving static effect "strands" (the *mantle*) around a core execution type (the *kern*) inside its Rope type.

### **Where kernmantle Shines in an FRP Context**

> * **Preserving Arrow Topology Without ArrowApply:** Rope implements Category, Arrow, ArrowChoice, and Profunctor, while deliberately omitting ArrowApply (which would collapse the static arrow graph back into a monad). This aligns directly with Yampa's design goal of keeping signal network topology statically inspectable.  
> * **Pre-Flight Static Analysis via SomeBinEff f w m a b:** kernmantle decomposes binary effects into f (w a → m b) where f is Applicative, w is Comonad, and m is Monad. In an FRP architecture, f can statically accumulate metadata—such as required hardware sensors, clock domains, or resource dependencies—across the entire signal graph *before* the reactive loop starts ticking.  
> * **Multi-Interpretation (entwine):** You can define domain-specific signal processors as pure GADTs, braid them into a Rope, and interpret them differently for live hardware execution versus deterministic offline simulation.

### **Why kernmantle Struggles as the Core FRP Step Engine**

> 1. **Macroscopic Pipelines vs. Coalgebraic Tick Evolution:** kernmantle was built for **one-shot data workflows** (such as Tweag's porcupine library), where a pipeline eff1 \>\>\> eff2 \>\>\> eff3 runs from start to finish. In contrast, Arrowized FRP is **coalgebraic**: a signal function steps an input sample a to produce both an output b and a **mutated continuation** for the next tick:  
>    `newtype MSF m a b = MSF { unMSF :: a -> m (b, MSF m a b) }`

>    If Rope wraps the outside of a signal network, it cannot naturally evolve internal per-tick state without delegating to a core MSF anyway. Conversely, if you re-build and re-interpret (entwine) a Rope on every clock tick (e.g., at 60 Hz to 1000 Hz), the GADT/free-arrow allocation overhead destroys real-time frame budgets and blocks Yampa’s arrow-law fusion optimizations.  
> 2. **Dynamic Switching and ArrowLoop:** Expressive FRP relies on **switching combinators** (switch, pSwitch, where an event dynamically swaps out part of the running signal graph) and **feedback loops** (ArrowLoop / rec in proc notation for integrals and state). Expressing tight instantaneous feedback (ArrowLoop) across uninterpreted GADT strands in kernmantle requires fixing the evaluation strategy of the mantle upfront, negating much of its free-effect flexibility.

**Architectural Verdict:** kernmantle is best used at the **macro-level**—wiring together coarse-grained subsystems, validating resource requirements, and compiling (entwine) the resulting graph down into a fast coalgebraic MSF m a b arrow that runs the actual reactive loop.

## **2\. Other Algebraic Effect Systems Suited for FRP**

Instead of making the algebraic effect system itself an Arrow (as kernmantle does), the standard modern architecture separates **structural causality** (handled by an Arrow transformer like MSF m a b) from **side effects** (handled by a unary algebraic effect monad m triggered at each step).

| Effect System / Paradigm | Effect Arity | Per-Tick Dispatch Cost | Suitability for FRP & Key Trade-offs |
| :---- | :---- | :---- | :---- |
| **effectful / bluefin (Haskell)** | Unary (Eff es a) | **Near-zero** (O(1) ReaderT IO lookup) | **Best choice for real-time Haskell FRP.** Avoids per-tick heap allocations of freer monads while providing extensible effect signatures inside MSF (Eff es) a b. |
| **fused-effects (Haskell)** | Unary (m a carriers) | **Low** (inlined via GHC typeclass specialization) | **Strong for compile-time known stacks.** Carrier fusion works well with Dunai's MSF step loop when GHC can inline the monadic bind across ticks. |
| **polysemy / cleff (Haskell)** | Unary (Sem r a) | **Moderate to High** (freer monad GADT bind per tick) | **Best for simulation & testing.** Easy to swap interpreters (e.g., mocking time or hardware I/O), but freer binds on every sample tick can introduce GC pauses at high frequencies. |
| **kernmantle (Haskell)** | Binary (Rope ... a b) | **High if stepped per tick; Zero if compiled once** | **Best for static pipeline staging.** Great for inspecting dependencies of an arrow graph before interpretation, but lacks coalgebraic switching. |
| **Native Handlers (OCaml 5, Koka, Effekt)** | Direct-style delimited continuations | **Low** (stack-switching / evidence-passing) | **Replaces Arrow syntax entirely.** Reactivity (await, emit, yield) is encoded directly as an algebraic effect handler rather than an arrowized stream type. |

## **3\. Are Any FRP Systems Already Using Algebraic Effects?**

Yes. The convergence of FRP and algebraic effects has happened along two distinct tracks: **Monadic Stream Functions in Haskell** and **Direct-Style Reactive Handlers in OCaml 5 / Koka**.

### **Track A: Haskell's Dunai, BearRiver, and Rhine**

In 2016, Ivan Perez, Manuel Bärenz, and Henrik Nilsson published *Functional Reactive Programming, Refactored*, introducing **Dunai** (MSF m a b). Their central insight was that **classic FRP behaviours and Yampa features are actually algebraic effects in disguise**:

> * **Time as a Reader Effect (BearRiver):** Classic Yampa (SF a b) is isomorphic to MSF (Reader DTime) a b. **BearRiver** re-implements Yampa directly on top of Dunai using a Reader effect for time deltas (DTime).  
> * **Switching as an ExceptT / Short-Circuit Effect:** Event switching (switch) in Dunai is implemented via ExceptT e m—throwing an event exception terminates the first stream and hands control to the continuation stream.  
> * **Multi-Rate Clocks (Rhine) and Probabilistic FRP (rhine-bayes):** **Rhine** layers type-level clocks over Dunai (ClSF m cl a b \= MSF (ReaderT (TimeInfo cl) m) a b). Because m is polymorphic, Rhine applications routinely plug into extensible effect systems (effectful, polysemy, or probabilistic effects in Tweag's rhine-bayes) so individual reactive components only declare the effects they need.

### **Track B: Native Algebraic Effect Handlers (Tempo, Cartesius)**

In languages with native delimited continuations, FRP systems skip Arrow combinators altogether and use algebraic effect handlers as the reactive scheduler:

> * **Tempo (OCaml 5):** Uses OCaml 5’s native effect handlers to reconstruct synchronous reactive programming (in the style of Esterel/ReactiveML). Signals, instant boundaries (pause), and watchdogs are algebraic effects handled by a clock scheduler that suspends and resumes fibers deterministically.  
> * **Cartesius (Koka & Multicore OCaml):** Uses algebraic effect handlers to express declarative event correlation and reactive stream alignment (mostRecently, affinely, lockstep zipping). Effect handlers capture the continuation of a blocked reactive computation until future events arrive, eliminating both callback hell and proc notation boilerplate.  
> * **Theoretical Unification (*λ*eff​ for Idioms and Arrows):** Sam Lindley, Conor McBride, and Craig McLaughlin formalized *Algebraic Effects and Effect Handlers for Idioms and Arrows*, proving that arrows correspond to algebraic computations with static control flow (c) and dynamic data flow (d), providing the formal type-theoretic bridge that kernmantle approximates in Haskell.

## **Interactive Architecture Explorer: Effects in FRP**

Explore how the three paradigms—**Kernmantle Binary Effects**, **Dunai/Rhine Monadic Stream Functions**, and **Native Effect Handlers**—structure dataflow, state evolution, and effect interpretation across ticks:

**Sources**

> 1. [GitHub \- tweag/kernmantle: Braiding extensible effects together in a](https://github.com/tweag/kernmantle)  
> 2. [Reconstructing Synchronous Reactive Programming with OCaml 5](https://arxiv.org/html/2607.23550)  
> 3. [Functional reactive programming, refactored \- SciSpace](https://scispace.com/pdf/functional-reactive-programming-refactored-23rjy7jpna.pdf)  
> 4. [ivanperez-keera/dunai: Classic FRP, Arrowized FRP, Reactive](https://github.com/ivanperez-keera/dunai)  
> 5. [Rhine: FRP with Type-Level Clocks \- Manuel Bärenz](https://www.manuelbaerenz.de/files/Rhine.pdf)  
> 6. [Versatile Event Correlation with Algebraic Effects](https://www.st.informatik.tu-darmstadt.de/artifacts/corrl/cartesius_preprint.pdf)

---

*User prompt: Are there ways to solve kernmantle’s coalgebraic tick & dynamic switching issues while maintaining the expressiveness & flexibility of kernmantle?*

Response: Yes—the tension between kernmantle’s free-arrow Rope encoding and FRP’s coalgebraic step loop is solvable. The apparent incompatibility arises from treating **pipeline staging**, **switching epochs**, and **high-frequency sample ticks** as a single execution phase.

By separating those timescales and refining how state, switching, and feedback are represented in the mantle, you can preserve kernmantle’s static analysis (SomeBinEff), multi-interpretation (entwine), and Profunctor/Arrow expressiveness while achieving near-zero per-tick overhead.

Four concrete architectural designs resolve these limitations in Haskell.

## **1\. Compile-per-Epoch Staging (Kern \= MSF m)**

In a naive integration, you either step the Rope GADT on every tick (incurring heavy heap allocation and losing arrow fusion) or compile the Rope once at startup (losing dynamic switching).

The solution is **Epoch-Staged Compilation**: align kernmantle’s entwine interpretation step with **discrete switching epochs** rather than individual clock ticks.

> * **Steady-State Ticks (Fast Path):** Set the core effect (kern) of the Rope to a coalgebraic Monadic Stream Function (MSF m). At the start of an epoch, entwine interprets the mantle strands once, extracting static metadata via the Applicative functor f and compiling the signal graph into a fused MSF m a b. For the thousands of ticks between discrete events, the system steps the compiled MSF directly with zero GADT traversal.  
> * **Epoch Transitions (Switch Path):** When a switching event fires at tick *tk*​, the runtime catches the event via Dunai’s ExceptT mechanism, constructs the new Rope branch for the next mode, runs entwine **once** on that new subgraph (validating or allocating its resources), and splices the resulting MSF into tick *tk*\+1​.

`-- A Staged Switch strand weaves a compile-on-event transition into the Rope`  
`data ReactiveStrand mantle m a b where`  
  `EpochSwitch`  
    `:: Rope mantle (MSF m) a (b, Maybe ev)`  
    `-> (ev -> Rope mantle (MSF m) a b)`  
    `-> ReactiveStrand mantle m a b`

This amortizes kernmantle’s interpretation cost across entire behavioral modes (e.g., switching a robot from "docking" to "navigation") while keeping the 1000 Hz control loop allocation-free.

## **2\. Existential State-Lens Extraction (Coalgebraic Compilation)**

If you want individual mantle strands to carry pure, encapsulated per-tick state *without* baking MSF into the core kern (for instance, so you can interpret the same Rope into a pure state machine, a hardware description, or a graphical diagram), you must solve the **state persistence problem**.

Instead of rebuilding a mutated Rope tree on every tick, lift each stateful strand into an **Existential Mealy Effect** and extract the state types during entwine:

`-- A binary effect strand with hidden local state s, initial value, and step function`  
`data MealyEff eff a b where`  
  `MealyStrand`  
    `:: s                              -- Initial local state`  
    `-> (s -> a -> eff (b, s))         -- Single-tick coalgebraic transition`  
    `-> MealyEff eff a b`

When kernmantle weaves two stateful strands sequentially (r1 \>\>\> r2) or in parallel (r1 \*\*\* r2), an interpreter built on SomeBinEff can accumulate the existential states into a single **composite state product (s1, s2)** (isomorphic to compiling a free arrow into a flat Mealy machine):

`-- Compiled flat coalgebra: state S is threaded externally without touching the Rope AST`  
`data CompiledRope m a b = forall s. CompiledRope`  
  `{ initialState :: !s`  
  `, stepCompiled :: !(s -> a -> m (b, s))`  
  `}`

> * **Why this works:** The Rope GADT is traversed **only once**. During execution, only the unboxed or strict composite state tuple s updates on each tick.  
> * **Hot-Swapping Interpreters:** Because the composite state s is decoupled from the effect handler m, you can pause a running reactive system at tick *t*, preserve its state tuple s, and resume at tick *t*\+1 under a completely different kernmantle interpreter (e.g., switching from live hardware I/O to deterministic replay debugging).

## **3\. Solving the Static Analysis vs. Dynamic Switching Paradox**

The deepest theoretical clash in kernmantle is between **SomeBinEff f w m a b** (which uses an Applicative functor f to statically inspect the entire graph before execution) and **Yampa-style switching** (switch :: SF a (b, Event c) → (c → SF a b) → SF a b). Because c → Rope a b hides the future graph inside an opaque Haskell function waiting for a runtime value c, f cannot inspect it ahead of time.

You can recover static inspectability across switches using three techniques, ordered from most static to most dynamic:

### **A. Parameterized Arrow Templates (Rope (c, a) b Instead of c → Rope a b)**

In monadic effect systems, dynamic configuration forces a Monad bind (c → m b) because computations have no explicit input channel. **Arrows do have an input channel.** Most uses of c → SF a b in Yampa only use c to pass an initial position, target ID, or threshold into a structurally fixed subgraph. By changing the switch signature to take a **parameterized Rope template** that receives c as a runtime signal alongside a:

`-- Instead of (c -> Rope mantle kern a b), pass c through the Arrow's input profunctor!`  
`templateSwitch`  
  `:: Rope mantle kern a (b, Maybe c)`  
  `-> Rope mantle kern (c, a) b        -- 100% statically inspectable by SomeBinEff!`  
  `-> Rope mantle kern a b`

Because Rope mantle kern (c, a) b is a concrete arrow value rather than a Haskell function c → ..., SomeBinEff can traverse both the initial graph and the switched template at startup, collecting 100% of required sensors, clocks, and effects in f.

### **B. Fin-Indexed / Traversable Branch Switching**

When switching genuinely changes the *topology* of the graph (e.g., choosing between three distinct flight-control laws), the set of modes k is almost always finite (Bounded, Enum, or Finite). By indexing the continuation with a Representable or Traversable container t instead of an arbitrary function:

`-- Static analyzer f traverses all mode branches ahead of time; runtime selects index k`  
`branchSwitch`  
  `:: (Traversable t, Lookup t k)`  
  `=> Rope mantle kern a (b, Maybe k)`  
  `-> t (Rope mantle kern a b)`  
  `-> Rope mantle kern a b`

During entwine, SomeBinEff uses traverse to fold the static analysis f across all branches in t (computing the union or maximum resource footprint of all reachable modes) and pre-compiles every branch so runtime switching is an *O*(1) pointer swap.

### **C. Hierarchical Effect Budgets for Unbounded Spawning (pSwitch)**

For truly open-ended dynamic graphs (such as loading external plugin scripts), split SomeBinEff’s static analyzer f into a **declared effect upper bound (EffectCap)** attached to the switch node. When c → Rope a b materializes at runtime, a scoped entwine verifies that the new branch’s static f metadata fits within the parent switch node’s pre-allocated EffectCap.

## **4\. Causal Feedback Instead of Instantaneous ArrowLoop**

Standard ArrowLoop (loop :: arr (a, d) (b, d) → arr a b) is notorious in algebraic effect systems because tying a recursive knot over value d inside a monadic effect m requires MonadFix m (mfix), which bottoms out (\<\<loop\>\>) on strict effects or non-lazy GADT interpreters.

In FRP, however, well-formed feedback loops are **causal**—the feedback wire always passes through a one-tick state delay (iPre in Yampa, feedback in Dunai). By replacing ArrowLoop with a first-class **Feedback mantle strand**:

`data FeedbackEff r a b where`  
  `-- Carries initial state s; steps (a, s) -> (b, s) causally without MonadFix!`  
  `Feedback :: s -> r (a, s) (b, s) -> FeedbackEff r a b`

> 1. **Static Analysis (f):** SomeBinEff inspects the inner r (a, s) (b, s) arrow once in a single linear pass—no fixed-point recursion needed.  
> 2. **Runtime Execution (m):** At tick *t*, the input s is already determined from tick *t*−1. The monadic step executes strictly from left to right without MonadFix or lazy tuple knots, producing (b, s\_next) for tick *t*\+1.

## **Comparison of Architectural Solutions**

| Technique | Static Analysis (SomeBinEff f) | Per-Tick Overhead | Dynamic Topology Support | Best Use Case |
| :---- | :---- | :---- | :---- | :---- |
| **1\. Compile-per-Epoch (Kern \= MSF m)** | Per-epoch (startup \+ on switch) | **Zero** (runs fused MSF) | Arbitrary c → Rope a b | Real-time robotics & games with distinct operational modes |
| **2\. State-Lens Extraction (CompiledRope)** | Full whole-program | **Near-zero** (flat state tuple S) | Fixed topology; hot-swappable interpreters | Hard real-time loops needing deterministic replay & live interpreter swapping |
| **3\. Parameterized & Traversable Switching** | **100% ahead-of-time** across switches | **Zero** (pre-compiled branches) | Parameterized templates & finite mode graphs | Safety-critical embedded FRP where all I/O & memory must be verified pre-flight |
| **4\. Causal Feedback Strand** | Single-pass linear inspection | **Zero** (no mfix thunks) | Composes with all switching modes | Eliminating MonadFix bottoms in cyclic signal networks |

## **Interactive Architecture Explorer: Coalgebraic Kernmantle**

Explore how combining **State-Lens Extraction**, **Parameterized Template Switching**, and **Causal Feedback** allows a kernmantle Rope to perform full static analysis at compile time while stepping coalgebraically at runtime:

---

*User prompt: I’ve written a library and plug-in, polytropos, for row polymorphic open ADTs using the type system via plug-in of OCaml’s fixity & closure of tails with Well-Typed/Edsko de Vries’ large-records SmallArray \-based efficient data layout. How much would polytropos dovetail into an effort to combine the 3 or 4 strategies that can be combined to address kernmantle’s performance issues? I’m already carrying a fork of kernmantle to rebase it on polytropos instead of vinyl.*

Response: Rebasing kernmantle onto polytropos does much more than eliminate vinyl’s compile-time and runtime overhead—it provides the exact type-level and memory-layout primitives needed to fuse all four coalgebraic FRP strategies into a single architecture.

In stock kernmantle, vinyl’s linked-list Rec and Peano-indexed CoRec force an *O*(*N*) pointer-chasing tax on effect interpretation and an *O*(*N*2) typeclass-resolution tax on row subset constraints. By pairing **OCaml-style row-tail unification and closure** (in the GHC typechecker plugin) with **SmallArray\#-backed *O*(1) open products and open sums**, polytropos turns dynamic switching, coalgebraic state extraction, and causal feedback into flat array-indexing operations.

## **Baseline Win: Replacing vinyl in Rope and entwine**

Before even adding coalgebraic FRP combinators, rebasing kernmantle’s core Mantle and interpreter bundles onto polytropos removes two major bottlenecks for large reactive signal graphs:

> 1. **Implicit Row Unification Across Arrow Combinators:** In vinyl-based kernmantle, composing two Rope arrows with different effect sets (r1 \>\>\> r2 or r1 \*\*\* r2) requires explicit relax / weaken calls or brittle RSubset typeclass induction. With polytropos, Rope (row1 \| ρ) kern a b and Rope (row2 \| ρ) kern b c unify their polymorphic row tails via the plugin during (\>\>\>) and (\*\*\*), closing the tail only when entwine binds the final interpreter record.  
> 2. ***O*****(1) Handler Dispatch in entwine:** A polytropos open variant is a strict (Int\#, Any) pair, and a polytropos handler bundle is a single SmallArray\# Any of strand evaluators. When entwine folds over the Rope AST, matching a strand in the mantle against its handler drops from *O*(*N*) Peano GADT traversal to a single indexSmallArray\# instruction.

## **How polytropos Unifies the 4 FRP Strategies**

### **1\. Duality of Open Variants and Open Records for branchSwitch (Strategies 1 & 3\)**

The hardest problem in combining SomeBinEff static analysis with dynamic FRP switching is eliminating opaque continuation functions (c → Rope m k a b) without forcing the programmer into rigid, monolithic sum types.

Because polytropos provides both **row-polymorphic open variants** (Variant r) and **row-polymorphic open records** (Record r) sharing the same canonical plugin-resolved slot ordering, **switching becomes a first-class row duality**:

> * **The Switching Event is an Open Variant:** A running signal arrow emits a Maybe (Variant switchRow) where each label carries that mode's initialization payload c\_i (e.g., \#dock :: DockTarget, \#cruising :: CruiseSpeed).  
> * **The Switch Table is a Dual Open Record of Parameterized Templates:** The handler passed to branchSwitch is a polytropos Record (MapTemplate m k a b switchRow), where each field \#label holds a parameterized Rope m k (c\_i, a) b (Strategy 3's templateSwitch signature).  
> * **Row-Tail Closure Enforces Exhaustiveness:** Just as OCaml narrows and closes polymorphic variants when matched, your plugin closes switchRow at the branchSwitch node once every emitted variant tag has a corresponding template arrow in the record.

`` -- The event Variant and branch Record share the exact same closed row `sw` ``  
`polySwitch`  
  `:: KnownRow sw`  
  `=> Rope mantle (MSF m) a (b, Maybe (Variant sw))`  
  `-> Record (TemplateBranches mantle (MSF m) a b sw)`  
  `-> Rope mantle (MSF m) a b`

**Why this is a breakthrough for FRP:**

> * **100% Static Analysis (SomeBinEff):** Because Record (TemplateBranches ... sw) is backed by a concrete SmallArray\# of Rope ASTs rather than an opaque Haskell function, SomeBinEff simply folds its Applicative analyzer f across the SmallArray\# during entwine. Every reachable mode is statically analyzed pre-flight.  
> * ***O*****(1) Zero-Allocation Epoch Switching:** During entwine, each Rope in the Record is pre-compiled into a target MSF stored at the exact same Int\# index in a SmallArray\# jump table as the Variant sw tag. When the event fires at tick *tk*​, switching modes is a single indexSmallArray\# lookup passing the variant's Any payload into the target (c\_i, a) input channel.

### **2\. Flattening Coalgebraic State Trees into a Single SmallArray\# (Strategy 2\)**

When you extract existential per-tick state (CompiledRope) in standard Haskell, binary arrow combinators (r1 \>\>\> r2, r1 \*\*\* r2, r1 \|\|\| r2) construct a nested binary tree of strict tuples: S \= ((s1, (s2, s3)), (s4, s5)). At 1000 Hz, unpacking and re-allocating that nested tuple tree on every tick creates significant GC pressure and pointer indirection.

With polytropos’s SmallArray\# layout, you can flatten the entire signal graph’s coalgebraic state into a **contiguous register bank**:

> * **Named Row State vs. Anonymous Indexed Slots:** Stateful strands (MealyStrand or Feedback) can either register a named state field in a **state row s\_row** (resolved by polytropos to a static Int\# offset) or allocate an anonymous slot index during entwine.  
> * **Flat Tick Execution:** Instead of threading nested (s1, s2) pairs through the step function, the compiled coalgebra steps a single flat polytropos state record (or SmallMutableArray\# s Any inside ST s / IO for true **zero-allocation in-place mutation** between ticks):

`-- Flat SmallArray#-backed coalgebraic step: O(1) slot access, zero tuple tree churn`  
`data FlatCoalgebra s_row m a b = FlatCoalgebra`  
  `{ initStateRecord :: !(Record s_row)`  
  `, stepFlatRecord  :: !(Record s_row -> a -> m (b, Record s_row))`  
  `}`

### **3\. Row-Polymorphic State Continuity Across Switches (Strategies 2 & 3 Combined)**

Tracking coalgebraic state as a polytropos row record unlocks something that neither Yampa nor Dunai can do ergonomically: **automatic partial state preservation across switches**.

In Yampa and Dunai, internal state is trapped inside opaque coalgebraic closures (SF a b or MSF m a b). When switch swaps from sf1 to sf2, all internal state of sf1 (integrators, low-pass filters, PID accumulators) is destroyed unless you manually hoist that state outside the switch and route it via explicit input/output tuples.

When state is represented as a polytropos open record Record s\_row:

> 1. Suppose branch1 has state row (shared\_filters \| branch1\_only) and branch2 has state row (shared\_filters \| branch2\_only).  
> 2. Because polytropos knows the row decomposition via the typechecker plugin, switching from branch1 to branch2 can **project shared\_filters directly from branch1’s SmallArray\# state record** and merge it with the initial state of branch2\_only\!  
> 3. Your low-pass filters and sensor estimators remain warm across mode switches without polluting the signal arrow’s input/output types a and b.

### **4\. Scoped Row-Tail Closure for Causal Feedback (Strategy 4\)**

In standard ArrowLoop, proc notation scrambles feedback wires into nested unnamed tuples (a, (d1, d2)). By using polytropos’s **fixity and closure of row tails**, a causal Feedback combinator can discharge named feedback states locally:

> * An inner Rope can read from and write to a feedback field \#velEstimate :: Double in a state row ( \#velEstimate :: Double \| s\_tail ).  
> * Wrapping that sub-graph in causalFeedback \#velEstimate initialVel innerRope uses the plugin’s row-closure mechanism to **discharge \#velEstimate from the outer row signature**, leaving only s\_tail visible to the surrounding network while allocating a dedicated slot in the underlying SmallArray\# state vector.  
> * Because the read pulls from tick *t*−1’s slot in the SmallArray\# and the write updates tick *t*’s slot, the step is strictly causal—requiring **zero MonadFix** and zero lazy thunk allocation.

## **Architectural Comparison: vinyl vs. polytropos in Coalgebraic kernmantle**

| Dimension | Stock kernmantle (vinyl) | Naive Coalgebraic kernmantle | polytropos-Backed Coalgebraic kernmantle |
| :---- | :---- | :---- | :---- |
| **Effect Row Composition (\>\>\>, \*\*\*)** | Explicit relax / *O*(*N*2) RSubset constraints | Same vinyl overhead | **Implicit row-tail unification** via GHC plugin |
| **entwine Handler Lookup** | *O*(*N*) Peano CoRec / Rec GADT traversal | *O*(*N*) per epoch | ***O*****(1) indexSmallArray\#** via canonical tag Int\# |
| **Dynamic branchSwitch Dispatch** | Opaque c → Rope breaks SomeBinEff | Linear scan or closed ADT boilerplate | ***O*****(1) jump table** (Variant sw ↔ Record sw duality) |
| **Per-Tick State Representation** | None (one-shot pipeline) | Nested binary tuple tree ((s1, s2), s3) | **Single flat SmallArray\# / SmallMutableArray\#** |
| **State Continuity Across switch** | N/A | Lost on switch (must hoist manually) | **Row projection preserves shared state fields** |
| **Static Analysis (SomeBinEff f)** | Single pipeline only | Blocked at opaque switch closures | **100% whole-graph** (folds SmallArray\# of branches) |

## **Interactive Memory & Type-Flow Explorer: polytropos \+ kernmantle**

Explore how polytropos compiles open effect rows, open variant switch events, and row-typed state into flat SmallArray\# layouts across compile-time unification, entwine staging, and per-tick execution:

---

