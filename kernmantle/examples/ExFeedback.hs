{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
Executable verification suite and example for 'Control.Kernmantle.Rope.Feedback'
(Causal Feedback Wire, Causal Arrow Transformer, and Streaming Combinators).
-}
module Main (main) where

import Prelude hiding (id, (.))

import Control.Arrow (Arrow(..), ArrowChoice(..), Kleisli(..), returnA)
import Control.Category (Category(..))
import Data.Function ((&))
import Control.Kernmantle.Rope
import Control.Kernmantle.Rope.Internal (pattern RNil)
import Polytropos


main :: IO ()
main = do
  putStrLn "=== Testing Kernmantle Causal Feedback Strand & Causal Arrows ==="

  ------------------------------------------------------------------------------
  -- 1. Pure Single-Step Causal Execution (stepCausal)
  ------------------------------------------------------------------------------
  putStrLn "\n--- [1/7] Testing Pure Single-Step Causal Execution (stepCausal) ---"

  -- Accumulator counter: internal state is running sum, emits running sum
  let counter :: Causal (->) Int Int
      counter = Causal (0 :: Int) \(inc, !st) ->
        let !st' = st + inc
        in (st', st')

  let (out1, c1) = stepCausal counter 10
  putStrLn $ "  Step 1 (+10): output=" ++ show out1
  if out1 == 10
    then putStrLn "  ✓ Step 1 passed"
    else error $ "Step 1 failed: expected 10, got " ++ show out1

  let (out2, c2) = stepCausal c1 25
  putStrLn $ "  Step 2 (+25): output=" ++ show out2
  if out2 == 35
    then putStrLn "  ✓ Step 2 passed"
    else error $ "Step 2 failed: expected 35, got " ++ show out2

  let (out3, _c3) = stepCausal c2 (-5)
  putStrLn $ "  Step 3 (-5):  output=" ++ show out3
  if out3 == 30
    then putStrLn "  ✓ Step 3 passed"
    else error $ "Step 3 failed: expected 30, got " ++ show out3

  ------------------------------------------------------------------------------
  -- 2. Stream Processing with causalLoopA & causalLoopRope
  ------------------------------------------------------------------------------
  putStrLn "\n--- [2/7] Testing Stream Processing with causalLoopA & causalLoopRope ---"

  -- Running cumulative sum over list
  let cumSumA :: [Int] -> [Int]
      cumSumA = causalLoopA (0 :: Int) (arr \(x, !s) -> let !s' = s + x in (s', s'))

  let resA = cumSumA [1, 2, 3, 4, 5]
  putStrLn $ "  causalLoopA [1..5]: " ++ show resA
  if resA == [1, 3, 6, 10, 15]
    then putStrLn "  ✓ causalLoopA passed"
    else error $ "causalLoopA failed: " ++ show resA

  -- Inside a TightRope
  let stepRope :: TightRope '[] (->) (Int, Int) (Int, Int)
      stepRope = mkRope \_ (x, !s) ->
        let !s' = s + x
        in (s', s')

  let cumSumRope :: TightRope '[] (->) [Int] [Int]
      cumSumRope = causalLoopRope (0 :: Int) stepRope

  let resRope = runRope cumSumRope RNil [1, 2, 3, 4, 5]
  putStrLn $ "  causalLoopRope [1..5]: " ++ show resRope
  if resRope == [1, 3, 6, 10, 15]
    then putStrLn "  ✓ causalLoopRope passed"
    else error $ "causalLoopRope failed: " ++ show resRope

  ------------------------------------------------------------------------------
  -- 3. Running Fold with causalFold
  ------------------------------------------------------------------------------
  putStrLn "\n--- [3/7] Testing Running Fold with causalFold ---"

  let runningFold = causalFold (0 :: Int) (arr \(x, !s) -> let !s' = s + x in (s', s'))

  let (lastOut, finalSt) = runningFold [10, 20, 30]
  putStrLn $ "  causalFold [10, 20, 30]: last=" ++ show lastOut ++ ", finalState=" ++ show finalSt
  if lastOut == Just 60 && finalSt == 60
    then putStrLn "  ✓ causalFold non-empty passed"
    else error $ "causalFold non-empty failed: " ++ show (lastOut, finalSt)

  let (emptyOut, emptySt) = runningFold []
  putStrLn $ "  causalFold []:            last=" ++ show emptyOut ++ ", finalState=" ++ show emptySt
  if emptyOut == Nothing && emptySt == 0
    then putStrLn "  ✓ causalFold empty passed"
    else error $ "causalFold empty failed: " ++ show (emptyOut, emptySt)

  ------------------------------------------------------------------------------
  -- 4. Arrow Composition & State Pair Fusing ((.) and (***))
  ------------------------------------------------------------------------------
  putStrLn "\n--- [4/7] Testing Arrow Composition & State Pair Fusing ((.) and (***)) ---"

  -- Arrow 1: f adds current state to input, and increments state
  let f :: Causal (->) Int Int
      f = Causal (10 :: Int) (arr \(x, !s) -> (x + s, s + 1))

  -- Arrow 2: g formats output as string and stores previous input
  let g :: Causal (->) Int String
      g = Causal ("init" :: String) (arr \(x, !s) -> (show x ++ " (prev:" ++ s ++ ")", show x))

  -- Sequential composition: fuses state into (Int, String)
  let h :: Causal (->) Int String
      h = g . f

  let (hOut1, h1) = stepCausal h 5
  -- Step 1: f(5, 10) -> (15, 11); g(15, "init") -> ("15 (prev:init)", "15")
  putStrLn $ "  (g . f) step 1: " ++ hOut1
  if hOut1 == "15 (prev:init)"
    then putStrLn "  ✓ Sequential step 1 passed"
    else error $ "Sequential step 1 failed: " ++ hOut1

  let (hOut2, _h2) = stepCausal h1 3
  -- Step 2: f(3, 11) -> (14, 12); g(14, "15") -> ("14 (prev:15)", "14")
  putStrLn $ "  (g . f) step 2: " ++ hOut2
  if hOut2 == "14 (prev:15)"
    then putStrLn "  ✓ Sequential step 2 passed"
    else error $ "Sequential step 2 failed: " ++ hOut2

  -- Parallel composition: fuses state into (Int, Int)
  let p :: Causal (->) (Int, Int) (Int, Int)
      p = counter *** f

  let ((pOutA, pOutB), _p1) = stepCausal p (50, 20)
  -- counter(50, 0) -> (50, 50); f(20, 10) -> (30, 11)
  putStrLn $ "  (counter *** f): (" ++ show pOutA ++ ", " ++ show pOutB ++ ")"
  if pOutA == 50 && pOutB == 30
    then putStrLn "  ✓ Parallel composition passed"
    else error $ "Parallel composition failed: " ++ show (pOutA, pOutB)

  ------------------------------------------------------------------------------
  -- 5. ArrowChoice ((+++)) Warm Accumulator Preservation across Mode Switches
  ------------------------------------------------------------------------------
  putStrLn "\n--- [5/7] Testing ArrowChoice ((+++)) Warm Accumulator Preservation ---"

  -- Choice between counter (Left) and f (Right)
  let branched = counter +++ f

  -- Step 1: Left 10 -> counter: s 0 -> 10, out 10. f remains cold (s = 10).
  let (cOut1, br1) = stepCausal branched (Left 10)
  putStrLn $ "  Branch Step 1 (Left 10):   " ++ show cOut1
  if cOut1 == Left 10
    then putStrLn "  ✓ Branch step 1 passed"
    else error $ "Branch step 1 failed: " ++ show cOut1

  -- Step 2: Right 5 -> f: s 10 -> 11, out 5+10=15. counter state kept warm (s = 10).
  let (cOut2, br2) = stepCausal br1 (Right 5)
  putStrLn $ "  Branch Step 2 (Right 5):  " ++ show cOut2
  if cOut2 == Right 15
    then putStrLn "  ✓ Branch step 2 passed"
    else error $ "Branch step 2 failed: " ++ show cOut2

  -- Step 3: Left 20 -> counter resumed from 10: s 10 -> 30, out 30!
  let (cOut3, br3) = stepCausal br2 (Left 20)
  putStrLn $ "  Branch Step 3 (Left 20):   " ++ show cOut3
  if cOut3 == Left 30
    then putStrLn "  ✓ Branch step 3 (counter preserved) passed"
    else error $ "Branch step 3 failed: " ++ show cOut3

  -- Step 4: Right 7 -> f resumed from 11: s 11 -> 12, out 7+11=18!
  let (cOut4, _br4) = stepCausal br3 (Right 7)
  putStrLn $ "  Branch Step 4 (Right 7):  " ++ show cOut4
  if cOut4 == Right 18
    then putStrLn "  ✓ Branch step 4 (f preserved) passed"
    else error $ "Branch step 4 failed: " ++ show cOut4

  ------------------------------------------------------------------------------
  -- 6. Strict IO Execution without MonadFix / <<loop>> Blackholes
  ------------------------------------------------------------------------------
  putStrLn "\n--- [6/7] Testing Effectful IO Execution without MonadFix (newCausalIO & interpFeedbackIO) ---"

  -- Note: A strict step function in ArrowLoop would crash with <<loop>> under MonadFix.
  -- Here, state is strictly evaluated at each discrete tick.
  let ioCausal :: Causal (Kleisli IO) Int Int
      ioCausal = Causal (100 :: Int) $ Kleisli \(inc, !st) -> do
        let !st' = st + inc
        pure (st', st')

  causalArrow <- newCausalIO ioCausal

  ioRes1 <- runKleisli causalArrow 15
  putStrLn $ "  IO Step 1 (+15 from 100): " ++ show ioRes1
  if ioRes1 == 115
    then putStrLn "  ✓ IO Step 1 passed"
    else error $ "IO Step 1 failed: " ++ show ioRes1

  ioRes2 <- runKleisli causalArrow 35
  putStrLn $ "  IO Step 2 (+35 from 115): " ++ show ioRes2
  if ioRes2 == 150
    then putStrLn "  ✓ IO Step 2 passed"
    else error $ "IO Step 2 failed: " ++ show ioRes2

  -- Also test interpFeedbackIO with a FeedbackEff strand
  let fbEff :: FeedbackEff (Kleisli IO) Int String
      fbEff = FeedbackEff (1 :: Int) $ Kleisli \(val, !tick) -> do
        let !msg = "Tick #" ++ show tick ++ ": val=" ++ show val
        pure (msg, tick + 1)

  fbArrow <- interpFeedbackIO id fbEff

  fb1 <- runKleisli fbArrow 42
  putStrLn $ "  FeedbackEff 1: " ++ fb1
  fb2 <- runKleisli fbArrow 99
  putStrLn $ "  FeedbackEff 2: " ++ fb2
  if fb1 == "Tick #1: val=42" && fb2 == "Tick #2: val=99"
    then putStrLn "  ✓ interpFeedbackIO passed"
    else error $ "interpFeedbackIO failed: " ++ show (fb1, fb2)

  ------------------------------------------------------------------------------
  -- 7. Weaving Feedback with Other Rope Strands in a Stream
  ------------------------------------------------------------------------------
  putStrLn "\n--- [7/7] Testing causalLoopRope Weaving with External Strands ---"

  -- A Rope step that uses a logging strand and causal feedback state
  -- The mantle has a single strand: #logger
  let stepLoggedRope
        :: TightRope '[ '("logger", Kleisli IO) ] (Kleisli IO) (Int, Int) (Int, Int)
      stepLoggedRope = proc (inp, st) -> do
        let !st' = st + inp
        strand #logger (Kleisli putStrLn) -< "Logging tick: input=" ++ show inp ++ ", state=" ++ show st'
        returnA -< (st', st')

  let streamLoggedRope = causalLoopRope (0 :: Int) (loosen stepLoggedRope)
      streamArrow = streamLoggedRope
        & weave' #logger id
        & untwine

  loggedOutputs <- runKleisli streamArrow [5, 15, 30]
  putStrLn $ "  Logged stream outputs: " ++ show loggedOutputs
  if loggedOutputs == [5, 20, 50]
    then putStrLn "  ✓ causalLoopRope with external strand passed"
    else error $ "causalLoopRope with external strand failed: " ++ show loggedOutputs

  putStrLn "\n=== ALL 7 CAUSAL FEEDBACK TESTS PASSED SUCCESSFULLY! ==="
