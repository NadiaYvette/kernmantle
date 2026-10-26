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
Executable verification suite and example for 'Control.Kernmantle.Rope.MSF'
(Dunai / Rhine Monadic Stream Function Adapter, Coalgebra Compilation, and Continuous Time-Domain).
-}
module Main (main) where

import Prelude hiding (id, (.))

import Control.Arrow (Arrow(..), ArrowChoice(..), Kleisli(..), returnA)
import Control.Category (Category(..))
import Data.Functor.Identity (Identity(..), runIdentity)
import Control.Kernmantle.FRP
import Control.Kernmantle.Rope.Internal (pattern RNil)
import Polytropos

-- | Flight modes sum row for polySwitchMSF
type FlightModes =
  '[ "docking"  ':= '["target" ':= String]
   , "cruising" ':= '["speed" ':= Int]
   ]

main :: IO ()
main = do
  putStrLn "=== Testing Kernmantle MSF (Dunai / Rhine Adapter) ==="

  ------------------------------------------------------------------------------
  -- 1. Pure MSF Execution & Stream Stepping
  ------------------------------------------------------------------------------
  putStrLn "\n--- [1/7] Testing Pure MSF Execution & Stream Stepping ---"

  let counterMSF :: MSF Identity Int Int
      counterMSF = feedbackMSF (0 :: Int) $ arr \(x, !s) ->
        let !s' = s + x in (s', s')

  -- Single step execution
  let (out1, msf1) = stepPureMSF counterMSF 10
  putStrLn $ "  Step 1 (+10): output=" ++ show out1
  let (out2, _msf2) = stepPureMSF msf1 25
  putStrLn $ "  Step 2 (+25): output=" ++ show out2
  if out1 == 10 && out2 == 35
    then putStrLn "  ✓ stepPureMSF passed"
    else error $ "stepPureMSF failed: " ++ show (out1, out2)

  -- Stream execution
  let Identity streamOut = runMSFStream counterMSF [1, 2, 3, 4, 5]
  putStrLn $ "  runMSFStream [1..5]: " ++ show streamOut
  if streamOut == [1, 3, 6, 10, 15]
    then putStrLn "  ✓ runMSFStream passed"
    else error $ "runMSFStream failed: " ++ show streamOut

  -- Fold execution
  let Identity (mLast, _msfFold) = foldMSF counterMSF [10, 20, 30]
  putStrLn $ "  foldMSF [10, 20, 30]: last=" ++ show mLast
  if mLast == Just 60
    then putStrLn "  ✓ foldMSF passed"
    else error $ "foldMSF failed: " ++ show mLast

  ------------------------------------------------------------------------------
  -- 2. Arrow & ArrowChoice Composition with Warm State Preservation
  ------------------------------------------------------------------------------
  putStrLn "\n--- [2/7] Testing Arrow & ArrowChoice Composition ---"

  -- Signal processor f: adds step counter s to input, increments s
  let f :: MSF Identity Int Int
      f = feedbackMSF (10 :: Int) $ arr \(x, !s) -> (x + s, s + 1)

  -- Signal processor g: formats string with current count
  let g :: MSF Identity Int String
      g = feedbackMSF (1 :: Int) $ arr \(x, !tick) ->
        ("Tick#" ++ show tick ++ ":" ++ show x, tick + 1)

  -- Sequential composition: g . f
  let h :: MSF Identity Int String
      h = g . f

  let (hOut1, h1) = stepPureMSF h 5
  putStrLn $ "  (g . f) step 1: " ++ hOut1
  let (hOut2, _h2) = stepPureMSF h1 3
  putStrLn $ "  (g . f) step 2: " ++ hOut2
  -- Step 1: f(5, 10) -> (15, 11); g(15, 1) -> ("Tick#1:15", 2)
  -- Step 2: f(3, 11) -> (14, 12); g(14, 2) -> ("Tick#2:14", 3)
  if hOut1 == "Tick#1:15" && hOut2 == "Tick#2:14"
    then putStrLn "  ✓ Sequential MSF composition passed"
    else error $ "Sequential MSF failed: " ++ show (hOut1, hOut2)

  -- Choice composition: warm state preservation on inactive branch
  let choiceMSF = counterMSF +++ f

  let (c1, ch1) = stepPureMSF choiceMSF (Left 10)
  putStrLn $ "  Choice step 1 (Left 10):  " ++ show c1
  let (c2, ch2) = stepPureMSF ch1 (Right 5)
  putStrLn $ "  Choice step 2 (Right 5): " ++ show c2
  let (c3, ch3) = stepPureMSF ch2 (Left 20)
  putStrLn $ "  Choice step 3 (Left 20):  " ++ show c3
  let (c4, _ch4) = stepPureMSF ch3 (Right 7)
  putStrLn $ "  Choice step 4 (Right 7): " ++ show c4

  -- Left resumed from 10 -> 30; Right resumed from 11 -> 18!
  if c1 == Left 10 && c2 == Right 15 && c3 == Left 30 && c4 == Right 18
    then putStrLn "  ✓ ArrowChoice warm state preservation passed"
    else error $ "ArrowChoice failed: " ++ show (c1, c2, c3, c4)

  ------------------------------------------------------------------------------
  -- 3. Causal Feedback Loop (feedbackMSF)
  ------------------------------------------------------------------------------
  putStrLn "\n--- [3/7] Testing Causal Feedback Loop (feedbackMSF in IO) ---"

  let ioFeedback :: MSF IO Int Int
      ioFeedback = feedbackMSF (100 :: Int) $ arr \(inc, !st) ->
        let !st' = st + inc in (st', st')

  ioRes1 <- stepMSF ioFeedback 25
  putStrLn $ "  IO Feedback tick 1 (+25): " ++ show (fst ioRes1)
  ioRes2 <- stepMSF (snd ioRes1) 50
  putStrLn $ "  IO Feedback tick 2 (+50): " ++ show (fst ioRes2)

  if fst ioRes1 == 125 && fst ioRes2 == 175
    then putStrLn "  ✓ feedbackMSF in IO passed"
    else error $ "feedbackMSF failed: " ++ show (fst ioRes1, fst ioRes2)

  ------------------------------------------------------------------------------
  -- 4. Compilation Bridges from Rope & Coalgebra to MSF
  ------------------------------------------------------------------------------
  putStrLn "\n--- [4/7] Testing Compilation Bridges from Rope & Coalgebra to MSF ---"

  -- Stateless Rope to MSF
  let pureRope :: TightRope '[] (Kleisli IO) Int String
      pureRope = mkRope \_ -> Kleisli \x -> pure ("RopeVal=" ++ show (x * 2))

  let msfFromRope = compileRopeToMSF pureRope
  (ropeOut, _) <- stepMSF msfFromRope 21
  putStrLn $ "  compileRopeToMSF output: " ++ ropeOut
  if ropeOut == "RopeVal=42"
    then putStrLn "  ✓ compileRopeToMSF passed"
    else error $ "compileRopeToMSF failed: " ++ ropeOut

  -- Stateful Causal Rope to MSF
  let causalRope :: TightRope '[] (Kleisli IO) (Int, Int) (Int, Int)
      causalRope = mkRope \_ -> Kleisli \(x, !s) -> do
        let !s' = s + x
        pure (s', s')

  let msfFromCausalRope = compileCausalRopeToMSF (0 :: Int) causalRope
  causalOutputs <- runMSFStream msfFromCausalRope [5, 10, 20]
  putStrLn $ "  compileCausalRopeToMSF outputs: " ++ show causalOutputs
  if causalOutputs == [5, 15, 35]
    then putStrLn "  ✓ compileCausalRopeToMSF passed"
    else error $ "compileCausalRopeToMSF failed: " ++ show causalOutputs

  -- FlatCoalgebra to MSF and back
  let coalgSample = FlatCoalgebra (10 :: Int) \st x -> Identity (st * x, st + x)
  let msfFromCoalg = coalgToMSF coalgSample
  let roundTripCoalg = msfToCoalgebra msfFromCoalg

  let Identity (rtOut, _) = stepCoalgebra roundTripCoalg 4
  putStrLn $ "  Coalgebra <-> MSF round-trip: " ++ show rtOut
  if rtOut == 40
    then putStrLn "  ✓ Coalgebra <-> MSF round-trip passed"
    else error $ "Round-trip failed: " ++ show rtOut

  ------------------------------------------------------------------------------
  -- 5. Continuous Time-Domain & Rhine Primitives (TimedMSF)
  ------------------------------------------------------------------------------
  putStrLn "\n--- [5/7] Testing Rhine Continuous Time-Domain Primitives ---"

  -- A. Numerical Integration: constant velocity 20.0 m/s with dt = 0.1s
  let posIntegrator :: TimedMSF Identity Double Double Double
      posIntegrator = integrate (0.0 :: Double)

  -- Inputs: (velocity, dt)
  let dt = 0.1
  let Identity posTrajectory = runMSFStream posIntegrator
        [ (20.0, dt), (20.0, dt), (20.0, dt) ]
  putStrLn $ "  Integrated positions (v=20, dt=0.1): " ++ show posTrajectory
  -- Expected: [2.0, 4.0, 6.0]
  if all (\(calc, expVal) -> abs (calc - expVal) < 1e-6) (zip posTrajectory [2.0, 4.0, 6.0])
    then putStrLn "  ✓ TimedMSF integrate passed"
    else error $ "integrate failed: " ++ show posTrajectory

  -- B. Numerical Derivative: positions [0.0, 2.0, 6.0, 12.0] at dt = 1.0s
  let velEstimator :: TimedMSF Identity Double Double Double
      velEstimator = derivative (0.0 :: Double)

  let Identity velTrajectory = runMSFStream velEstimator
        [ (2.0, 1.0), (6.0, 1.0), (12.0, 1.0) ]
  putStrLn $ "  Estimated velocities (dt=1.0): " ++ show velTrajectory
  -- Expected: [(2-0)/1 = 2.0, (6-2)/1 = 4.0, (12-6)/1 = 6.0]
  if all (\(calc, expVal) -> abs (calc - expVal) < 1e-6) (zip velTrajectory [2.0, 4.0, 6.0])
    then putStrLn "  ✓ TimedMSF derivative passed"
    else error $ "derivative failed: " ++ show velTrajectory

  -- C. First-Order Low-Pass Filter: tau = 1.0s, step input 100.0, dt = 0.5s
  let filterMSF :: TimedMSF Identity Double Double Double
      filterMSF = lowPass (1.0 :: Double) 0.0

  -- alpha = dt / (tau + dt) = 0.5 / 1.5 = 1/3
  -- Step 1: y1 = 0 + (1/3)*(100 - 0) = 33.3333
  -- Step 2: y2 = 33.3333 + (1/3)*(100 - 33.3333) = 55.5555
  let Identity (filt1, fMSF1) = stepMSF filterMSF (100.0, 0.5)
  let Identity (filt2, _fMSF2) = stepMSF fMSF1 (100.0, 0.5)
  putStrLn $ "  Low-pass filter step 1: " ++ show filt1 ++ ", step 2: " ++ show filt2
  if abs (filt1 - 33.333333) < 1e-4 && abs (filt2 - 55.555555) < 1e-4
    then putStrLn "  ✓ TimedMSF lowPass passed"
    else error $ "lowPass failed: " ++ show (filt1, filt2)

  ------------------------------------------------------------------------------
  -- 6. Dunai-Style Epoch Switching (epochSwitch)
  ------------------------------------------------------------------------------
  putStrLn "\n--- [6/7] Testing Dunai-Style Epoch Switching (epochSwitch) ---"

  -- Initial mode: counts ticks until tick >= 3, then emits Just "THRESHOLD_REACHED"
  let initialMode :: MSF Identity Int (String, Maybe String)
      initialMode = feedbackMSF (1 :: Int) $ arr \(inp, !tick) ->
        let !msg = "ModeA: inp=" ++ show inp ++ ", tick=" ++ show tick
            !mEv = if tick >= 3 then Just "THRESHOLD_REACHED" else Nothing
        in ((msg, mEv), tick + 1)

  -- Continuation mode: entered once event fires
  let continuationMode :: String -> MSF Identity Int String
      continuationMode evMsg = arr \inp -> "ModeB[" ++ evMsg ++ "]: inp=" ++ show inp

  let switchedEpoch = epochSwitch initialMode continuationMode

  let Identity epochOutputs = runMSFStream switchedEpoch [10, 20, 30, 40, 50]
  putStrLn $ "  Epoch-switched outputs: " ++ show epochOutputs
  -- Tick 1: ModeA (tick=1, Nothing) -> ModeA: inp=10, tick=1
  -- Tick 2: ModeA (tick=2, Nothing) -> ModeA: inp=20, tick=2
  -- Tick 3: ModeA (tick=3, Just)    -> ModeA: inp=30, tick=3 (event triggers switch!)
  -- Tick 4: ModeB (continuation)   -> ModeB[THRESHOLD_REACHED]: inp=40
  -- Tick 5: ModeB (continuation)   -> ModeB[THRESHOLD_REACHED]: inp=50
  if epochOutputs == [ "ModeA: inp=10, tick=1"
                     , "ModeA: inp=20, tick=2"
                     , "ModeA: inp=30, tick=3"
                     , "ModeB[THRESHOLD_REACHED]: inp=40"
                     , "ModeB[THRESHOLD_REACHED]: inp=50"
                     ]
    then putStrLn "  ✓ epochSwitch passed"
    else error $ "epochSwitch failed: " ++ show epochOutputs

  ------------------------------------------------------------------------------
  -- 7. Polytropos Open Variant Switching (polySwitchMSF)
  ------------------------------------------------------------------------------
  putStrLn "\n--- [7/7] Testing Polytropos Open Variant Switching (polySwitchMSF) ---"

  -- Initial steady state: emits Nothing on inp <= 2, otherwise triggers variant switch
  let initNavMSF :: MSF Identity Int (String, Maybe (Variant FlightModes))
      initNavMSF = arr \cmd -> case cmd of
        1 -> ("Navigating: steady cruise", Nothing)
        2 -> ("Navigating: steady cruise", Nothing)
        3 -> ("Switching to docking...", Just (injectVariant (Field @"docking") (REC { target = "ISS-Alpha" })))
        _ -> ("Switching to cruising...", Just (injectVariant (Field @"cruising") (REC { speed = 500 })))

  -- Template jump table: maps variant constructors to target MSFs receiving (Record fs, a)
  let templates :: Record (TemplateBranchesCore (MSF Identity) Int String FlightModes)
      templates = REC
        { docking = arr \(REC { target = tgt }, inp) ->
            "Docking Engaged: target=" ++ tgt ++ ", cmd=" ++ show inp
        , cruising = arr \(REC { speed = spd }, inp) ->
            "Cruising Engaged: speed=" ++ show spd ++ " knots, cmd=" ++ show inp
        }

  let polySwitchedMSF = polySwitchMSF @FlightModes initNavMSF templates

  let Identity polyOutputs = runMSFStream polySwitchedMSF [1, 2, 3, 4]
  putStrLn $ "  polySwitchMSF outputs: " ++ show polyOutputs

  -- Tick 1 (cmd=1): Navigating: steady cruise
  -- Tick 2 (cmd=2): Navigating: steady cruise
  -- Tick 3 (cmd=3): Docking Engaged: target=ISS-Alpha, cmd=3
  -- Tick 4 (cmd=4): Docking Engaged: target=ISS-Alpha, cmd=4 (continues in docking mode)
  if polyOutputs == [ "Navigating: steady cruise"
                    , "Navigating: steady cruise"
                    , "Docking Engaged: target=ISS-Alpha, cmd=3"
                    , "Docking Engaged: target=ISS-Alpha, cmd=4"
                    ]
    then putStrLn "  ✓ polySwitchMSF O(1) jump table passed"
    else error $ "polySwitchMSF failed: " ++ show polyOutputs

  putStrLn "\n=== ALL 7 MSF & DUNAI/RHINE ADAPTER TESTS PASSED SUCCESSFULLY! ==="
