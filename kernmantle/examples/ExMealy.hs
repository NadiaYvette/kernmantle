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
Executable verification suite and example for 'Control.Kernmantle.Rope.Mealy'
(Existential State-Lens Extraction, Flat Coalgebra Compilation, and Polytropos Register Bank).
-}
module Main (main) where

import Prelude hiding (id, (.))

import Control.Arrow (Arrow(..), ArrowChoice(..), Kleisli(..), returnA)
import Control.Category (Category(..))
import Data.Functor.Identity (Identity(..), runIdentity)
import Control.Kernmantle.Rope
import Control.Kernmantle.Rope.Internal (pattern RNil)
import Polytropos

-- | Continuous physics state row
type PhysicsRow = '["pos" ':= Double, "vel" ':= Double]

-- | Cruise mode state row
type CruiseRow = '["velocity" ':= Double, "heading" ':= Double]

-- | Approach mode state row
type ApproachRow = '["velocity" ':= Double, "glideslope" ':= Double]

main :: IO ()
main = do
  putStrLn "=== Testing Kernmantle Mealy Machines & Existential State-Lens Extraction ==="

  ------------------------------------------------------------------------------
  -- 1. Pure Discrete Mealy Machine Stepping & Stream Execution
  ------------------------------------------------------------------------------
  putStrLn "\n--- [1/7] Testing Pure Discrete Mealy Stepping & Stream Execution ---"

  let mealyCounter :: MealyEff Identity Int Int
      mealyCounter = mealyPure (0 :: Int) \(st :: Int) (inc :: Int) ->
        let !st' = st + inc in (st', st')

  let pureCoalg = extractMealy mealyCounter

  -- Single step execution
  let (out1, c1) = stepPureCoalgebra pureCoalg 10
  putStrLn $ "  Step 1 (+10): output=" ++ show out1
  if out1 == 10
    then putStrLn "  ✓ Step 1 passed"
    else error $ "Step 1 failed: expected 10, got " ++ show out1

  let (out2, _c2) = stepPureCoalgebra c1 25
  putStrLn $ "  Step 2 (+25): output=" ++ show out2
  if out2 == 35
    then putStrLn "  ✓ Step 2 passed"
    else error $ "Step 2 failed: expected 35, got " ++ show out2

  -- Stream processing over list
  let Identity streamOut = runCoalgebraStream pureCoalg [1, 2, 3, 4, 5]
  putStrLn $ "  runCoalgebraStream [1..5]: " ++ show streamOut
  if streamOut == [1, 3, 6, 10, 15]
    then putStrLn "  ✓ runCoalgebraStream passed"
    else error $ "runCoalgebraStream failed: " ++ show streamOut

  -- Running fold
  let Identity (mLast, _finalC) = foldCoalgebra pureCoalg [10, 20, 30]
  putStrLn $ "  foldCoalgebra [10, 20, 30]: last=" ++ show mLast
  if mLast == Just 60
    then putStrLn "  ✓ foldCoalgebra passed"
    else error $ "foldCoalgebra failed: " ++ show mLast

  ------------------------------------------------------------------------------
  -- 2. Arrow & ArrowChoice Coalgebra Composition ((.) and (+++))
  ------------------------------------------------------------------------------
  putStrLn "\n--- [2/7] Testing Arrow & ArrowChoice Composition ---"

  -- Coalgebra f: adds internal state s to input, increments s
  let f :: FlatCoalgebra Identity Int Int
      f = FlatCoalgebra (10 :: Int) \st x -> Identity (x + st, st + 1)

  -- Coalgebra g: formats string and records previous input
  let g :: FlatCoalgebra Identity Int String
      g = FlatCoalgebra ("init" :: String) \st x ->
        Identity (show x ++ " (prev:" ++ st ++ ")", show x)

  -- Sequential composition: fuses state into (Int, String)
  let h :: FlatCoalgebra Identity Int String
      h = g . f

  let (hOut1, h1) = stepPureCoalgebra h 5
  putStrLn $ "  (g . f) step 1: " ++ hOut1
  let (hOut2, _h2) = stepPureCoalgebra h1 3
  putStrLn $ "  (g . f) step 2: " ++ hOut2
  if hOut1 == "15 (prev:init)" && hOut2 == "14 (prev:15)"
    then putStrLn "  ✓ Sequential coalgebra composition passed"
    else error $ "Sequential composition failed: " ++ show (hOut1, hOut2)

  -- Choice composition: warm accumulator preservation on inactive branch
  let choiceCoalg = pureCoalg +++ f

  let (c1Out, ch1) = stepPureCoalgebra choiceCoalg (Left 10)
  putStrLn $ "  Choice step 1 (Left 10):  " ++ show c1Out
  let (c2Out, ch2) = stepPureCoalgebra ch1 (Right 5)
  putStrLn $ "  Choice step 2 (Right 5): " ++ show c2Out
  let (c3Out, ch3) = stepPureCoalgebra ch2 (Left 20)
  putStrLn $ "  Choice step 3 (Left 20):  " ++ show c3Out
  let (c4Out, _ch4) = stepPureCoalgebra ch3 (Right 7)
  putStrLn $ "  Choice step 4 (Right 7): " ++ show c4Out

  if c1Out == Left 10 && c2Out == Right 15 && c3Out == Left 30 && c4Out == Right 18
    then putStrLn "  ✓ ArrowChoice warm accumulator preservation passed"
    else error $ "ArrowChoice failed: " ++ show (c1Out, c2Out, c3Out, c4Out)

  ------------------------------------------------------------------------------
  -- 3. Existential State-Lens Extraction & Composition
  ------------------------------------------------------------------------------
  putStrLn "\n--- [3/7] Testing Existential State-Lens Extraction & Composition ---"

  -- Lenses into composite pair (Int, String)
  let intLens :: StateLens Int (Int, String)
      intLens = fstLens

      strLens :: StateLens String (Int, String)
      strLens = sndLens

  let testPair = (100 :: Int, "active" :: String)
  putStrLn $ "  Initial composite pair: " ++ show testPair
  let testPair' = lensModify intLens (+ 50) testPair
  putStrLn $ "  lensModify intLens (+50): " ++ show testPair'
  if testPair' == (150, "active")
    then putStrLn "  ✓ StateLens modification passed"
    else error $ "StateLens modification failed: " ++ show testPair'

  -- Nested lens composition: (Double, (Int, String))
  let nestedLens = composeLens intLens sndLens
  let nestedPair = (3.14 :: Double, (42 :: Int, "nested" :: String))
  let nestedGet = lensGet nestedLens nestedPair
  let nestedSet = lensSet nestedLens 999 nestedPair
  putStrLn $ "  Nested lens get: " ++ show nestedGet
  putStrLn $ "  Nested lens set: " ++ show nestedSet
  if nestedGet == 42 && nestedSet == (3.14, (999, "nested"))
    then putStrLn "  ✓ Nested composeLens passed"
    else error $ "composeLens failed: " ++ show (nestedGet, nestedSet)

  -- Embedding step function via coalgWithLens
  let embeddedCoalg :: FlatCoalgebra Identity Int Int
      embeddedCoalg = coalgWithLens intLens (0, "ready") (10 :: Int) \st x ->
        Identity (st + x, st + x)

  let Identity (embedOut, _embFinal) = stepCoalgebra embeddedCoalg 25
  putStrLn $ "  coalgWithLens output: " ++ show embedOut
  if embedOut == 35
    then putStrLn "  ✓ coalgWithLens passed"
    else error $ "coalgWithLens failed: " ++ show embedOut

  ------------------------------------------------------------------------------
  -- 4. Polytropos Record Register Bank Coalgebra (FlatRecordCoalgebra)
  ------------------------------------------------------------------------------
  putStrLn "\n--- [4/7] Testing Polytropos Record Register Bank Coalgebra ---"

  let initialPhysics :: Record PhysicsRow
      initialPhysics = REC { pos = 0.0, vel = 10.0 }

  -- Continuous physics integration step: vel' = vel + acc * dt; pos' = pos + vel' * dt
  let physicsCoalg :: FlatRecordCoalgebra PhysicsRow Identity Double (Double, Double)
      physicsCoalg = FlatRecordCoalgebra initialPhysics \st acc ->
        let dt = 0.1
            v = recordField (Field @"vel") st
            p = recordField (Field @"pos") st
            v' = v + acc * dt
            p' = p + v' * dt
            st' = setField (Field @"pos") p' (setField (Field @"vel") v' st)
        in Identity ((p', v'), st')

  let (phys1, cPhys1) = runIdentity (stepRecordCoalgebra physicsCoalg 2.0)
  putStrLn $ "  Physics Step 1 (acc=2.0): pos=" ++ show (fst phys1) ++ ", vel=" ++ show (snd phys1)
  let (phys2, _cPhys2) = runIdentity (stepRecordCoalgebra cPhys1 0.0)
  putStrLn $ "  Physics Step 2 (acc=0.0): pos=" ++ show (fst phys2) ++ ", vel=" ++ show (snd phys2)

  -- Expected:
  -- Step 1: v' = 10 + 2*0.1 = 10.2; p' = 0 + 10.2*0.1 = 1.02
  -- Step 2: v' = 10.2 + 0 = 10.2;   p' = 1.02 + 10.2*0.1 = 2.04
  if abs (fst phys1 - 1.02) < 1e-6 && abs (snd phys1 - 10.2) < 1e-6 &&
     abs (fst phys2 - 2.04) < 1e-6 && abs (snd phys2 - 10.2) < 1e-6
    then putStrLn "  ✓ FlatRecordCoalgebra physics integration passed"
    else error $ "FlatRecordCoalgebra failed: " ++ show (phys1, phys2)

  -- Record Field StateLens
  let posLens = recordFieldLens #pos
  let velLens = recordFieldLens #vel
  let updatedRec = lensSet posLens 500.0 (lensSet velLens 88.0 initialPhysics)
  putStrLn $ "  recordFieldLens pos=" ++ show (lensGet posLens updatedRec)
          ++ ", vel=" ++ show (lensGet velLens updatedRec)
  if lensGet posLens updatedRec == 500.0 && lensGet velLens updatedRec == 88.0
    then putStrLn "  ✓ recordFieldLens passed"
    else error $ "recordFieldLens failed: " ++ show updatedRec

  ------------------------------------------------------------------------------
  -- 5. Row-Polymorphic State Continuity across Mode Switches
  ------------------------------------------------------------------------------
  putStrLn "\n--- [5/7] Testing Row-Polymorphic State Continuity across Mode Switches ---"

  let cruiseRec :: Record CruiseRow
      cruiseRec = REC { velocity = 240.0, heading = 180.0 }

  let approachInitRec :: Record ApproachRow
      approachInitRec = REC { velocity = 0.0, glideslope = 3.0 }

  -- Mode switch: preserve velocity across transition from Cruise to Approach
  let approachWarmRec = copyFieldState #velocity cruiseRec approachInitRec
  let warmVel = recordField (Field @"velocity") approachWarmRec
  let warmSlope = recordField (Field @"glideslope") approachWarmRec

  putStrLn $ "  Exiting CruiseMode:   vel=" ++ show (recordField (Field @"velocity") cruiseRec)
  putStrLn $ "  Entering ApproachMode: vel=" ++ show warmVel ++ ", glideslope=" ++ show warmSlope

  if warmVel == 240.0 && warmSlope == 3.0
    then putStrLn "  ✓ copyFieldState row continuity passed"
    else error $ "copyFieldState failed: " ++ show (warmVel, warmSlope)

  ------------------------------------------------------------------------------
  -- 6. Monadic IO Execution (interpMealyIO & interpCoalgebraIO)
  ------------------------------------------------------------------------------
  putStrLn "\n--- [6/7] Testing Monadic IO Execution (interpMealyIO & interpCoalgebraIO) ---"

  let ioMealy :: MealyEff IO Int Int
      ioMealy = MealyStrand (1000 :: Int) \st x -> do
        let !st' = st + x
        pure (st', st')

  ioArrow <- interpMealyIO ioMealy
  r1 <- runKleisli ioArrow 50
  putStrLn $ "  IO Mealy tick 1 (+50):  " ++ show r1
  r2 <- runKleisli ioArrow 100
  putStrLn $ "  IO Mealy tick 2 (+100): " ++ show r2

  if r1 == 1050 && r2 == 1150
    then putStrLn "  ✓ interpMealyIO strict IO execution passed"
    else error $ "interpMealyIO failed: " ++ show (r1, r2)

  let ioCoalg = extractMealy ioMealy
  coalgIO <- interpCoalgebraIO ioCoalg
  c1Res <- runKleisli coalgIO 25
  putStrLn $ "  IO Coalgebra tick 1 (+25): " ++ show c1Res
  if c1Res == 1025
    then putStrLn "  ✓ interpCoalgebraIO passed"
    else error $ "interpCoalgebraIO failed: " ++ show c1Res

  ------------------------------------------------------------------------------
  -- 7. Conversion Bridges between MealyEff, FlatCoalgebra, & FeedbackEff
  ------------------------------------------------------------------------------
  putStrLn "\n--- [7/7] Testing Conversion Bridges between MealyEff, FlatCoalgebra, & FeedbackEff ---"

  let mealySample :: MealyEff IO Int Int
      mealySample = MealyStrand (5 :: Int) \st x -> pure (st * x, st + x)

  let fbBridge = mealyToFeedback mealySample
  let coalgBridge = feedbackToCoalgebra fbBridge
  let mealyBack = feedbackToMealy (coalgebraToFeedback coalgBridge)

  bridgeArrow <- interpMealyIO mealyBack
  b1 <- runKleisli bridgeArrow 4
  putStrLn $ "  Round-trip converted tick 1 (x=4, s=5): out=" ++ show b1
  b2 <- runKleisli bridgeArrow 2
  putStrLn $ "  Round-trip converted tick 2 (x=2, s=9): out=" ++ show b2

  -- Tick 1: out = 5*4 = 20, s' = 5+4 = 9
  -- Tick 2: out = 9*2 = 18, s' = 9+2 = 11
  if b1 == 20 && b2 == 18
    then putStrLn "  ✓ Conversion bridges round-trip passed"
    else error $ "Conversion bridges failed: " ++ show (b1, b2)

  putStrLn "\n=== ALL 7 MEALY & STATE-LENS EXTRACTION TESTS PASSED SUCCESSFULLY! ==="
