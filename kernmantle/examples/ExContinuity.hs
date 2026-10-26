{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Main
Description : Executable verification suite for Control.Kernmantle.Rope.Continuity
Copyright   : (c) 2026 The Kernmantle Authors
License     : BSD-3-Clause

Verifies all aspects of Tier 3.1: Row-Polymorphic State Continuity across Mode Switches:
1. Vectorized Shared State Transfer (transferSharedState)
2. Shared Record Injection & Projection (injectSharedRecord & projectSharedState)
3. Pair-wise Continuous Mode Switch (switchContinuous2)
4. N-ary Open Variant Continuous Switching (continuousMSF - Drone Flight Controller)
5. Continuous Telemetry Stream Observation (continuousTelemetryMSF)
6. Multi-Transition Round-Trip State Stability (Mode A <-> Mode B)
7. Coalgebra State Warm-Start (transferCoalgebraState)
-}
module Main (main) where

import Control.Arrow
import Control.Category
import Control.Monad (when)
import Data.Functor.Identity (Identity(..))
import GHC.TypeLits (Symbol)
import System.Exit (exitFailure)

import Prelude hiding (id, (.))

import Control.Kernmantle.Rope
import Polytropos
  ( Record
  , Variant
  , emptyRecord
  , insertField
  , setField
  , recordField
  , injectVariant
  , Field(..)
  )
import Polytropos.Core
  ( SubRow(..)
  , Row
  , SumRow
  , Pair(..)
  )

-- | Test assertion helper.
assert :: String -> Bool -> IO ()
assert name True  = putStrLn $ "  [PASS] " ++ name
assert name False = do
  putStrLn $ "  [FAIL] " ++ name
  exitFailure

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq name actual expected = do
  if actual == expected
    then putStrLn $ "  [PASS] " ++ name
    else do
      putStrLn $ "  [FAIL] " ++ name
      putStrLn $ "    Expected: " ++ show expected
      putStrLn $ "    Actual:   " ++ show actual
      exitFailure

assertApprox :: String -> Double -> Double -> Double -> IO ()
assertApprox name actual expected tol = do
  let diff = abs (actual - expected)
  if diff <= tol
    then putStrLn $ "  [PASS] " ++ name ++ " (" ++ show actual ++ " ~= " ++ show expected ++ ")"
    else do
      putStrLn $ "  [FAIL] " ++ name
      putStrLn $ "    Expected: " ++ show expected
      putStrLn $ "    Actual:   " ++ show actual
      putStrLn $ "    Diff:     " ++ show diff
      exitFailure

{-------------------------------------------------------------------------------
  Row Definitions for Flight Simulation Tests
-------------------------------------------------------------------------------}

type SharedFlight =
  '[ "alt"  ':= Double
   , "vel"  ':= Double
   , "dist" ':= Double
   ]

type TakeoffState =
  '[ "alt"      ':= Double
   , "vel"      ':= Double
   , "dist"     ':= Double
   , "throttle" ':= Double
   ]

type CruiseState =
  '[ "alt"         ':= Double
   , "vel"         ':= Double
   , "dist"         ':= Double
   , "pidIntegral" ':= Double
   , "waypoint"    ':= Int
   ]

type LandState =
  '[ "alt"       ':= Double
   , "vel"       ':= Double
   , "dist"      ':= Double
   , "touchdown" ':= Bool
   ]

type FlightModes =
  '[ "Takeoff" ':= '[]
   , "Cruise"  ':= '[ "targetAlt" ':= Double ]
   , "Land"    ':= '[ "descentRate" ':= Double ]
   ]

{-------------------------------------------------------------------------------
  Test Suite 1: Vectorized Shared State Transfer
-------------------------------------------------------------------------------}

testSuite1 :: IO ()
testSuite1 = do
  putStrLn "--- Test Suite 1: Vectorized Shared State Transfer (transferSharedState) ---"
  let src :: Record TakeoffState
      src = insertField (Field @"alt") (12.5 :: Double)
          $ insertField (Field @"vel") (4.2 :: Double)
          $ insertField (Field @"dist") (15.0 :: Double)
          $ insertField (Field @"throttle") (0.85 :: Double)
          $ emptyRecord

      dstInit :: Record CruiseState
      dstInit = insertField (Field @"alt") (0.0 :: Double)
              $ insertField (Field @"vel") (0.0 :: Double)
              $ insertField (Field @"dist") (0.0 :: Double)
              $ insertField (Field @"pidIntegral") (1.75 :: Double)
              $ insertField (Field @"waypoint") (3 :: Int)
              $ emptyRecord

      dstWarm :: Record CruiseState
      dstWarm = transferSharedState @SharedFlight src dstInit

  let altVal = recordField (Field @"alt") dstWarm :: Double
      velVal = recordField (Field @"vel") dstWarm :: Double
      distVal = recordField (Field @"dist") dstWarm :: Double
      pidVal = recordField (Field @"pidIntegral") dstWarm :: Double
      wpVal = recordField (Field @"waypoint") dstWarm :: Int

  assertEq "Transferred alt matches source" altVal 12.5
  assertEq "Transferred vel matches source" velVal 4.2
  assertEq "Transferred dist matches source" distVal 15.0
  assertEq "Target-private pidIntegral preserved" pidVal 1.75
  assertEq "Target-private waypoint preserved" wpVal 3

{-------------------------------------------------------------------------------
  Test Suite 2: Shared Record Injection & Projection
-------------------------------------------------------------------------------}

testSuite2 :: IO ()
testSuite2 = do
  putStrLn "--- Test Suite 2: Shared Record Injection & Projection ---"
  let src :: Record TakeoffState
      src = insertField (Field @"alt") (25.0 :: Double)
          $ insertField (Field @"vel") (10.0 :: Double)
          $ insertField (Field @"dist") (100.0 :: Double)
          $ insertField (Field @"throttle") (0.9 :: Double)
          $ emptyRecord

      shared :: Record SharedFlight
      shared = projectSharedState @SharedFlight src

      altShared = recordField (Field @"alt") shared :: Double
      velShared = recordField (Field @"vel") shared :: Double
      distShared = recordField (Field @"dist") shared :: Double

  assertEq "Projected shared alt matches" altShared 25.0
  assertEq "Projected shared vel matches" velShared 10.0
  assertEq "Projected shared dist matches" distShared 100.0

  let landInit :: Record LandState
      landInit = insertField (Field @"alt") (0.0 :: Double)
               $ insertField (Field @"vel") (0.0 :: Double)
               $ insertField (Field @"dist") (0.0 :: Double)
               $ insertField (Field @"touchdown") False
               $ emptyRecord

      landWarm = injectSharedRecord @SharedFlight shared landInit
      altLand = recordField (Field @"alt") landWarm :: Double
      tdLand = recordField (Field @"touchdown") landWarm :: Bool

  assertEq "Injected warm alt into LandState" altLand 25.0
  assertEq "Preserved LandState touchdown default" tdLand False

{-------------------------------------------------------------------------------
  Test Suite 3: Pair-wise Continuous Mode Switch (switchContinuous2)
-------------------------------------------------------------------------------}

type SpeedShared =
  '[ "speed"    ':= Double
   , "distance" ':= Double
   ]

type AccelState =
  '[ "speed"    ':= Double
   , "distance" ':= Double
   , "accelTicks" ':= Int
   ]

type CruiseSpeedState =
  '[ "speed"    ':= Double
   , "distance" ':= Double
   , "governorCount" ':= Int
   ]

testSuite3 :: IO ()
testSuite3 = do
  putStrLn "--- Test Suite 3: Pair-wise Continuous Mode Switch (switchContinuous2) ---"

  -- Mode 1: Acceleration mode
  -- Ramps speed by +5.0 each tick until speed >= 20.0, then emits Just ()
  let accelInit :: Record AccelState
      accelInit = insertField (Field @"speed") (0.0 :: Double)
                $ insertField (Field @"distance") (0.0 :: Double)
                $ insertField (Field @"accelTicks") (0 :: Int)
                $ emptyRecord

      accelCoalg = FlatRecordCoalgebra accelInit $ \st () -> do
        let spd = recordField (Field @"speed") st :: Double
            dst = recordField (Field @"distance") st :: Double
            ticks = recordField (Field @"accelTicks") st :: Int
            !spd' = spd + 5.0
            !dst' = dst + spd'
            !ticks' = ticks + 1
            st' = setField (Field @"speed") spd'
                $ setField (Field @"distance") dst'
                $ setField (Field @"accelTicks") ticks' st
            !mSwitch = if spd' >= 20.0 then Just () else Nothing
        pure ((dst', mSwitch), st')

  -- Mode 2: Governor Cruise mode
  -- Maintains speed at target (no acceleration), advances distance by speed
  let cruiseInit :: Record CruiseSpeedState
      cruiseInit = insertField (Field @"speed") (0.0 :: Double)
                 $ insertField (Field @"distance") (0.0 :: Double)
                 $ insertField (Field @"governorCount") (0 :: Int)
                 $ emptyRecord

      cruiseStep st (_, ()) = do
        let spd = recordField (Field @"speed") st :: Double
            dst = recordField (Field @"distance") st :: Double
            cnt = recordField (Field @"governorCount") st :: Int
            !dst' = dst + spd
            !cnt' = cnt + 1
            st' = setField (Field @"distance") dst'
                $ setField (Field @"governorCount") cnt' st
        pure (dst', st')

  let streamMSF :: MSF IO () Double
      streamMSF = switchContinuous2 @SpeedShared accelCoalg cruiseInit cruiseStep

  distances <- runMSFStream streamMSF (replicate 8 ())

  -- Ticks 1-4 are in Accel:
  -- Tick 1: spd=5,  dst=5
  -- Tick 2: spd=10, dst=15
  -- Tick 3: spd=15, dst=30
  -- Tick 4: spd=20, dst=50 (emits switch! Mode 2 entered with spd=20, dst=50, advances to dst=70)
  -- Tick 5: Mode 2 cruise: dst = 70 + 20 = 90
  -- Tick 6: Mode 2 cruise: dst = 90 + 20 = 110
  -- Tick 7: Mode 2 cruise: dst = 110 + 20 = 130
  -- Tick 8: Mode 2 cruise: dst = 130 + 20 = 150
  assertEq "Tick 1 distance" (distances !! 0) 5.0
  assertEq "Tick 2 distance" (distances !! 1) 15.0
  assertEq "Tick 3 distance" (distances !! 2) 30.0
  assertEq "Tick 4 distance (switch tick)" (distances !! 3) 70.0
  assertEq "Tick 5 distance (warm cruise)" (distances !! 4) 90.0
  assertEq "Tick 6 distance (warm cruise)" (distances !! 5) 110.0
  assertEq "Tick 7 distance (warm cruise)" (distances !! 6) 130.0
  assertEq "Tick 8 distance (warm cruise)" (distances !! 7) 150.0

{-------------------------------------------------------------------------------
  Test Suite 4: N-ary Open Variant Continuous Switching (Drone Flight Controller)
-------------------------------------------------------------------------------}

testSuite4 :: IO ()
testSuite4 = do
  putStrLn "--- Test Suite 4: N-ary Open Variant Continuous Switching (Drone Flight Controller) ---"

  -- Mode 1: Takeoff mode
  -- Ramps altitude by 2.0 per tick until alt >= 10.0, then emits Cruise with targetAlt = 30.0
  let takeoffInit :: Record TakeoffState
      takeoffInit = insertField (Field @"alt") (0.0 :: Double)
                  $ insertField (Field @"vel") (0.0 :: Double)
                  $ insertField (Field @"dist") (0.0 :: Double)
                  $ insertField (Field @"throttle") (0.5 :: Double)
                  $ emptyRecord

      takeoffStep st () = do
        let alt = recordField (Field @"alt") st :: Double
            vel = recordField (Field @"vel") st :: Double
            dist = recordField (Field @"dist") st :: Double
            !alt' = alt + 2.5
            !vel' = 2.5
            !dist' = dist + 1.0
            st' = setField (Field @"alt") alt'
                $ setField (Field @"vel") vel'
                $ setField (Field @"dist") dist' st
            !mVar = if alt' >= 10.0
                      then Just (injectVariant (Field @"Cruise") (insertField (Field @"targetAlt") (30.0 :: Double) emptyRecord))
                      else Nothing
        pure (alt', mVar, st')

      takeoffMode = ContinuousMode @SharedFlight @FlightModes takeoffInit takeoffStep

  -- Template for Cruise:
  -- Flies forward, climbs towards targetAlt until dist >= 35.0, then emits Land with descentRate = -2.5
  let cruiseInit :: Record CruiseState
      cruiseInit = insertField (Field @"alt") (0.0 :: Double)
                 $ insertField (Field @"vel") (0.0 :: Double)
                 $ insertField (Field @"dist") (0.0 :: Double)
                 $ insertField (Field @"pidIntegral") (0.0 :: Double)
                 $ insertField (Field @"waypoint") (1 :: Int)
                 $ emptyRecord

      cruiseTpl = ContinuousTemplate @SharedFlight @FlightModes cruiseInit $ \st (payload, ()) -> do
        let targetAlt = recordField (Field @"targetAlt") payload :: Double
            alt = recordField (Field @"alt") st :: Double
            dist = recordField (Field @"dist") st :: Double
            intg = recordField (Field @"pidIntegral") st :: Double
            err = targetAlt - alt
            !intg' = intg + err * 0.1
            !alt' = min targetAlt (alt + 5.0)
            !dist' = dist + 5.0
            st' = setField (Field @"alt") alt'
                $ setField (Field @"vel") (5.0 :: Double)
                $ setField (Field @"dist") dist'
                $ setField (Field @"pidIntegral") intg' st
            !mVar = if dist' >= 35.0
                      then Just (injectVariant (Field @"Land") (insertField (Field @"descentRate") (-2.5 :: Double) emptyRecord))
                      else Nothing
        pure (alt', mVar, st')

  -- Template for Land:
  -- Descends at descentRate until alt <= 0.0
  let landInit :: Record LandState
      landInit = insertField (Field @"alt") (0.0 :: Double)
               $ insertField (Field @"vel") (0.0 :: Double)
               $ insertField (Field @"dist") (0.0 :: Double)
               $ insertField (Field @"touchdown") False
               $ emptyRecord

      landTpl = ContinuousTemplate @SharedFlight @FlightModes landInit $ \st (payload, ()) -> do
        let descent = recordField (Field @"descentRate") payload :: Double
            alt = recordField (Field @"alt") st :: Double
            dist = recordField (Field @"dist") st :: Double
            !alt' = max 0.0 (alt + descent)
            !dist' = dist + 0.5
            !isTouchdown = alt' <= 0.0
            st' = setField (Field @"alt") alt'
                $ setField (Field @"vel") descent
                $ setField (Field @"dist") dist'
                $ setField (Field @"touchdown") isTouchdown st
        pure (alt', Nothing, st')

  -- Dummy Takeoff template for full branch coverage
  let takeoffTpl = ContinuousTemplate @SharedFlight @FlightModes takeoffInit $ \st (_, ()) -> pure (0.0, Nothing, st)

  let templates = insertField (Field @"Takeoff") takeoffTpl
                $ insertField (Field @"Cruise") cruiseTpl
                $ insertField (Field @"Land") landTpl
                $ emptyRecord

  let flightMSF :: MSF IO () Double
      flightMSF = continuousMSF @SharedFlight @FlightModes takeoffMode templates

  -- Run flight for 25 ticks (allowing complete climb, cruise, and touchdown)
  alts <- runMSFStream flightMSF (replicate 25 ())

  -- Check progression:
  -- Takeoff: starts alt=2.5, 5.0, 7.5, 10.0.
  -- At alt=10.0, switches to Cruise.
  -- Cruise receives warm alt=10.0, climbs 15, 20, 25, 30.
  -- Cruise reaches dist >= 35.0, switches to Land.
  -- Land receives warm alt=30.0, descends by -2.5 each tick until 0.0.
  assertEq "Takeoff tick 1 alt" (alts !! 0) 2.5
  assertEq "Takeoff tick 2 alt" (alts !! 1) 5.0
  assertEq "Takeoff tick 3 alt" (alts !! 2) 7.5
  assertEq "Takeoff switch tick alt" (alts !! 3) 15.0  -- Cruise warm-started from 10.0 and stepped +5.0 to 15.0!
  assertEq "Cruise tick 5 alt" (alts !! 4) 20.0
  assertEq "Cruise tick 6 alt" (alts !! 5) 25.0
  assertEq "Cruise tick 7 alt" (alts !! 6) 30.0
  -- Eventually lands to 0.0:
  let finalAlt = last alts
  assertEq "Final landed altitude" finalAlt 0.0

{-------------------------------------------------------------------------------
  Test Suite 5: Continuous Telemetry Stream Observation
-------------------------------------------------------------------------------}

testSuite5 :: IO ()
testSuite5 = do
  putStrLn "--- Test Suite 5: Continuous Telemetry Stream Observation ---"

  let takeoffInit :: Record TakeoffState
      takeoffInit = insertField (Field @"alt") (0.0 :: Double)
                  $ insertField (Field @"vel") (0.0 :: Double)
                  $ insertField (Field @"dist") (0.0 :: Double)
                  $ insertField (Field @"throttle") (0.5 :: Double)
                  $ emptyRecord

      takeoffStep st () = do
        let alt = recordField (Field @"alt") st :: Double
            vel = recordField (Field @"vel") st :: Double
            dist = recordField (Field @"dist") st :: Double
            !alt' = alt + 5.0
            !dist' = dist + 2.0
            st' = setField (Field @"alt") alt'
                $ setField (Field @"vel") (5.0 :: Double)
                $ setField (Field @"dist") dist' st
            !mVar = if alt' >= 10.0
                      then Just (injectVariant (Field @"Cruise") (insertField (Field @"targetAlt") (20.0 :: Double) emptyRecord))
                      else Nothing
        pure (alt', mVar, st')

      takeoffMode = ContinuousMode @SharedFlight @FlightModes takeoffInit takeoffStep

      cruiseInit :: Record CruiseState
      cruiseInit = insertField (Field @"alt") (0.0 :: Double)
                 $ insertField (Field @"vel") (0.0 :: Double)
                 $ insertField (Field @"dist") (0.0 :: Double)
                 $ insertField (Field @"pidIntegral") (0.0 :: Double)
                 $ insertField (Field @"waypoint") (1 :: Int)
                 $ emptyRecord

      cruiseTpl = ContinuousTemplate @SharedFlight @FlightModes cruiseInit $ \st (payload, ()) -> do
        let alt = recordField (Field @"alt") st :: Double
            dist = recordField (Field @"dist") st :: Double
            !alt' = alt + 2.0
            !dist' = dist + 10.0
            st' = setField (Field @"alt") alt'
                $ setField (Field @"dist") dist' st
        pure (alt', Nothing, st')

      takeoffTpl = ContinuousTemplate @SharedFlight @FlightModes takeoffInit $ \st (_, ()) -> pure (0.0, Nothing, st)
      landInit :: Record LandState
      landInit = insertField (Field @"alt") 0.0 $ insertField (Field @"vel") 0.0 $ insertField (Field @"dist") 0.0 $ insertField (Field @"touchdown") False emptyRecord
      landTpl = ContinuousTemplate @SharedFlight @FlightModes landInit $ \st (_, ()) -> pure (0.0, Nothing, st)

      templates = insertField (Field @"Takeoff") takeoffTpl
                $ insertField (Field @"Cruise") cruiseTpl
                $ insertField (Field @"Land") landTpl
                $ emptyRecord

      telemetryMSF :: MSF IO () (Double, Record SharedFlight)
      telemetryMSF = continuousTelemetryMSF @SharedFlight @FlightModes takeoffMode templates

  samples <- runMSFStream telemetryMSF (replicate 5 ())

  -- Verify telemetry matches internal shared state on each tick
  let (alt1, tele1) = samples !! 0
      (alt2, tele2) = samples !! 1  -- Switch happens here
      (alt3, tele3) = samples !! 2  -- Inside cruise

  assertEq "Tick 1 output alt matches telemetry alt" alt1 (recordField (Field @"alt") tele1 :: Double)
  assertEq "Tick 2 output alt matches telemetry alt" alt2 (recordField (Field @"alt") tele2 :: Double)
  assertEq "Tick 3 output alt matches telemetry alt" alt3 (recordField (Field @"alt") tele3 :: Double)
  assertEq "Tick 3 dist is continuous into cruise" (recordField (Field @"dist") tele3 :: Double) 24.0

{-------------------------------------------------------------------------------
  Test Suite 6: Multi-Transition Round-Trip State Stability (Mode A <-> Mode B)
-------------------------------------------------------------------------------}

type EnergyShared =
  '[ "energy"    ':= Double
   , "stepCount" ':= Int
   ]

type ModeAState =
  '[ "energy"    ':= Double
   , "stepCount" ':= Int
   , "boost"     ':= Double
   ]

type ModeBState =
  '[ "energy"    ':= Double
   , "stepCount" ':= Int
   , "decay"     ':= Double
   ]

type EnergyModes =
  '[ "ModeA" ':= '[]
   , "ModeB" ':= '[]
   ]

testSuite6 :: IO ()
testSuite6 = do
  putStrLn "--- Test Suite 6: Multi-Transition Round-Trip State Stability (Mode A <-> Mode B) ---"

  -- Mode A adds 10.0 energy per tick. After 2 ticks, switches to Mode B.
  let initA :: Record ModeAState
      initA = insertField (Field @"energy") (0.0 :: Double)
            $ insertField (Field @"stepCount") (0 :: Int)
            $ insertField (Field @"boost") (1.0 :: Double)
            $ emptyRecord

      tplA = ContinuousTemplate @EnergyShared @EnergyModes initA $ \st (_, ()) -> do
        let e = recordField (Field @"energy") st :: Double
            cnt = recordField (Field @"stepCount") st :: Int
            !e' = e + 10.0
            !cnt' = cnt + 1
            st' = setField (Field @"energy") e'
                $ setField (Field @"stepCount") cnt' st
            !mVar = if cnt' `mod` 4 == 2
                      then Just (injectVariant (Field @"ModeB") emptyRecord)
                      else Nothing
        pure (e', mVar, st')

  -- Mode B adds 5.0 energy per tick. After 2 ticks, switches back to Mode A.
  let initB :: Record ModeBState
      initB = insertField (Field @"energy") (0.0 :: Double)
            $ insertField (Field @"stepCount") (0 :: Int)
            $ insertField (Field @"decay") (0.1 :: Double)
            $ emptyRecord

      tplB = ContinuousTemplate @EnergyShared @EnergyModes initB $ \st (_, ()) -> do
        let e = recordField (Field @"energy") st :: Double
            cnt = recordField (Field @"stepCount") st :: Int
            !e' = e + 5.0
            !cnt' = cnt + 1
            st' = setField (Field @"energy") e'
                $ setField (Field @"stepCount") cnt' st
            !mVar = if cnt' `mod` 4 == 0
                      then Just (injectVariant (Field @"ModeA") emptyRecord)
                      else Nothing
        pure (e', mVar, st')

      templates = insertField (Field @"ModeA") tplA
                $ insertField (Field @"ModeB") tplB
                $ emptyRecord

      initialMode = ContinuousMode @EnergyShared @EnergyModes initA $ \st () -> do
        let e = recordField (Field @"energy") st :: Double
            cnt = recordField (Field @"stepCount") st :: Int
            !e' = e + 10.0
            !cnt' = cnt + 1
            st' = setField (Field @"energy") e'
                $ setField (Field @"stepCount") cnt' st
            !mVar = if cnt' == 2
                      then Just (injectVariant (Field @"ModeB") emptyRecord)
                      else Nothing
        pure (e', mVar, st')

  let streamMSF = continuousMSF @EnergyShared @EnergyModes initialMode templates

  energies <- runMSFStream streamMSF (replicate 8 ())

  -- Check monotonic accumulation across back-and-forth transitions:
  -- Tick 1 (Mode A): e = 10.0
  -- Tick 2 (Mode A reaches 20.0, switches to Mode B which steps +5.0): e = 25.0
  -- Tick 3 (Mode B reaches 30.0, switches to Mode A which steps +10.0): e = 40.0
  -- Tick 4 (Mode A reaches 50.0, switches to Mode B which steps +5.0): e = 55.0
  -- Tick 5 (Mode B reaches 60.0, switches to Mode A which steps +10.0): e = 70.0
  -- Tick 6 (Mode A reaches 80.0, switches to Mode B which steps +5.0): e = 85.0
  -- Tick 7 (Mode B reaches 90.0, switches to Mode A which steps +10.0): e = 100.0
  -- Tick 8 (Mode A reaches 110.0, switches to Mode B which steps +5.0): e = 115.0
  assertEq "Tick 1 energy (Mode A)" (energies !! 0) 10.0
  assertEq "Tick 2 energy (switch to Mode B)" (energies !! 1) 25.0
  assertEq "Tick 3 energy (switch to Mode A)" (energies !! 2) 40.0
  assertEq "Tick 4 energy (switch to Mode B)" (energies !! 3) 55.0
  assertEq "Tick 5 energy (switch to Mode A)" (energies !! 4) 70.0
  assertEq "Tick 6 energy (switch to Mode B)" (energies !! 5) 85.0
  assertEq "Tick 7 energy (switch to Mode A)" (energies !! 6) 100.0
  assertEq "Tick 8 energy (switch to Mode B)" (energies !! 7) 115.0

{-------------------------------------------------------------------------------
  Test Suite 7: Coalgebra State Warm-Start (transferCoalgebraState)
-------------------------------------------------------------------------------}

testSuite7 :: IO ()
testSuite7 = do
  putStrLn "--- Test Suite 7: Coalgebra State Warm-Start (transferCoalgebraState) ---"

  let c1Init :: Record TakeoffState
      c1Init = insertField (Field @"alt") (100.0 :: Double)
             $ insertField (Field @"vel") (25.0 :: Double)
             $ insertField (Field @"dist") (500.0 :: Double)
             $ insertField (Field @"throttle") (0.95 :: Double)
             $ emptyRecord

      c1 :: FlatRecordCoalgebra TakeoffState IO () Double
      c1 = FlatRecordCoalgebra c1Init $ \st () ->
        pure (recordField (Field @"alt") st, st)

      c2Init :: Record CruiseState
      c2Init = insertField (Field @"alt") (0.0 :: Double)
             $ insertField (Field @"vel") (0.0 :: Double)
             $ insertField (Field @"dist") (0.0 :: Double)
             $ insertField (Field @"pidIntegral") (5.5 :: Double)
             $ insertField (Field @"waypoint") (4 :: Int)
             $ emptyRecord

      c2 :: FlatRecordCoalgebra CruiseState IO () (Double, Double, Int)
      c2 = FlatRecordCoalgebra c2Init $ \st () -> do
        let alt = recordField (Field @"alt") st :: Double
            pid = recordField (Field @"pidIntegral") st :: Double
            wp = recordField (Field @"waypoint") st :: Int
        pure ((alt, pid, wp), st)

      c2Warm = transferCoalgebraState @SharedFlight c1 c2

  ((alt, pid, wp), _) <- recordCoalgStep c2Warm (recordCoalgInit c2Warm) ()

  assertEq "Warmed c2 altitude matches c1" alt 100.0
  assertEq "Warmed c2 pidIntegral preserved" pid 5.5
  assertEq "Warmed c2 waypoint preserved" wp 4

{-------------------------------------------------------------------------------
  Main Runner
-------------------------------------------------------------------------------}

main :: IO ()
main = do
  putStrLn "================================================================================"
  putStrLn "  Control.Kernmantle.Rope.Continuity Verification Suite (Tier 3.1)"
  putStrLn "================================================================================"
  testSuite1
  testSuite2
  testSuite3
  testSuite4
  testSuite5
  testSuite6
  testSuite7
  putStrLn "================================================================================"
  putStrLn "  All 7 Verification Suites for Tier 3.1 Passed Successfully!"
  putStrLn "================================================================================"
