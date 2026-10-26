{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Main
Description : Executable verification suite for Control.Kernmantle.Rope.Analysis
Copyright   : (c) 2026 The Kernmantle Authors
License     : BSD-3-Clause

Verifies all aspects of Tier 3.2: Pre-Flight Static Resource Analysis with SomeBinEff:
1. Pure Functorial SomeBinEff Composition (Category, Arrow, ArrowChoice, Profunctor, Strong, Choice)
2. Whole-Graph Hardware Port Accumulation (analyzeSwitchGraph across initial mode and template branches)
3. Hardware Port Topology Validation (verifyHardwareTopology passing on valid motherboard and catching missing ports)
4. Rhine Multi-Rate Nyquist Bounds Verification (verifyClockDomains catching under-sampled signal bandwidths)
5. Resource Budget Constraints (verifyResourceBudget enforcing memory and port bounds)
6. Pre-Flight Gate & Compilation to MSF (verifyAndCompileSwitchMSF returning Left on invalid networks and Right on valid networks)
7. Full Execution of Verified MSF (ticking discrete causal steps with zero analysis overhead)
-}
module Main (main) where

import Control.Arrow
import Control.Category
import Control.Monad (when)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.List (isInfixOf)
import qualified Data.Set as Set
import GHC.TypeLits (Symbol)
import System.Exit (exitFailure)

import Prelude hiding (id, (.))

import Control.Kernmantle.FRP
import Polytropos
  ( Record
  , Variant
  , emptyRecord
  , insertField
  , setField
  , recordField
  , injectVariant
  , Field(..)
  , pattern REC
  )
import Polytropos.Core
  ( Row
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

{-------------------------------------------------------------------------------
  Flight Modes & Domain Types
-------------------------------------------------------------------------------}

data FlightCommand
  = CmdHold
  | CmdCruise !Double
  | CmdFailsafe
  deriving (Eq, Show)

type DroneModes =
  '[ "hover"    ':= '[]
   , "cruise"   ':= '["targetSpeed" ':= Double]
   , "failsafe" ':= '[]
   ]

-- Hardware Port Descriptors
portGPS :: HardwarePort
portGPS = HardwarePort
  { portName      = "U-Blox GNSS Receiver"
  , portDirection = PortInput
  , portDevice    = SensorDevice
  , portRateHz    = 10.0
  }

portBaro :: HardwarePort
portBaro = HardwarePort
  { portName      = "BMP388 Barometer"
  , portDirection = PortInput
  , portDevice    = SensorDevice
  , portRateHz    = 50.0
  }

portPitot :: HardwarePort
portPitot = HardwarePort
  { portName      = "Differential Airspeed Sensor (Pitot)"
  , portDirection = PortInput
  , portDevice    = SensorDevice
  , portRateHz    = 100.0
  }

portParachute :: HardwarePort
portParachute = HardwarePort
  { portName      = "Emergency Ballistic Parachute Actuator"
  , portDirection = PortOutput
  , portDevice    = ActuatorDevice
  , portRateHz    = 10.0
  }

portTelemetryRadio :: HardwarePort
portTelemetryRadio = HardwarePort
  { portName      = "433MHz Telemetry Transceiver"
  , portDirection = PortBidirectional
  , portDevice    = CommunicationBus
  , portRateHz    = 20.0
  }

main :: IO ()
main = do
  putStrLn "================================================================================"
  putStrLn "  Kernmantle Tier 3.2: Pre-Flight Static Resource Analysis with SomeBinEff"
  putStrLn "================================================================================"

  ------------------------------------------------------------------------------
  -- Suite 1: Pure Functorial SomeBinEff Composition
  ------------------------------------------------------------------------------
  putStrLn "\n--- [Suite 1] Pure Functorial SomeBinEff Composition ---"

  let eff1 :: SomeBinEff (Const [String]) Identity Identity Int String
      eff1 = SomeBinEff (Const ["eff1:IntToStr"])

      eff2 :: SomeBinEff (Const [String]) Identity Identity String Int
      eff2 = SomeBinEff (Const ["eff2:StrLen"])

      effCombined = eff1 >>> eff2

  case unSomeBinEff effCombined of
    Const tags -> do
      assertEq "effCombined collects accumulated static tags" tags ["eff1:IntToStr", "eff2:StrLen"]

  -- Map metadata test
  let effMapped = mapSomeBinEffMeta (\(Const xs) -> Const (length xs)) effCombined
  case unSomeBinEff effMapped of
    Const count -> assertEq "mapSomeBinEffMeta transforms static functor" count 2

  -- Pure evaluation test with runSomeBinEffPure
  let pureEff :: SomeBinEff Identity Identity Identity Double Double
      pureEff = arr (* 2.5) >>> arr (+ 10.0)

      resVal = runSomeBinEffPure pureEff 4.0
  assertEq "runSomeBinEffPure executes composed pipeline" resVal 20.0

  -- ArrowChoice branching on SomeBinEff
  let effChoice :: SomeBinEff (Const [String]) Identity Identity (Either Int Double) (Either String String)
      effChoice = arr show +++ arr show
  case unSomeBinEff effChoice of
    Const _ -> assert "ArrowChoice on SomeBinEff compiles and constructs cleanly" True

  ------------------------------------------------------------------------------
  -- Suite 2: Whole-Graph Hardware Port Accumulation
  ------------------------------------------------------------------------------
  putStrLn "\n--- [Suite 2] Whole-Graph Hardware Port Accumulation (analyzeSwitchGraph) ---"

  -- Initial mode: GPS hold requiring portGPS (128 bytes state footprint)
  let initMode :: AnalyzedArrow HardwareAnalysis (MSF Identity) (FlightCommand, Double) (String, Maybe (Variant DroneModes))
      initMode =
        requireHardware portGPS 128 >>>
        arr (\(cmd, alt) -> case cmd of
          CmdHold -> ("Steady GPS Hold at alt " ++ show alt, Nothing)
          CmdCruise spd -> ("Transitioning to cruise...", Just (injectVariant (Field @"cruise") (REC { targetSpeed = spd })))
          CmdFailsafe -> ("EMERGENCY: Transitioning to failsafe!", Just (injectVariant (Field @"failsafe") emptyRecord))
        )

  -- Switch Table Templates: concrete Record of AnalyzedArrow templates
  let hoverTemplate :: AnalyzedArrow HardwareAnalysis (MSF Identity) (Record '[], (FlightCommand, Double)) String
      hoverTemplate =
        requireHardware portBaro 64 >>>
        arr (\(_payload, (_cmd, alt)) -> "Hovering at alt " ++ show alt)

      cruiseTemplate :: AnalyzedArrow HardwareAnalysis (MSF Identity) (Record '["targetSpeed" ':= Double], (FlightCommand, Double)) String
      cruiseTemplate =
        requireHardware portPitot 256 >>>
        arr (\(REC { targetSpeed = spd }, (_cmd, alt)) ->
          "Cruising at speed " ++ show spd ++ " knots, alt " ++ show alt)

      failsafeTemplate :: AnalyzedArrow HardwareAnalysis (MSF Identity) (Record '[], (FlightCommand, Double)) String
      failsafeTemplate =
        requireHardware portParachute 32 >>>
        arr (\(_payload, (_cmd, alt)) -> "EMERGENCY PARACHUTE DEPLOYED at alt " ++ show alt)

      templates :: Record (TemplateBranchesCore (AnalyzedArrow HardwareAnalysis (MSF Identity)) (FlightCommand, Double) String DroneModes)
      templates = REC
        { hover    = hoverTemplate
        , cruise   = cruiseTemplate
        , failsafe = failsafeTemplate
        }

  -- 100% whole-graph pre-flight static analysis
  let !totalAnalysis = analyzeSwitchGraph @DroneModes initMode templates

  putStrLn $ "  Accumulated ports: " ++ show (Set.size (allocatedPorts totalAnalysis))
  putStrLn $ "  Max state footprint: " ++ show (maxStateBytes totalAnalysis) ++ " bytes"
  putStrLn $ "  Total reachable subsystems: " ++ show (totalSubsystems totalAnalysis)

  assertEq "analyzeSwitchGraph accumulated all 4 reachable hardware ports"
    (allocatedPorts totalAnalysis)
    (Set.fromList [portGPS, portBaro, portPitot, portParachute])

  assertEq "analyzeSwitchGraph computed maximum state footprint"
    (maxStateBytes totalAnalysis)
    256 -- max of 128 (GPS), 64 (Baro), 256 (Pitot), 32 (Parachute)

  assertEq "analyzeSwitchGraph computed total reachable subsystems"
    (totalSubsystems totalAnalysis)
    4

  ------------------------------------------------------------------------------
  -- Suite 3: Hardware Port Topology Validation
  ------------------------------------------------------------------------------
  putStrLn "\n--- [Suite 3] Hardware Port Topology Validation (verifyHardwareTopology) ---"

  -- Case A: Valid Motherboard (equipped with all 4 required ports + spare telemetry radio)
  let moboValid = Set.fromList [portGPS, portBaro, portPitot, portParachute, portTelemetryRadio]
      (validPassed, validErrors) = verifyHardwareTopology moboValid totalAnalysis

  assert "verifyHardwareTopology succeeds on motherboard with all ports" validPassed
  assertEq "verifyHardwareTopology reports zero errors on valid motherboard" validErrors []

  -- Case B: Faulty Motherboard (missing the Emergency Parachute actuator)
  let moboMissingParachute = Set.fromList [portGPS, portBaro, portPitot, portTelemetryRadio]
      (invalidPassed, invalidErrors) = verifyHardwareTopology moboMissingParachute totalAnalysis

  assert "verifyHardwareTopology catches missing parachute actuator before flight" (not invalidPassed)
  assert "verifyHardwareTopology produces diagnostic error message" (length invalidErrors == 1)
  case invalidErrors of
    (err:_) -> putStrLn $ "  Caught expected pre-flight error: " ++ err
    []      -> pure ()

  ------------------------------------------------------------------------------
  -- Suite 4: Rhine Multi-Rate Nyquist Bounds Verification
  ------------------------------------------------------------------------------
  putStrLn "\n--- [Suite 4] Rhine Multi-Rate Nyquist Bounds Verification ---"

  -- Case A: Compliant Multi-Rate Clock Domains
  -- IMU Gyro: f_max = 200 Hz -> sampled at f_s = 500 Hz (>= 2 * 200 = 400 Hz) -> VALID
  -- Barometer: f_max = 10 Hz -> sampled at f_s = 50 Hz (>= 2 * 10 = 20 Hz) -> VALID
  let clkIMU :: AnalyzedArrow ClockVerification (MSF Identity) a a
      clkIMU = declareClockDomain "IMU-Attitude" 500.0 (Just ("Gyroscope", 200.0))

      clkBaro :: AnalyzedArrow ClockVerification (MSF Identity) a a
      clkBaro = declareClockDomain "Baro-Altitude" 50.0 (Just ("PressureSensor", 10.0))

      validClockPipeline = clkIMU >>> clkBaro
      validClockAnalysis = getAnalysis validClockPipeline
      (nyquistPassed, nyquistErrors) = verifyClockDomains validClockAnalysis

  assert "verifyClockDomains passes compliant multi-rate sampling rates" nyquistPassed
  assertEq "verifyClockDomains reports zero violations on compliant clock pipeline" nyquistErrors []

  -- Case B: Nyquist Violation
  -- Accelerometer Vibration: f_max = 1000 Hz -> sampled at f_s = 800 Hz (< 2000 Hz) -> VIOLATION!
  let clkVibrationUnderSampled :: AnalyzedArrow ClockVerification (MSF Identity) a a
      clkVibrationUnderSampled = declareClockDomain "Motor-Vibration" 800.0 (Just ("PiezoSensor", 1000.0))

      violatingPipeline = clkIMU >>> clkVibrationUnderSampled
      violatingAnalysis = getAnalysis violatingPipeline
      (nyquistViolPassed, nyquistViolErrors) = verifyClockDomains violatingAnalysis

  assert "verifyClockDomains catches Nyquist violation (f_s < 2 * f_max)" (not nyquistViolPassed)
  assert "verifyClockDomains identifies offending clock domain in error message"
    (any (isInfixOf "Motor-Vibration") nyquistViolErrors)
  case nyquistViolErrors of
    (viol:_) -> putStrLn $ "  Caught Nyquist violation: " ++ viol
    []       -> pure ()

  ------------------------------------------------------------------------------
  -- Suite 5: Resource Budget Constraints
  ------------------------------------------------------------------------------
  putStrLn "\n--- [Suite 5] Resource Budget Constraints (verifyResourceBudget) ---"

  -- Scenario A: Generous budget (512 bytes memory, 8 ports)
  let budgetA = BudgetLimit { maxAllowedMemoryBytes = 512, maxAllowedPorts = 8 }
      reportA = verifyResourceBudget budgetA totalAnalysis

  assert "verifyResourceBudget passes within generous limits" (budgetPassed reportA)
  assertEq "Actual memory matches max state bytes" (memoryActual reportA) 256
  assertEq "Actual port count matches allocated ports" (portCountActual reportA) 4

  -- Scenario B: Memory budget exceeded (128 bytes limit < 256 bytes required)
  let budgetB = BudgetLimit { maxAllowedMemoryBytes = 128, maxAllowedPorts = 8 }
      reportB = verifyResourceBudget budgetB totalAnalysis

  assert "verifyResourceBudget flags exceeded memory footprint" (not (budgetPassed reportB))
  assert "verifyResourceBudget reports memory warning" (not (null (budgetWarnings reportB)))
  case budgetWarnings reportB of
    (warn:_) -> putStrLn $ "  Memory warning: " ++ warn
    []       -> pure ()

  -- Scenario C: Port count budget exceeded (3 ports limit < 4 ports required)
  let budgetC = BudgetLimit { maxAllowedMemoryBytes = 512, maxAllowedPorts = 3 }
      reportC = verifyResourceBudget budgetC totalAnalysis

  assert "verifyResourceBudget flags exceeded port count" (not (budgetPassed reportC))
  assert "verifyResourceBudget reports port warning" (not (null (budgetWarnings reportC)))
  case budgetWarnings reportC of
    (warn:_) -> putStrLn $ "  Port warning: " ++ warn
    []       -> pure ()

  ------------------------------------------------------------------------------
  -- Suite 6: Pre-Flight Gate & Compilation to MSF
  ------------------------------------------------------------------------------
  putStrLn "\n--- [Suite 6] Pre-Flight Gate & Compilation to MSF ---"

  let validatorFor mobo = \hwAnalysis ->
        let (hwOk, hwErrs) = verifyHardwareTopology mobo hwAnalysis
            bReport = verifyResourceBudget (BudgetLimit 512 8) hwAnalysis
            bOk = budgetPassed bReport
            bErrs = budgetWarnings bReport
        in (hwOk && bOk, hwErrs ++ bErrs)

  -- Test compilation failure on invalid motherboard
  let compileResultBad = verifyAndCompileSwitchMSF (validatorFor moboMissingParachute) initMode templates
  case compileResultBad of
    Left errs -> do
      assert "verifyAndCompileSwitchMSF returns Left on invalid motherboard topology" True
      case errs of
        (e:_) -> putStrLn $ "  Compilation gate rejected graph with: " ++ e
        []    -> pure ()
    Right _ -> do
      assert "verifyAndCompileSwitchMSF should not have compiled invalid topology" False

  -- Test compilation success on valid motherboard
  let compileResultGood = verifyAndCompileSwitchMSF (validatorFor moboValid) initMode templates
  case compileResultGood of
    Left errs -> do
      putStrLn $ "  Unexpected compilation failure: " ++ show errs
      assert "verifyAndCompileSwitchMSF compiled valid graph" False
    Right _compiledMSF -> do
      assert "verifyAndCompileSwitchMSF returns Right compiled bare MSF on valid topology" True

  ------------------------------------------------------------------------------
  -- Suite 7: Full Execution of Verified MSF (Zero Runtime Overhead)
  ------------------------------------------------------------------------------
  putStrLn "\n--- [Suite 7] Full Execution of Verified MSF ---"

  case compileResultGood of
    Left _ -> exitFailure
    Right compiledMSF -> do
      -- Step 1: In GPS hold mode (steady-state continuation)
      let (out1, msf1) = stepPureMSF compiledMSF (CmdHold, 100.0)
      assertEq "Step 1: Steady GPS Hold" out1 "Steady GPS Hold at alt 100.0"

      -- Step 2: Trigger mode switch to cruise with targetSpeed = 45.0
      let (out2, msf2) = stepPureMSF msf1 (CmdCruise 45.0, 120.0)
      assertEq "Step 2: Dynamic switch to Cruise template with injected speed"
        out2
        "Cruising at speed 45.0 knots, alt 120.0"

      -- Step 3: Next tick continues in cruise mode
      let (out3, _msf3) = stepPureMSF msf2 (CmdCruise 55.0, 150.0)
      assertEq "Step 3: Continuous execution in Cruise mode"
        out3
        "Cruising at speed 45.0 knots, alt 150.0"

  putStrLn "\n================================================================================"
  putStrLn "✓ ALL 7 Tier 3.2 Pre-Flight Static Resource Analysis verification suites PASSED!"
  putStrLn "================================================================================"
