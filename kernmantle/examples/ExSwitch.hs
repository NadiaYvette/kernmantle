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
Executable verification suite and example for 'Control.Kernmantle.Rope.Switch'
(polySwitch, polyBranch, templateSwitch, templateBranch).
-}
module Main (main) where

import Prelude hiding (id, (.))

import Control.Arrow (Kleisli(..))
import Control.Category (id, (.))
import Control.Kernmantle.Rope
import Control.Kernmantle.Rope.Internal (pattern RNil)
import Polytropos

-- | Input sensor data passed to the reactive system.
data SensorData = SensorData
  { altitude :: !Double
  , velocity :: !Double
  } deriving (Show, Eq)

-- | Commands that may trigger discrete mode transitions.
data Command
  = IdleCmd
  | DockCmd !String
  | CruiseCmd !Int
  | LandCmd
  deriving (Show, Eq)

-- | Parameterized open variant representing discrete flight mode transition events.
type FlightModes =
  '[ "docking"  ':= '["target" ':= String]
   , "cruising" ':= '["speed" ':= Int]
   , "landing"  ':= '[]
   ]

main :: IO ()
main = do
  putStrLn "=== Testing Kernmantle Parameterized Open Variant Switching (polySwitch) ==="

  let sensor = SensorData { altitude = 1000.0, velocity = 120.0 }

  ------------------------------------------------------------------------------
  -- 1. Test polySwitch with Parameterized Rope Arrow Templates
  ------------------------------------------------------------------------------
  putStrLn "\n--- [1/5] Testing polySwitch with Pure Arrows ((->)) ---"

  -- Initial idle behavior arrow: produces steady-state telemetry AND checks for mode transitions
  let idleRope :: TightRope '[] (->) (Command, SensorData) (String, Maybe (Variant FlightModes))
      idleRope = mkRope \_ (cmd, s) -> case cmd of
        IdleCmd ->
          ( "Idle Mode: alt=" ++ show (altitude s) ++ ", vel=" ++ show (velocity s)
          , Nothing
          )
        DockCmd targetStation ->
          ( "Transitioning to docking..."
          , Just (injectVariant (Field @"docking") (REC { target = targetStation }))
          )
        CruiseCmd cruisingSpeed ->
          ( "Transitioning to cruising..."
          , Just (injectVariant (Field @"cruising") (REC { speed = cruisingSpeed }))
          )
        LandCmd ->
          ( "Transitioning to landing..."
          , Just (injectVariant (Field @"landing") emptyRecord)
          )

  -- Dual switch table: a Polytropos Record mapping each constructor to a parameterized Rope template
  let switchTable :: Record (TightTemplateBranches '[] (->) (Command, SensorData) String FlightModes)
      switchTable = REC
        { docking = mkRope \_ (REC { target = tgt }, (_cmd, s)) ->
            "Docking Template Engaged: Station=" ++ tgt ++ ", approach_alt=" ++ show (altitude s)
        , cruising = mkRope \_ (REC { speed = spd }, (_cmd, s)) ->
            "Cruising Template Engaged: TargetSpeed=" ++ show spd ++ " m/s, current_vel=" ++ show (velocity s)
        , landing = mkRope \_ (_payload, (_cmd, s)) ->
            "Landing Template Engaged: Final touchdown descent from alt=" ++ show (altitude s)
        }

  let switchedRope = polySwitch idleRope switchTable
      runPure (rope :: TightRope '[] (->) a b) inp = runRope rope RNil inp

  -- Case A: No switch fired (continues steady-state idle telemetry)
  let resIdle = runPure switchedRope (IdleCmd, sensor)
  putStrLn $ "  [Idle]     " ++ resIdle
  if resIdle == "Idle Mode: alt=1000.0, vel=120.0"
    then putStrLn "  ✓ Steady-state continuation passed!"
    else error $ "Steady-state failed: " ++ resIdle

  -- Case B: Docking event fired -> dispatches to #docking template in O(1)
  let resDock = runPure switchedRope (DockCmd "ISS-Gateway", sensor)
  putStrLn $ "  [Docking]  " ++ resDock
  if resDock == "Docking Template Engaged: Station=ISS-Gateway, approach_alt=1000.0"
    then putStrLn "  ✓ Parameterized docking template switch passed!"
    else error $ "Docking switch failed: " ++ resDock

  -- Case C: Cruising event fired -> dispatches to #cruising template in O(1)
  let resCruise = runPure switchedRope (CruiseCmd 450, sensor)
  putStrLn $ "  [Cruising] " ++ resCruise
  if resCruise == "Cruising Template Engaged: TargetSpeed=450 m/s, current_vel=120.0"
    then putStrLn "  ✓ Parameterized cruising template switch passed!"
    else error $ "Cruising switch failed: " ++ resCruise

  -- Case D: Landing event fired (zero-payload constructor) -> dispatches to #landing template in O(1)
  let resLand = runPure switchedRope (LandCmd, sensor)
  putStrLn $ "  [Landing]  " ++ resLand
  if resLand == "Landing Template Engaged: Final touchdown descent from alt=1000.0"
    then putStrLn "  ✓ Empty-payload landing template switch passed!"
    else error $ "Landing switch failed: " ++ resLand

  ------------------------------------------------------------------------------
  -- 2. Test polyBranch: Explicit Either Branching
  ------------------------------------------------------------------------------
  putStrLn "\n--- [2/5] Testing polyBranch (Either b (Variant sw)) ---"

  let branchingRope :: TightRope '[] (->) (Command, SensorData) (Either String (Variant FlightModes))
      branchingRope = mkRope \_ (cmd, s) -> case cmd of
        IdleCmd -> Left ("Holding: alt=" ++ show (altitude s))
        DockCmd tgt -> Right (injectVariant (Field @"docking") (REC { target = tgt }))
        CruiseCmd spd -> Right (injectVariant (Field @"cruising") (REC { speed = spd }))
        LandCmd -> Right (injectVariant (Field @"landing") emptyRecord)

  let branchedFlight = polyBranch branchingRope switchTable

  let bResIdle = runPure branchedFlight (IdleCmd, sensor)
      bResDock = runPure branchedFlight (DockCmd "Mars-Orbiter", sensor)

  putStrLn $ "  [Left]     " ++ bResIdle
  putStrLn $ "  [Right]    " ++ bResDock

  if bResIdle == "Holding: alt=1000.0" &&
     bResDock == "Docking Template Engaged: Station=Mars-Orbiter, approach_alt=1000.0"
    then putStrLn "  ✓ polyBranch tests passed!"
    else error "polyBranch tests failed!"

  ------------------------------------------------------------------------------
  -- 3. Test templateSwitch: Single Parameterized Arrow Template Switch
  ------------------------------------------------------------------------------
  putStrLn "\n--- [3/5] Testing templateSwitch (Single Parameterized Mode) ---"

  let singleInit :: TightRope '[] (->) SensorData (String, Maybe Double)
      singleInit = mkRope \_ s ->
        if altitude s > 500.0
          then ("High altitude: " ++ show (altitude s), Nothing)
          else ("Low altitude alert", Just (altitude s))

      lowAltTemplate :: TightRope '[] (->) (Double, SensorData) String
      lowAltTemplate = mkRope \_ (threshold, s) ->
        "TERRAIN ALERT at alt=" ++ show (altitude s) ++ " (threshold=" ++ show threshold ++ ")"

      singleSwitched = templateSwitch singleInit lowAltTemplate

  let highRes = runPure singleSwitched (SensorData 800.0 100.0)
      lowRes  = runPure singleSwitched (SensorData 350.0 100.0)

  putStrLn $ "  [High Alt] " ++ highRes
  putStrLn $ "  [Low Alt]  " ++ lowRes

  if highRes == "High altitude: 800.0" &&
     lowRes == "TERRAIN ALERT at alt=350.0 (threshold=350.0)"
    then putStrLn "  ✓ templateSwitch single parameterized template tests passed!"
    else error "templateSwitch tests failed!"

  ------------------------------------------------------------------------------
  -- 4. Test templateBranch: Single Branching (Either b c)
  ------------------------------------------------------------------------------
  putStrLn "\n--- [4/5] Testing templateBranch (Either b c) ---"

  let branchEitherInit :: TightRope '[] (->) Int (Either String Double)
      branchEitherInit = mkRope \_ n ->
        if n > 0
          then Left ("Positive number: " ++ show n)
          else Right (fromIntegral (-n) * 1.5)

      branchTemplate :: TightRope '[] (->) (Double, Int) String
      branchTemplate = mkRope \_ (scaled, original) ->
        "Negative transformed: scaled=" ++ show scaled ++ ", original=" ++ show original

      singleBranched = templateBranch branchEitherInit branchTemplate

  let bPosRes = runPure singleBranched 10
      bNegRes = runPure singleBranched (-4)

  putStrLn $ "  [Pos]      " ++ bPosRes
  putStrLn $ "  [Neg]      " ++ bNegRes

  if bPosRes == "Positive number: 10" &&
     bNegRes == "Negative transformed: scaled=6.0, original=-4"
    then putStrLn "  ✓ templateBranch tests passed!"
    else error "templateBranch tests failed!"

  ------------------------------------------------------------------------------
  -- 5. Test switchCore & Monadic Arrows (Kleisli IO)
  ------------------------------------------------------------------------------
  putStrLn "\n--- [5/5] Testing Monadic Switch with Kleisli IO ---"

  let kleisliInit :: Kleisli IO Command (String, Maybe (Variant FlightModes))
      kleisliInit = Kleisli \cmd -> do
        case cmd of
          IdleCmd -> pure ("IO Idle", Nothing)
          DockCmd st -> pure ("IO Switching", Just (injectVariant (Field @"docking") (REC { target = st })))
          CruiseCmd sp -> pure ("IO Switching", Just (injectVariant (Field @"cruising") (REC { speed = sp })))
          LandCmd -> pure ("IO Switching", Just (injectVariant (Field @"landing") emptyRecord))

  let kleisliTemplates :: Record (TemplateBranchesCore (Kleisli IO) Command String FlightModes)
      kleisliTemplates = REC
        { docking = Kleisli \(REC { target = tgt }, _cmd) ->
            pure ("IO Docking initiated to: " ++ tgt)
        , cruising = Kleisli \(REC { speed = spd }, _cmd) ->
            pure ("IO Cruising set to: " ++ show spd ++ " knots")
        , landing = Kleisli \(_payload, _cmd) ->
            pure "IO Landing sequence executed"
        }

  let kleisliSwitched = switchCore kleisliInit kleisliTemplates

  kResIdle <- runKleisli kleisliSwitched IdleCmd
  kResDock <- runKleisli kleisliSwitched (DockCmd "Lunar-Base-Alpha")
  kResCruise <- runKleisli kleisliSwitched (CruiseCmd 600)
  kResLand <- runKleisli kleisliSwitched LandCmd

  putStrLn $ "  [IO Idle]   " ++ kResIdle
  putStrLn $ "  [IO Dock]   " ++ kResDock
  putStrLn $ "  [IO Cruise] " ++ kResCruise
  putStrLn $ "  [IO Land]   " ++ kResLand

  if kResIdle == "IO Idle" &&
     kResDock == "IO Docking initiated to: Lunar-Base-Alpha" &&
     kResCruise == "IO Cruising set to: 600 knots" &&
     kResLand == "IO Landing sequence executed"
    then putStrLn "  ✓ Monadic switchCore with Kleisli IO passed!"
    else error "Monadic switchCore failed!"

  putStrLn "\n========================================================================"
  putStrLn "✓ ALL Kernmantle parameterized open variant switch tests PASSED cleanly!"
  putStrLn "========================================================================"
