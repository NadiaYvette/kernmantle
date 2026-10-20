{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Prelude hiding (id, (.))

import Control.Kernmantle.Rope
import Control.Kernmantle.Rope.Fusion
import Control.Kernmantle.Rope.Internal (Rec(..), pattern RNil)
import Control.Kernmantle.Rope.Variant
import Polytropos
import Polytropos.Core

-- | An open variant of effect requests.
type AppEvents = Variant
  '[ "ping" ':= '["seq" ':= Int]
   , "msg"  ':= '["text" ':= String]
   ]

-- | Arrow implementation routing variant requests via jump table.
main :: IO ()
main = do
  putStrLn "=== Testing Kernmantle Variant Router and Fusion ==="

  -- 1. Test O(1) Variant Jump Table Routing
  let router :: VariantRouter (->) () String
      router = makeRouter
        [ \rep () ->
            let s :: Int = getSlot 0 rep
            in "Pong sequence: " ++ show s
        , \rep () ->
            let t :: String = getSlot 0 rep
            in "Received message: " ++ t
        ]

  let eventPing :: AppEvents
      eventPing = injectVariant (Field @"ping") (REC { seq = 42 :: Int })

      eventMsg :: AppEvents
      eventMsg = injectVariant (Field @"msg") (REC { text = "Hello Kernmantle!" })

  let ropeRouter :: AppEvents -> TightRope '[] (->) () String
      ropeRouter = routeVariantRope router
      ropePing = ropeRouter eventPing
      ropeMsg  = ropeRouter eventMsg

  let runPure (rope :: TightRope '[] (->) () String) = runRope rope RNil ()
      resPing = runPure ropePing
      resMsg  = runPure ropeMsg

  putStrLn resPing
  putStrLn resMsg

  if resPing == "Pong sequence: 42" && resMsg == "Received message: Hello Kernmantle!"
    then putStrLn "✓ Variant router tests passed!"
    else error "Variant router test failed!"

  -- 2. Test Type-Safe branchRope Multi-Way Branching
  let branchHandlers = REC
        { ping = \(REC { seq = s }) -> mkRope \_ -> \() -> "Ping branched: " ++ show s
        , msg  = \(REC { text = t }) -> mkRope \_ -> \() -> "Msg branched: " ++ t
        }

  let branchedPing = branchRope branchHandlers eventPing
      branchedMsg  = branchRope branchHandlers eventMsg

  let bResPing = runPure branchedPing
      bResMsg  = runPure branchedMsg

  putStrLn bResPing
  putStrLn bResMsg

  if bResPing == "Ping branched: 42" && bResMsg == "Msg branched: Hello Kernmantle!"
    then putStrLn "✓ branchRope tests passed!"
    else error "branchRope test failed!"

  -- 3. Test Coyoneda Fusion
  let baseRope :: TightRope '[] (->) () Int
      baseRope = mkRope \_ () -> 100

      fusedRope :: TightRope '[] (->) () String
      fusedRope = fuseRope
        ( fuseRopeMap (\s -> "[" ++ s ++ "]")
        $ fuseRopeMap (\n -> "Score: " ++ show n)
        $ fuseRopeMap (+ 25)
        $ liftCoyoneda baseRope
        )

      fusedRes = runPure fusedRope

  putStrLn fusedRes

  if fusedRes == "[Score: 125]"
    then putStrLn "✓ Coyoneda fusion tests passed!"
    else error "Coyoneda fusion test failed!"

  putStrLn "All Kernmantle variant routing and fusion tests completed successfully."
