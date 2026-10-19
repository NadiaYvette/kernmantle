{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Example and test for Control.Kernmantle.Rope.ReaderSoup:
-- Demonstrates multi-context Reader soup arrows backed by Polytropos flat Records.
module Main where

import Control.Arrow
import Control.Category
import Control.Monad (unless)
import Prelude hiding (id, (.))

import Control.Kernmantle.Rope
import Control.Kernmantle.Rope.ReaderSoup

type AppEnv =
  '[ "db" ':= String
   , "port" ':= Int
   , "debug" ':= Bool
   ]

type SubEnv =
  '[ "db" ':= String
   , "port" ':= Int
   ]

mkAppEnv :: String -> Int -> Bool -> Record AppEnv
mkAppEnv db p d =
  insertField #db db
    $ insertField #port p
    $ insertField #debug d
    $ emptyRecord

-- | Arrow pipeline querying multiple labeled contexts from a single soup strand.
appPipeline :: AnyRopeWith '[ "soup" ::: ReaderSoupEff AppEnv ] '[Arrow] () (String, Int, Bool, Int)
appPipeline = proc () -> do
  dbUrl <- askSoup @AppEnv #soup #db -< ()
  p     <- askSoup @AppEnv #soup #port -< ()
  dbg   <- askSoup @AppEnv #soup #debug -< ()
  len   <- asksSoup @AppEnv #soup #db length -< ()
  returnA -< (dbUrl, p, dbg, len)

-- | Sub-routine requiring only the subset of contexts in SubEnv.
subPipeline :: AnyRopeWith '[ "soup" ::: ReaderSoupEff SubEnv ] '[Arrow] () String
subPipeline = proc () -> do
  dbUrl <- askSoup @SubEnv #soup #db -< ()
  p     <- askSoup @SubEnv #soup #port -< ()
  returnA -< dbUrl ++ ":" ++ show p

main :: IO ()
main = do
  putStrLn "Testing Kernmantle ReaderSoup integration..."
  let env = mkAppEnv "postgres://localhost:5432/prod" 5432 True

  -- 1. Run main pipeline with full environment
  let runner1 = appPipeline
                  & loosen
                  & weaveSoup #soup env
                  & untwine
      (db, port, debug, len) = runner1 ()

  putStrLn $ "  db: " ++ db
  putStrLn $ "  port: " ++ show port
  putStrLn $ "  debug: " ++ show debug
  putStrLn $ "  length: " ++ show len

  unless (db == "postgres://localhost:5432/prod") $
    error "Assertion failed: db mismatch"
  unless (port == 5432) $
    error "Assertion failed: port mismatch"
  unless (debug == True) $
    error "Assertion failed: debug mismatch"
  unless (len == 30) $
    error "Assertion failed: len mismatch"

  -- 2. Run sub-routine narrowed from full environment via filteringSoup
  let runner2 = subPipeline
                  & loosen
                  & filteringSoup #soup env
                  & untwine
      subRes = runner2 ()

  putStrLn $ "  subRes: " ++ subRes
  unless (subRes == "postgres://localhost:5432/prod:5432") $
    error "Assertion failed: subRes mismatch"

  -- 3. Run Kleisli monadic soup weaving
  let runnerK = appPipeline
                  & loosen
                  & weaveSoupK #soup env
                  & untwine
  (kDb, kPort, kDebug, kLen) <- runKleisli runnerK ()
  unless (kDb == "postgres://localhost:5432/prod" && kPort == 5432 && kDebug && kLen == 30) $
    error "Assertion failed: Kleisli soup mismatch"

  putStrLn "All Kernmantle ReaderSoup tests passed successfully!"
