{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M

import Entry
import Types
import TaggedMap

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [cycleTests]

cycleTests = testGroup "Cycle detection"
  [ testCase "cycle"    $ findCycles oneBlockLogic proofWithCycle @?= [["c"]] 
  , testCase "no cycle" $ findCycles oneBlockLogic proofWithoutCycle @?= [] 
  ]
  where
    oneBlockLogic =
        Context (M.singleton "r" (Rule (M.fromList [("in", Port PTAssumption "foo"), ("out", Port PTConclusion "foo")])))
    proofWithCycle = Proof (M.singleton "b" (Block "r")) (M.singleton "c" (Connection (BlockPort "b" "out") (BlockPort "b" "in")))
    proofWithoutCycle = Proof (M.singleton "b" (Block "r")) (M.singleton "c" (Connection (BlockPort "b" "out") (ConclusionPort 1)))
