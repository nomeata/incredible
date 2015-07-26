{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as M

import ShapeChecks
import Types
import TaggedMap

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ cycleTests
    , escapedHypothesesTests
    ]

cycleTests = testGroup "Cycle detection"
  [ testCase "cycle"    $ findCycles oneBlockLogic proofWithCycle @?= [["c"]]
  , testCase "no cycle" $ findCycles oneBlockLogic proofWithoutCycle @?= []
  ]
  where

escapedHypothesesTests = testGroup "Escaped Hypotheses"
  [ testCase "direct"    $ findEscapedHypotheses impILogic directEscape @?= [["c"]]
  , testCase "indirect"  $ findEscapedHypotheses impILogic indirectEscape @?= [["c", "c2"]]
  , testCase "ok"        $ findEscapedHypotheses impILogic noEscape @?= []
  ]


oneBlockLogic = Context
    (M.singleton "r" (Rule (M.fromList [("in", Port PTAssumption "foo"), ("out", Port PTConclusion "foo")])))

proofWithCycle = Proof
    (M.singleton "b" (Block "r"))
    (M.singleton "c" (Connection (BlockPort "b" "out") (BlockPort "b" "in")))

proofWithoutCycle = Proof
    (M.singleton "b" (Block "r"))
    (M.singleton "c" (Connection (BlockPort "b" "out") (ConclusionPort 1)))

impILogic = Context
    (M.singleton "impI" (Rule (M.fromList [("in", Port PTAssumption "B"), ("out", Port PTConclusion "A→B"), ("hyp", Port (PTLocalHyp "in") "A")])))

directEscape = Proof
    (M.singleton "b" (Block "impI"))
    (M.singleton "c" (Connection (BlockPort "b" "hyp") (ConclusionPort 1)))

noEscape = Proof
    (M.singleton "b" (Block "impI"))
    (M.fromList
        [ ("c",  (Connection (BlockPort "b" "hyp") (BlockPort "b" "in")))
        , ("c2", (Connection (BlockPort "b" "out") (ConclusionPort 1)))
        ])

indirectEscape = Proof
    (M.fromList [("b", Block "impI"), ("b2", Block "impI")])
    (M.fromList
        [ ("c",  (Connection (BlockPort "b" "hyp") (BlockPort "b2" "in")))
        , ("c2", (Connection (BlockPort "b2" "out") (ConclusionPort 1)))
        ])

