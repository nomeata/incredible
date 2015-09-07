{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.Map as M
import Data.String
import Control.Monad
import Data.Maybe

import ShapeChecks
import Types
import TaggedMap
import Propositions
import LabelConnections
import Unification


-- Hack for here
instance IsString Proposition where
    fromString = readTerm
instance IsString Var where
    fromString = string2Name
{-
instance IsString GroundTerm where
    fromString = either (error . show) id . parseGroundTerm
-}

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ parserTests
    , cycleTests
    , escapedHypothesesTests
    , unconnectedGoalsTests
    , labelConectionsTests
    , unificationTests
    ]

parserTests = testGroup "Parsers"
  [ testCase ("can parse " ++ s) $ assertRight $ parseTerm s
  | s <- [ "∀x.P(x)"
         , "!x.P(x)"
         ]
  ]

cycleTests = testGroup "Cycle detection"
  [ testCase "cycle"    $ findCycles oneBlockLogic proofWithCycle @?= [["c"]]
  , testCase "no cycle" $ findCycles oneBlockLogic proofWithoutCycle @?= []
  ]
  where

escapedHypothesesTests = testGroup "Escaped hypotheses"
  [ testCase "direct"    $ findEscapedHypotheses impILogic directEscape @?= [["c"]]
  , testCase "indirect"  $ findEscapedHypotheses impILogic indirectEscape @?= [["c", "c2"]]
  , testCase "ok"        $ findEscapedHypotheses impILogic noEscape @?= []
  ]

unconnectedGoalsTests = testGroup "Unsolved goals"
  [ testCase "empty"     $ findUnconnectedGoals impILogic simpleTask emptyProof @?= [ConclusionPort 1]
  , testCase "indirect"  $ findUnconnectedGoals impILogic simpleTask partialProof @?= [BlockPort "b" "in"]
  , testCase "complete"  $ findUnconnectedGoals impILogic simpleTask completeProof @?= []
  ]

labelConectionsTests = testGroup "Label Connections"
  [ testCase "complete" $ labelConnections impILogic simpleTask completeProof @?=
        M.fromList [("c1",Ok $ Var "Prop"),("c2", Ok $ Symb (Var "→") [Var "Prop",Var "Prop"])]
  ]

unificationTests = testGroup "Unification tests"
  [ testCase "regression1" $
    addEquationToBinding ["A"] emptyBinding ("f(A,not(A))", "f(not(not(A)),not(not(A)))"::Proposition) @?= Nothing
  ]

oneBlockLogic = Context
    (M.singleton "r" (Rule ["A"] ["A"] (M.fromList [("in", Port PTAssumption "A"), ("out", Port PTConclusion "A")])))

proofWithCycle = Proof
    (M.singleton "b" (Block "r"))
    (M.singleton "c" (Connection (BlockPort "b" "out") (BlockPort "b" "in")))

proofWithoutCycle = Proof
    (M.singleton "b" (Block "r"))
    (M.singleton "c" (Connection (BlockPort "b" "out") (ConclusionPort 1)))

impILogic = Context
    (M.fromList
        [ ("impI", Rule ["A", "B"] ["A", "B"] (M.fromList
            [ ("in",  Port PTAssumption "B")
            , ("out", Port PTConclusion "A→B")
            , ("hyp", Port (PTLocalHyp "in") "A")
            ]))
        ]
    )

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

simpleTask = Task [] ["Prop→Prop"]

emptyProof = Proof M.empty M.empty

partialProof = Proof
    (M.fromList [("b", Block "impI")])
    (M.fromList [("c", (Connection (BlockPort "b" "out") (ConclusionPort 1)))])
completeProof = Proof
    (M.fromList [("b", Block "impI")])
    (M.fromList [ ("c1", (Connection (BlockPort "b" "hyp") (BlockPort "b" "in")))
                , ("c2", (Connection (BlockPort "b" "out") (ConclusionPort 1)))])

-- Quickcheck tests

-- Nice try, but random equations are not equal likely enough.
unificationUnifiesProp :: Equality -> Property
unificationUnifiesProp (prop1, prop2) =
    isJust result ==>
    counterexample txt (not (bindingOk bind) || prop1' == prop2')
  where
    result = addEquationToBinding [] emptyBinding (prop1, prop2)
    bind = fromJust result
    prop1' = applyBinding bind prop1
    prop2' = applyBinding bind prop2

    txt = "Input was " ++ printTerm prop1 ++ "=" ++ printTerm prop2


instance Arbitrary Proposition where
    arbitrary = sized genProp

genProp :: Int -> Gen Proposition
genProp 0 = elements $
    map Var ["A","B","C","D","E"] ++
    [Symb (Var "Prop1") [], Symb (Var "Prop2") []]
genProp n = do
    (name,arity) <- elements [("∧",2), ("∨",2), ("not",1)]
    args <- replicateM arity $ do
        n' <- choose (0,n`div`2)
        genProp n'
    return $ Symb (Var name) args

assertRight :: Either String a -> Assertion
assertRight = either assertFailure (const (return ()))
