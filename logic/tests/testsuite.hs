{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
-- Once https://github.com/feuerbach/tasty/pull/115 is merged:
-- import Test.Tasty.Runners
import qualified Data.Map as M
import Data.String
import Control.Monad
import Data.Maybe
import Unbound.LocallyNameless

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
  [ testCase "unify pred" $
    assertUnifies ["P"] [("P(x)", "Q(R(x))")]
        [("P", absTerm ["x"] "Q(R(x))")]
  , testCase "unify with ∀" $
    assertUnifies ["R"] [("R", "∀z.P(z)")]
        [("R", noAbs "∀z.P(z)")]
  , testCase "unify pred under ∀" $
    assertUnifies ["P","Z"] [("∀z.P(z)", "∀z.Q(R(z))")]
        [("P", absTerm ["x"] "Q(R(x))")]
  , testCase "unify subst under ∀" $
    assertUnifies ["P","V"] [("P(x)", "Q(R(x))"), ("∀z.P(z)", "∀z.Q(V(z))")]
        [("P", absTerm ["x"] "Q(R(x))"), ("V", absTerm ["x"] "R(x)")]
  {- TODO
  , testCase "∀ escape" $
    assertUnifies ["P","V"] [("∀y.P(x)", "∀y.Q(y)")]
        []
  -}
  , testCase "regression1" $
    runLFreshMT (addEquationToBinding ["A"] emptyBinding ("f(A,not(A))", "f(not(not(A)),not(not(A)))"::Proposition)) @?= Nothing
  , expectFail $
    testCase "regression2" $
    assertUnifies
        -- P1: exE
        -- P2: allE
        -- P3: exI
        -- P4: allI
        ["P1","P2","P3","P4","A","B","y1","y2"]
        [ "A"        >: "∃x.P1(x)"
        , "P1(c)"    >: "∀x.P2(x)"
        , "P2(y1)"   >: "P3(y2)"
        , "∃x.P3(x)" >: "P4(c2)"
        , "∀x.P4(x)" >: "B"
        , "A→B"      >: "(∃y.∀x.P(x,y))→(∀x.∃y.P(x,y))"
        ]
        [ "A" >: noAbs "∃y.P1(y)"
        , "B" >: noAbs "∀x.P4(x)"
        , "P1" >: absTerm ["y"] "∀x.P(x,y)"
        , "P2" >: absTerm ["x"] "P(x,c)"
        , "P3" >: absTerm ["x"] "P(x,c)"
        , "P4" >: absTerm ["x"] "∃y.P(x,y)"
        ]
  ]

assertUnifies vars eqns expt = do
    let expt' = M.fromList expt
    let res = unifyEquationsWherePossible vars eqns
    unless (res == expt') $ do
        assertFailure $ unlines $
            ["expected: "] ++
            map (\(k,v) -> "    " ++ show k ++ ": " ++ printAbs v) (M.toList expt') ++
            [" but got: "] ++
            map (\(k,v) -> "    " ++ show k ++ ": " ++ printAbs v) (M.toList res)

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
    result = runLFreshMT $ addEquationToBinding [] emptyBinding (prop1, prop2)
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

(>:) :: a -> b -> (a, b)
(>:) = (,)

-- Remove if https://github.com/feuerbach/tasty/pull/115 is merged
expectFail = const $ testGroup "ignored test case" []
