{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, TupleSections #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.ExpectedFailure
import qualified Data.Map as M
import Data.String
import Control.Monad
import Data.Maybe
import Unbound.LocallyNameless
import Control.Arrow

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
  [ testCase ("can parse " ++ f) $ assertParse f t
  | (f,t) <- [ ("∀x.P(x)", "∀x.P(x)")
             , ("!x.P(x)", "∀x.P(x)")
             , ("∃x.P(x)", "∃x.P(x)")
             , ("?x.P(x)", "∃x.P(x)")
             ]
  ]

assertParse f t = do
    let r@(~(Right t')) = parseTerm f
    assertRight r
    assertEqual "pretty-printed:" t (printTerm t')


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
        M.fromList [("c1",Ok $ C "Prop"),("c2", Ok $ Symb (C "→") [C "Prop", C "Prop"])]
  ]

unificationTests = testGroup "Unification tests"
  [ testCase "unify pred" $
    assertUnifies ["P"] [("P(V x)", "Q(R(V x))")]
        [("P", absTerm ["x"] "Q(R(x))")]
  , testCase "unify with ∀" $
    assertUnifies ["R"] [("R", "∀z.P(z)")]
        [("R", "∀z.P(z)")]
  , testCase "unify pred under ∀" $
    assertUnifies ["P","Z"] [("∀z.P(z)", "∀z.Q(R(z))")]
        [("P", absTerm ["x"] "Q(R(x))")]
  , testCase "unify subst under ∀" $
    assertUnifies ["P","V"] [("P(V x)", "Q(R(V x))"), ("∀z.P(z)", "∀z.Q(V(z))")]
        [("P", absTerm ["x"] "Q(R(x))"), ("V", absTerm ["x"] "R(x)")]
  {- TODO
  , testCase "∀ escape" $
    assertUnifies ["P","V"] [("∀y.P(x)", "∀y.Q(y)")]
        []
  -}
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
        [ "A" >: "∃y.P1(y)"
        , "B" >: "∀x.P4(x)"
        , "P1" >: absTerm ["y"] "∀x.P(x,y)"
        , "P2" >: absTerm ["x"] "P(x,c)"
        , "P3" >: absTerm ["x"] "P(x,c)"
        , "P4" >: absTerm ["x"] "∃y.P(x,y)"
        ]
  , expectFail $
    testCase "quantifier order" $
    assertUnifies
        -- We can prove (∀y.∃x.P(x,y))→(∃x.P(x,x)) if we do not watch for quantifier order
        -- P1: allE
        -- P2: exE
        -- P3: exI
        ["P1","P2","P3","A","B","y1","y2"]
        [ "A"        >: "∀x.P1(x)"
        , "P1(y1)"   >: "∃x.P2(x)"
        , "P2(c)"    >: "P3(y2)"
        , "∃x.P3(x)" >: "B"
        , "A→B"      >: "(∀y.∃x.P(x,y))→(∃x.P(x,x))"
        ]
        [ "A" >: "∀x.P1(x)"
        , "B" >: "∃x.P3(x)"
        , "P1" >: absTerm ["y"] "∃x.P2(x)"
        , "P2" >: absTerm ["x"] "P3(y2)"
        , "P3" >: absTerm ["tmp"] "∃x.P(x,y1)"
        ]
  ]

assertUnifies :: [Var] -> [Equality] -> [(Var, Term)] -> Assertion
assertUnifies vars eqns expt = do
    let expt' = M.fromList expt
    let eqns' = map (both (const2Var vars)) eqns
    let (res,_) = unifyLiberally vars (map ((),) eqns')
    unless (res == expt') $ do
        assertFailure $ unlines $
            ["expected: "] ++
            map (\(k,v) -> "    " ++ show k ++ ": " ++ printTerm v) (M.toList expt') ++
            [" but got: "] ++
            map (\(k,v) -> "    " ++ show k ++ ": " ++ printTerm v) (M.toList res)

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
        [ ("impI", Rule f f (M.fromList
            [ ("in",  Port PTAssumption (const2Var f "B"))
            , ("out", Port PTConclusion (const2Var f "A→B"))
            , ("hyp", Port (PTLocalHyp "in") (const2Var f "A"))
            ]))
        ]
    )
  where f = ["A","B"]

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

{-
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
-}

assertRight :: Either String a -> Assertion
assertRight = either assertFailure (const (return ()))

(>:) :: a -> b -> (a, b)
(>:) = (,)

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f
