{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, TupleSections, RecordWildCards, StandaloneDeriving #-}
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.ExpectedFailure
import qualified Data.Map as M
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Map ((!))
import Data.String
import Control.Monad
import Data.Maybe
import Unbound.LocallyNameless
import Control.Arrow
import Data.Aeson.Types
import qualified Data.Yaml

import ShapeChecks
import Types
import TaggedMap
import Propositions
import LabelConnections
import Unification
import ConvertAeson
import Examples
import Entry
import Analysis
import Rules



-- Hack for here
deriving instance Eq Rule
deriving instance Eq Port
deriving instance Eq PortType

instance IsString Proposition where
    fromString = readTerm
instance IsString Var where
    fromString = string2Name
{-
instance IsString GroundTerm where
    fromString = either (error . show) id . parseGroundTerm
-}

main = do
    examples <- readExamples "../examples"
    -- analysis are not part of the examples, as the latter is also shipped to
    -- the client.
    analyses <- readDirectoryOfYamlFiles "../examples/results"
    defaultMain $ testGroup "Tests"
        [ parserTests
        , cycleTests
        , escapedHypothesesTests
        , unconnectedGoalsTests
        , labelConnectionsTests
        , unificationTests
        , ruleExportTest
        , exampleTests examples analyses
        ]

parserTests = testGroup "Parsers"
  [ testCase ("can parse " ++ f) $ assertParse f t
  | (f,t) <- [ ("∀x.P(x)", "∀x.P(x)")
             , ("!x.P(x)", "∀x.P(x)")
             , ("∃x.P(x)", "∃x.P(x)")
             , ("?x.P(x)", "∃x.P(x)")
             , ("¬¬P", "¬¬P")
             , ("~¬P", "¬¬P")
             , ("P->Q","P→Q")
             , ("P&Q", "P∧Q")
             , ("P|Q", "P∨Q")
             , ("A↑B", "A↑B")
             , ("(A↑B)↑C", "A↑B↑C")
             , ("A↑(B↑C)", "A↑(B↑C)")
             ]
  ]

assertParse f t = do
    t' <- assertRight (parseTerm f)
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

labelConnectionsTests = testGroup "Label Connections"
  [ testCase "complete" $ labelConnections completeProof sp final_bind unificationResults @?=
        M.fromList [("c1",Ok $ C "Prop"),("c2", Ok $ App (C "→") [C "Prop", C "Prop"])]
  ]
  where
    sp = prepare impILogic simpleTask completeProof
    (final_bind, unificationResults) = analyse completeProof sp

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
  , testCase "regression2" $
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
        , "P3" >: absTerm ["x"] "P(c2,x)"
        , "P4" >: absTerm ["x"] "∃y.P(x,y)"
        , "y1" >: "c2"
        , "y2" >: "c"
        ]
  , expectFail $
    testCase "better unification" $
    assertUnifies
        [ "P", "y" ]
        [ "P(c)" >: "f(y)" ] -- here, we can make some progress on P
        [ "P" >: absTerm ["x"] "f(y)"]
  , testCase "basic pattern-matching" $
    assertUnifies
        [ "P1", "P2" ]
        [ "P1(V c1)" >: "∀x.P2(V c1,x)"]
        [ "P1" >: absTerm ["c1"] "∀x.P2(c1,x)"]

  ]

ruleExportTest = testGroup "Rule export"
  [ testCase "full call" $ assertEqualValues (toJSON $ deriveRule oneBlockLogic oneBlockProof sp b) $ toJSON renamedRule
  ]
  where
    sp = prepare oneBlockLogic emptyTask oneBlockProof
    (b, _) = analyse oneBlockProof sp

assertUnifies :: [Var] -> [Equality] -> [(Var, Term)] -> Assertion
assertUnifies vars eqns expt = do
    let expt' = M.fromList $ map (second (const2Var vars)) expt
    let eqns' = map (both (const2Var vars)) eqns
    let (res,_) = unifyLiberally vars (map ((),) eqns')
    unless (res == expt') $ do
        assertFailure $ unlines $
            ["expected: "] ++
            map (\(k,v) -> "    " ++ show k ++ ": " ++ printTerm v) (M.toList expt') ++
            [" but got: "] ++
            map (\(k,v) -> "    " ++ show k ++ ": " ++ printTerm v) (M.toList res)

emptyTask :: Task
emptyTask = Task [] []

oneBlockLogic :: Context
oneBlockLogic = Context
    (M.singleton "r" (Rule ["A"] ["A"] (M.fromList ["in" >: Port PTAssumption "A" [], "out" >: Port PTConclusion "A" []])))

oneBlockProof = Proof
    (M.singleton "b" (Block 1 "r"))
    M.empty

renamedRule = Rule ["B"] ["B"] (M.fromList ["in1" >: Port PTAssumption "B" [], "in2" >: Port PTConclusion "B" []])

uniVars = ["B"]
scopeVars _ _ = []

blockProps :: BlockProps
blockProps = M.singleton "b" $ M.fromList ["in" >: (V "A"), "out" >: (V "A")]

bindingAB :: Bindings
bindingAB = M.singleton "A" (V "B")

proofWithCycle = Proof
    (M.singleton "b" (Block 1 "r"))
    (M.singleton "c" (Connection 1 (BlockPort "b" "out") (BlockPort "b" "in")))

proofWithoutCycle = Proof
    (M.singleton "b" (Block 1 "r"))
    (M.singleton "c" (Connection 1 (BlockPort "b" "out") (ConclusionPort 1)))

impILogic :: Context
impILogic = Context
    (M.fromList
        [ ("impI", Rule f f (M.fromList
            [ "in"  >: Port PTAssumption "B" []
            , "out" >: Port PTConclusion "A→B" []
            , "hyp" >: Port (PTLocalHyp "in") "A" []
            ]))
        ]
    )
  where f = ["A","B"]

directEscape = Proof
    (M.singleton "b" (Block 1 "impI"))
    (M.singleton "c" (Connection 1 (BlockPort "b" "hyp") (ConclusionPort 1)))

noEscape = Proof
    (M.singleton "b" (Block 1 "impI"))
    (M.fromList
        [ ("c",  (Connection 1 (BlockPort "b" "hyp") (BlockPort "b" "in")))
        , ("c2", (Connection 2 (BlockPort "b" "out") (ConclusionPort 1)))
        ])

indirectEscape = Proof
    (M.fromList [("b", Block 1 "impI"), ("b2", Block 2 "impI")])
    (M.fromList
        [ ("c",  (Connection 1 (BlockPort "b" "hyp") (BlockPort "b2" "in")))
        , ("c2", (Connection 2 (BlockPort "b2" "out") (ConclusionPort 1)))
        ])

simpleTask = Task [] ["Prop→Prop"]

emptyProof = Proof M.empty M.empty

partialProof = Proof
    (M.fromList [("b", Block 1 "impI")])
    (M.fromList [("c", (Connection 1 (BlockPort "b" "out") (ConclusionPort 1)))])
completeProof = Proof
    (M.fromList [("b", Block 1 "impI")])
    (M.fromList [ ("c1", (Connection 1 (BlockPort "b" "hyp") (BlockPort "b" "in")))
                , ("c2", (Connection 2 (BlockPort "b" "out") (ConclusionPort 1)))])

-- Quickcheck tests

{-
genProp :: Int -> Gen Proposition
genProp 0 = elements $
    map Var ["A","B","C","D","E"] ++
    [App (Var "Prop1") [], Symb (Var "Prop2") []]
genProp n = do
    (name,arity) <- elements [("∧",2), ("∨",2), ("not",1)]
    args <- replicateM arity $ do
        n' <- choose (0,n`div`2)
        genProp n'
    return $ App (Var name) args
-}

exampleTests :: Examples -> M.Map String Value -> TestTree
exampleTests (Examples {..}) exampleAnalyses = testGroup "Examples"
    [ testCase name $ do
        result <- assertRight $ incredibleLogic (fromJSON' logic) (fromJSON' task) (fromJSON' proof)
        assertEqualValues (toJSON result) analysis
    | (name, analysis) <- M.toList exampleAnalyses
    , let proof = exampleProofs ! name
    , let task  = exampleTasks  ! (proof !!! "task")
    , let logic = exampleLogics ! (task !!! "logic")
    ]
  where
    value !!! field = either error id $ parseEither (withObject "" (.: field)) value

assertEqualValues :: Value -> Value -> Assertion
assertEqualValues v expt = do
    unless (v == expt) $ do
        assertFailure $ unlines $
            ["expected: "] ++
            ( map ("    "++) . lines . T.unpack $ T.decodeUtf8 $ Data.Yaml.encode expt ) ++
            [" but got: "] ++
            ( map ("    "++) . lines . T.unpack $ T.decodeUtf8 $ Data.Yaml.encode v )


fromJSON' :: FromJSON a => Value -> a
fromJSON' = either error id . parseEither parseJSON

assertFailure' :: String -> IO a
assertFailure' s = assertFailure s >> return (error "unrechable")

assertRight :: Either String a -> IO a
assertRight = either assertFailure' return

(>:) :: a -> b -> (a, b)
(>:) = (,)

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f
