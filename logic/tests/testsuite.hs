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
import Unification
import ConvertAeson
import Examples
import Entry
import Analysis
import Rules
import ProofGraph



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
    custom_rules <- readDirectoryOfYamlFiles "../examples/custom_rules"
    custom_rules_out <- readDirectoryOfYamlFiles "../examples/custom_rules_out"
    defaultMain $ testGroup "Tests"
        [ parserTests
        , cycleTests
        , escapedHypothesesTests
        , unconnectedGoalsTests
        , unificationTests
        , ruleExportTest
        , exampleTests examples analyses
        , ruleDerivationTests examples custom_rules custom_rules_out
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
             , ("(A↑B)↑C", "(A↑B)↑C")
             , ("A↑(B↑C)", "A↑(B↑C)")
             ]
  ]

assertParse f t = do
    t' <- assertRight (parseTerm f)
    assertEqual "pretty-printed:" t (printTerm t')


cycleTests = testGroup "Cycle detection"
  [ testCase "cycle"    $ f proofWithCycle @?= [["c"]]
  , testCase "no cycle" $ f proofWithoutCycle @?= []
  ]
 where f proof = findCycles (proof2Graph oneBlockLogic proof)

escapedHypothesesTests = testGroup "Escaped hypotheses"
  [ testCase "direct"    $ f directEscape @?= [["c"]]
  , testCase "indirect"  $ f indirectEscape @?= [["c", "c2"]]
  , testCase "ok"        $ f noEscape @?= []
  , testCase "tricky"    $ f trickyEscape @?= [["c", "c5", "c3", "c1"]]
  ]
 where f proof = findEscapedHypotheses impILogic proof (proof2Graph impILogic proof)

unconnectedGoalsTests = testGroup "Unsolved goals"
  [ testCase "empty"     $ f emptyProof @?= [BlockPort "c" "in"]
  , testCase "dangling"  $ f danglingProof @?= [BlockPort "c" "in"]
  , testCase "indirect"  $ f partialProof @?= [BlockPort "b" "in"]
  , testCase "complete"  $ f completeProof @?= []
  ]
 where f proof = findUnconnectedGoals (proof2Graph impILogic proof)


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
  , testCase "better unification" $
    assertUnifies
        [ "P", "y" ]
        [ "P(V c)" >: "f(y)" ]
        [ "P" >: absTerm ["x"] "f(y)"]
  , testCase "basic pattern-matching" $
    assertUnifies
        [ "P1", "P2" ]
        [ "P1(V c1)" >: "∀x.P2(V c1,x)"]
        [ "P1" >: absTerm ["c1"] "∀x.P2(c1,x)"]
  , testCase "pattern vs. non-pattern" $
    assertUnifies
        [ "P1", "P2", "y"]
        [ "P1(V c)" >: "P2(V c, y(V c))" ]
        [ "P1" >: absTerm ["x"] "P2(V x, y(V x))"]
  , testCase "non-pattern vs. pattern" $
    assertUnifies
        [ "P1", "P2", "y"]
        [ "P2(V c, y(V c))" >: "P1(V c)" ]
        [ "P1" >: absTerm ["x"] "P2(V x, y(V x))"]

  ]

ruleExportTest = testGroup "Rule export"
  [ testCase "full call" $ assertEqualValues (toJSON $ deriveRule oneBlockLogic oneBlockProof sp') $ toJSON renamedRule
  ]
  where
    graph = proof2Graph oneBlockLogic oneBlockProof
    sp = prepare oneBlockLogic oneBlockProof graph
    (sp', _) = unifyScopedProof oneBlockProof sp

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

oneBlockLogic :: Context
oneBlockLogic = Context
    (M.singleton "r" (Rule ["A"] ["A"] (M.fromList ["in" >: Port PTAssumption "A" [], "out" >: Port PTConclusion "A" []])))

oneBlockProof = Proof
    (M.singleton "b" (Block 1 "r"))
    M.empty

renamedRule = Rule ["A"] ["A"] (M.fromList ["port1" >: Port PTAssumption "A" [], "port2" >: Port PTConclusion "A" []])

f --> t = Connection 1 (Just f) (Just t)

proofWithCycle = Proof
    (M.singleton "b" (Block 1 "r"))
    (M.singleton "c" (BlockPort "b" "out" --> BlockPort "b" "in"))

proofWithoutCycle = Proof
    (M.fromList ["c" >: ConclusionBlock 0 "P→P", "b" >: (Block 1 "r")])
    (M.singleton "c" (BlockPort "b" "out" --> BlockPort "c" "in"))

impILogic :: Context
impILogic = Context
    (M.fromList
        [ ("impI", Rule f f (M.fromList
            [ "in"  >: Port PTAssumption "B" []
            , "out" >: Port PTConclusion "A→B" []
            , "hyp" >: Port (PTLocalHyp "in") "A" []
            ]))
        , ("two2two", Rule f f (M.fromList
            [ "in1"  >: Port PTAssumption "A" []
            , "in2"  >: Port PTAssumption "B" []
            , "out1" >: Port PTConclusion "A" []
            , "out2" >: Port PTConclusion "B" []
            ]))
        ]
    )
  where f = ["A","B"]


directEscape = Proof
    (M.fromList ["c" >: ConclusionBlock 0 "P→P", "b" >: Block 1 "impI"])
    (M.singleton "c" (BlockPort "b" "hyp" --> BlockPort "c" "in"))

noEscape = Proof
    (M.fromList ["c" >: ConclusionBlock 0 "P→P", "b" >: Block 1 "impI"])
    (M.fromList
        [ ("c",  BlockPort "b" "hyp" --> BlockPort "b" "in")
        , ("c2", BlockPort "b" "out" --> BlockPort "c" "in")
        ])

indirectEscape = Proof
    (M.fromList ["c" >: ConclusionBlock 0 "P→P", "b" >: Block 1 "impI", "b2" >: Block 2 "impI"])
    (M.fromList
        [ ("c",  BlockPort "b" "hyp" --> BlockPort "b2" "in")
        , ("c2", BlockPort "b2" "out" --> BlockPort "c" "in")
        ])

trickyEscape = Proof
    (M.fromList
        [ "c"  >: ConclusionBlock 0 "P→P"
        , "i"  >: Block 1 "impI"
        , "b1" >: Block 2 "two2two"
        , "b2" >: Block 2 "two2two"
        , "b3" >: Block 2 "two2two"
        , "b4" >: Block 2 "two2two"
        ])
    (M.fromList
        [ ("c",  BlockPort "i"  "hyp"  --> BlockPort "b4" "in1")
        , ("c1", BlockPort "i"  "out"  --> BlockPort "b1" "in1")
        , ("c2", BlockPort "b1" "out1" --> BlockPort "b3" "in1")
        , ("c3", BlockPort "b1" "out2" --> BlockPort "b2" "in1")
        , ("c4", BlockPort "b2" "out1" --> BlockPort "b3" "in2")
        , ("c5", BlockPort "b2" "out2" --> BlockPort "b4" "in2")
        ])

emptyProof = Proof
    (M.fromList [("c", ConclusionBlock 0 "Prop→Prop")])
    (M.fromList [])

danglingProof = Proof
    (M.fromList [("c", ConclusionBlock 0 "Prop→Prop")])
    (M.fromList [("c", Connection 0 Nothing (Just (BlockPort "c" "in")))])

partialProof = Proof
    (M.fromList [("c", ConclusionBlock 0 "Prop→Prop"),("b", Block 1 "impI")])
    (M.fromList [("c", (BlockPort "b" "out" --> BlockPort "c" "in"))])
completeProof = Proof
    (M.fromList [("c", ConclusionBlock 0 "Prop→Prop"),("b", Block 1 "impI")])
    (M.fromList [ ("c1", (BlockPort "b" "hyp" --> BlockPort "b" "in"))
                , ("c2", (BlockPort "b" "out" --> BlockPort "c" "in"))])

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
    [ (if isBad proof then expectFail else id) $
      testCase name $ do
        result <- assertRight $ incredibleLogic (fromJSON' logic) (fromJSON' proof)
        assertEqualValues (toJSON result) analysis
    | (name, analysis) <- M.toList exampleAnalyses
    , let proof = exampleProofs ! name
    , let logic = exampleLogics ! (proof !!! "logic")
    ]
  where
    value !!! field = either error id $ parseEither (withObject "" (.: field)) value
    isBad value = either error id $ parseEither (withObject "" (\o -> o .:? "expectedBad" .!= False)) value

ruleDerivationTests :: Examples -> M.Map String Value -> M.Map String Value -> TestTree
ruleDerivationTests (Examples {..}) inputs outputs = testGroup "Rule derivation"
    [ testCase name $ do
        result <- assertRight $ incredibleNewRule (fromJSON' logic) (fromJSON' input)
        assertEqualValues (toJSON result) expected
    | (name, input) <- M.toList inputs
    , let logic = exampleLogics ! (input !!! "logic")
    , let expected = outputs M.! name
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
