-- | The data types for the UI→Haskell interface. Roughly corresponds to the
-- specification in SPEC.md.
module Types
    ( module Types
    , Var
    )
    where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tagged

import Propositions
import Unification (UnificationResult(..))

-- We might want to look into other implementation of key-value maps
type Key k = Tagged k String
type KMap v = M.Map (Key v) v
type KSet v = S.Set (Key v)

-- This is different from the specification:
--  - There, we have a list of rules with an id.
--  - Here, we have a map from id to rule.
-- The Logic does not worry about the order, so a map interface with efficient
-- lookup is nicer.
data Rule = Rule
 { localVars :: [Var]
 , freeVars :: [Var] -- Subset of the local vars!
 , ports :: KMap Port
 }
 deriving Show

data PortType = PTAssumption | PTConclusion | PTLocalHyp (Key Port)
 deriving Show

isPortTypeOut :: PortType -> Bool
isPortTypeOut PTConclusion = True
isPortTypeOut (PTLocalHyp _) = True
isPortTypeOut PTAssumption = False

isPortTypeIn :: PortType -> Bool
isPortTypeIn PTConclusion = False
isPortTypeIn (PTLocalHyp _) = False
isPortTypeIn PTAssumption = True

data Port = Port
 { portType :: PortType
 , portProp :: Proposition
 , portScopes :: [Var]
 }
 deriving Show

data Context = Context
 { ctxtRules :: KMap Rule
 }
 deriving Show



type BlockNum = Int

data Block
    = AnnotationBlock BlockNum Proposition
      -- ^ A block with an annotation (no associated rule)
    | AssumptionBlock BlockNum Proposition
      -- ^ A block representing an assumption
    | ConclusionBlock BlockNum Proposition
      -- ^ A block representing a conclusion
    | Block BlockNum (Key Rule)
      -- ^ A normal with block with a rule
 deriving Show

data PortSpec = BlockPort (Key Block) (Key Port)
 deriving (Eq, Ord, Show)

data Connection = Connection
 { connSortKey :: Integer -- Put sort key first, for a convenient Ord instance
 , connFrom :: Maybe PortSpec
 , connTo :: Maybe PortSpec
 }
 deriving (Eq, Ord, Show)

data Proof = Proof
 { blocks :: KMap Block
 , connections :: KMap Connection
 }
 deriving Show

badResult :: UnificationResult -> Bool
badResult Solved = False
badResult Failed = True
badResult Dunno  = True

data Analysis = Analysis
 { connectionStatus :: M.Map (Key Connection) UnificationResult
 , portLabels :: M.Map PortSpec Term
 , unconnectedGoals :: [PortSpec]
 , cycles :: [Cycle]
 , escapedHypotheses :: [Path]
 , qed :: Bool
 }
 deriving Show

type Cycle = [Key Connection]
type Path = [Key Connection]

-- Various accessors

blockNum :: Block -> BlockNum
blockNum (AnnotationBlock n _) = n
blockNum (AssumptionBlock n _) = n
blockNum (ConclusionBlock n _) = n
blockNum (Block n _) = n

block2Rule :: Context -> Block -> Rule
block2Rule ctxt (Block _ rule) = ctxtRules ctxt M.! rule
block2Rule _    (AnnotationBlock _ prop) = annotationRule prop
block2Rule _    (AssumptionBlock _ prop) = assumptionRule prop
block2Rule _    (ConclusionBlock _ prop) = conclusionRule prop

annotationRule :: Proposition -> Rule
annotationRule prop = Rule
    { localVars = []
    , freeVars = []
    , ports = M.fromList
        [ (fakePortIn, Port
            { portType = PTAssumption
            , portProp = prop
            , portScopes = []
            })
        , (fakePortOut, Port
            { portType = PTConclusion
            , portProp = prop
            , portScopes = []
            })
        ]
    }

assumptionRule :: Proposition -> Rule
assumptionRule prop = Rule
    { localVars = []
    , freeVars = []
    , ports = M.fromList
        [(fakePortOut, Port
            { portType = PTConclusion
            , portProp = prop
            , portScopes = []
            })
        ]
    }

conclusionRule :: Proposition -> Rule
conclusionRule prop = Rule
    { localVars = []
    , freeVars = []
    , ports = M.fromList
        [(fakePortIn, Port
            { portType = PTAssumption
            , portProp = prop
            , portScopes = []
            })
        ]
    }

fakePortIn :: Key Port
fakePortIn = Tagged "in"
fakePortOut :: Key Port
fakePortOut = Tagged "out"

