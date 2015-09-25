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

-- We might want to look into other implementation of key-value maps
type Key k = Tagged k String
type KMap v = M.Map (Key v) v
type KSet v = S.Set (Key v)

data Task = Task
 { tAssumptions :: [Term]
 , tConclusions :: [Term]
 }
 deriving Show

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
    | Block BlockNum (Key Rule)
      -- ^ A normal with block with a rule
 deriving Show

data PortSpec = NoPort | AssumptionPort Int | ConclusionPort Int | BlockPort (Key Block) (Key Port)
 deriving (Eq, Ord, Show)

data Connection = Connection
 { connSortKey :: Integer -- Put sort key first, for a convenient Ord instance
 , connFrom :: PortSpec
 , connTo :: PortSpec
 }
 deriving (Eq, Ord, Show)

data Proof = Proof
 { blocks :: KMap Block
 , connections :: KMap Connection
 }
 deriving Show

data ConnLabel = Unconnected | Ok Proposition | Mismatch Proposition Proposition | DunnoLabel Proposition Proposition
 deriving (Eq, Show)

badLabel :: ConnLabel -> Bool
badLabel (Mismatch {})   = True
badLabel (DunnoLabel {}) = True
badLabel (Ok {})         = False
badLabel Unconnected     = False

data Analysis = Analysis
 { connectionLabels :: M.Map (Key Connection) ConnLabel
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
blockNum (Block n _) = n

block2Rule :: Context -> Block -> Rule
block2Rule ctxt (Block _ rule) = ctxtRules ctxt M.! rule
block2Rule _    (AnnotationBlock _ prop) = annotationRule prop

annotationRule :: Proposition -> Rule
annotationRule prop = Rule
    { localVars = []
    , freeVars = []
    , ports = M.fromList
        [ (annotationPortIn, Port
            { portType = PTAssumption
            , portProp = prop
            , portScopes = []
            })
        , (annotationPortOut, Port
            { portType = PTConclusion
            , portProp = prop
            , portScopes = []
            })
        ]
    }

annotationPortIn :: Key Port
annotationPortIn = Tagged "in"
annotationPortOut :: Key Port
annotationPortOut = Tagged "out"

