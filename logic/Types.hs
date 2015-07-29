-- | The data types for the UI→Haskell interface. Roughly corresponds to the
-- specification in SPEC.md.
module Types where

import qualified Data.Map as M
import Data.Tagged

import Propositions

-- We might want to look into other implementation of key-value maps
type Key k = Tagged k String
type KMap v = M.Map (Key v) v


data Task = Task
 { tAssumptions :: [GroundTerm]
 , tConclusions :: [GroundTerm]
 }
 deriving Show

-- This is different from the specification:
--  - There, we have a list of rules with an id.
--  - Here, we have a map from id to rule.
-- The Logic does not worry about the order, so a map interface with efficient
-- lookup is nicer.
data Rule = Rule
 { ports :: KMap Port
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
 }
 deriving Show

data Context = Context
 { ctxtRules :: KMap Rule
 }
 deriving Show


data Block = Block
 { blockRule :: Key Rule
 }
 deriving Show

data PortSpec = AssumptionPort Int | ConclusionPort Int | BlockPort (Key Block) (Key Port)
 deriving (Eq, Ord, Show)

data Connection = Connection
 { connFrom :: PortSpec
 , connTo :: PortSpec
 }
 deriving Show

data Proof = Proof
 { blocks :: KMap Block
 , connections :: KMap Connection
 }
 deriving Show

data ConnLabel = Ok Proposition | Mismatch Proposition Proposition
 deriving (Eq, Show)

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
