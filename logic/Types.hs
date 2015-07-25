-- | The data types for the UI→Haskell interface. Roughly corresponds to the
-- specification in SPEC.md.
module Types where

import qualified Data.Map as M

type Proposition = String

-- We might want to look into other maps
type Key = String
type KMap v = M.Map Key v


data Task = Task
 { tAssumptions :: [Proposition]
 , tConclusions :: [Proposition]
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

data PortType = PTAssumption | PTConclusion | PTLocalHyp Key
 deriving Show

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
 { blockRule :: Key
 }
 deriving Show

data PortSpec = AssumptionPort Int | ConclusionPort Int | BlockPort Key Key
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

data Analysis = Analysis
 { connectionPropositions :: KMap Proposition
 , unsolvedGoals :: [PortSpec]
-- , unificationFailures
 , cycles :: [Cycle]
-- , escapedHypotheses
 , qed :: Bool
 }
 deriving Show

type Cycle = [Key]
