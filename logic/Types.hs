-- | The data types for the UI→Haskell interface. Roughly corresponds to the
-- specification in SPEC.md.
module Types where

import qualified Data.Map as M

type Proposition = String

-- We might want to look into other maps
type Key = String
type KMap v = M.Map Key v


data Entailment = Entailment
 { eAssumptions :: [Proposition]
 , eConclusions :: [Proposition]
 }

data Rule = Rule
 { id :: Key
 , ports :: KMap Port
 }

data PortType = PTAssumption | PTConclusion | PTLocalHyp Key

data Port = Port
 { portType :: PortType
 , portProp :: Proposition
 }

data Context = Context
 { ctxtProposition :: Entailment
 , ctxtRules :: [Rule]
 }


data Block = Block
 { blockRule :: Key
 }

data PortSpec = AssumptionPort Int | ConclusionPort Int | BlockPort Key Key

data Connection = Connection
 { connFrom :: PortSpec
 , connTo :: PortSpec
 }

data Proof = Proof
 { blocks :: KMap Block
 , connections :: KMap Connection
 }

data Analysis = Analysis
 { connectionPropositions :: KMap Proposition
 , unsolvedGoals :: [PortSpec]
-- , unificationFailures
-- , cycles
-- , escapedHypotheses
 , qed :: Bool
 }
