Core-API
=======

This file summarises the signature of the logical core, as exported from Haskell to the JavaScript world:

```haskell
type Key k = Tagged k String
type KMap v = Data.Map.Map (Key v) v

type Var = Unbound.LocallyNameless.Name Term
data Term = App Term [Term] | V Var | C Var
          | Lam (Unbound.LocallyNameless.Bind Var Term)

-- Context
data PortType = PTAssumption | PTConclusion | PTLocalHyp (Key Port)
data Port = Port {portType :: PortType, portProp :: Term, portScopes :: [Var]}
data Rule = Rule {localVars :: [Var], freeVars :: [Var], ports :: KMap Port}
data Context = Context {ctxtRules :: KMap Rule}

-- Proof
data PortSpec = BlockPort {psBlock :: Key Block, psPort :: Key Port}

data Block
  = AnnotationBlock Int Proposition
  | AssumptionBlock Int Proposition
  | ConclusionBlock Int Proposition
  | Block           Int (Key Rule)

data Connection = Connection {connSortKey :: Integer,
                              connFrom    :: Maybe PortSpec,
                              connTo      :: Maybe PortSpec}

data Proof = Proof {blocks :: KMap Block, connections :: KMap Connection}

-- Result
data UnificationResult = Solved | Failed | Dunno
type Cycle = [Key Connection]
type Path =  [Key Connection]
data Analysis = Analysis { qed               :: Bool,
                           connectionStatus  :: M.Map (Key Connection) UnificationResult,
                           portLabels        :: M.Map PortSpec Term,
                           unconnectedGoals  :: [PortSpec],
                           cycles            :: [Cycle],
                           escapedHypotheses :: [Path]}

-- The actual function
incredibleLogic :: Context -> Proof -> Either String Analysis
```
