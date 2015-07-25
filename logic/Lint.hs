-- | This module is a linter for the input and output of our programs. This
-- means it checks properties that, if they fail, are a bug somewhere in the
-- program.
--
-- It should not be possible to trigger the linter via the usual interface (but
-- easily so via the debugging interface).
--
-- The existence of this module means that Haskell’s type system is not perfect...
--
-- The name "linter" comes from GHC, which also has that.

module Lint where

import qualified Data.Map as M
import Text.Printf
import Data.Monoid

import Types

type Lints = [String]

lintLogic :: Context -> Lints
lintLogic logic = wrongLocalHyps
  where
    wrongLocalHyps =
        [ printf "local hypothesis \"%s\" of rule \"%s\" has an invalid consumedBy field \"%s\""
          portKey ruleKey consumedBy
        | (ruleKey, rule) <- M.toList (ctxtRules logic)
        , (portKey, Port (PTLocalHyp consumedBy) _) <- M.toList (ports rule)
        , maybe True (not.isAssumption) $ M.lookup consumedBy (ports rule)
        ]
      where isAssumption (Port PTAssumption _) = True
            isAssumption _ = False

lintsToEither :: Lints -> Either String ()
lintsToEither [] = Right ()
lintsToEither ls = Left (unlines ls)


