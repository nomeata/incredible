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
import Data.Tagged

import Types



type Lints = [String]

lint :: Context -> Task -> Proof -> Lints
lint logic _task proof = mconcat
    [ wrongLocalHyps, missingRule, wrongBlock, wrongPort, wrongSourceType, wrongTargetType, nonUniqueBlockNums ]
  where
    wrongLocalHyps =
        [ printf "local hypothesis \"%s\" of rule \"%s\" has an invalid consumedBy field \"%s\""
          (untag portKey) (untag ruleKey) (untag consumedBy)
        | (ruleKey, rule) <- M.toList (ctxtRules logic)
        , (portKey, Port (PTLocalHyp consumedBy) _ _) <- M.toList (ports rule)
        , maybe True (not.isAssumption) $ M.lookup consumedBy (ports rule)
        ]
      where isAssumption (Port PTAssumption _ _) = True
            isAssumption _ = False
    missingRule =
        [ printf "Block \"%s\" references unknown rule \"%s\"" (untag blockKey) (untag ruleKey)
        | (blockKey, Block _ ruleKey) <- M.toList (blocks proof)
        , ruleKey `M.notMember` ctxtRules logic
        ]
    wrongBlock =
        [ printf "Connection \"%s\" references unknown block \"%s\""
          (untag connKey) (untag blockKey)
        | (connKey, conn) <- M.toList (connections proof)
        , BlockPort blockKey _ <- [connFrom conn, connTo conn]
        , blockKey `M.notMember` blocks proof
        ]
    wrongPort =
        [ printf "Connection \"%s\" references unknown port \"%s\" of block \"%s\", rule \"%s\""
          (untag connKey) (untag portKey) (untag blockKey) (untag ruleKey)
        | (connKey, conn) <- M.toList (connections proof)
        , BlockPort blockKey portKey <- [connFrom conn, connTo conn]
        , Just (Block _ ruleKey) <- return $ M.lookup blockKey (blocks proof)
        , Just rule <- return $ M.lookup ruleKey (ctxtRules logic)
        , portKey `M.notMember` ports rule
        ]
    wrongSourceType =
        [ printf "Connection \"%s\" begins in port \"%s\" of block \"%s\", which is not a conclusion or local hypothesis"
          (untag connKey) (untag portKey) (untag blockKey)
        | (connKey, conn) <- M.toList (connections proof)
        , BlockPort blockKey portKey <- return $ connFrom conn
        , Just (Block _ ruleKey) <- return $ M.lookup blockKey (blocks proof)
        , Just rule <- return $ M.lookup ruleKey (ctxtRules logic)
        , Just port <- return $ M.lookup portKey (ports rule)
        , not $ isOk port
        ] ++
        [ printf "Connection \"%s\" begins in conclusion %d"
          (untag connKey) n
        | (connKey, conn) <- M.toList (connections proof)
        , ConclusionPort n <- return $ connFrom conn
        ]
      where isOk (Port (PTLocalHyp _) _ _) = True
            isOk (Port PTConclusion _ _) = True
            isOk _ = False
    wrongTargetType =
        [ printf "Connection \"%s\" ends in port \"%s\" of block \"%s\", which is not an assumption."
          (untag connKey) (untag portKey) (untag blockKey)
        | (connKey, conn) <- M.toList (connections proof)
        , BlockPort blockKey portKey <- return $ connTo conn
        , Just (Block _ ruleKey) <- return $ M.lookup blockKey (blocks proof)
        , Just rule <- return $ M.lookup ruleKey (ctxtRules logic)
        , Just port <- return $ M.lookup portKey (ports rule)
        , not $ isOk port
        ] ++
        [ printf "Connection \"%s\" begins in global assumption %d"
          (untag connKey) n
        | (connKey, conn) <- M.toList (connections proof)
        , AssumptionPort n <- return $ connTo conn
        ]
      where isOk (Port PTAssumption _ _) = True
            isOk _ = False

    nonUniqueBlockNums
        = [ printf "Block number %d assigned %d times." n c | (n,c) <- M.toList bad ]
      where
        bad = M.filter (>1) $
              M.fromListWith (+) $
              [ (blockNum b ,(1::Int)) | b <- M.elems (blocks proof) ]


lintsToEither :: Lints -> Either String ()
lintsToEither [] = Right ()
lintsToEither ls = Left (unlines ls)


