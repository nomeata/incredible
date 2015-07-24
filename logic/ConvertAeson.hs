{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module ConvertAeson (toContext, toProof, fromAnalysis) where

import qualified Data.Text as T
import Data.Aeson.Types
import qualified Data.Map as M
import Control.Applicative
import Data.List
import Control.Monad

import Types

-- Conversion from/to aeson value

toContext :: Value -> Either String Context
toContext = parseEither parseJSON

instance FromJSON Entailment where
  parseJSON = withObject "entailment" $ \o ->
    Entailment <$> o .:? "assumptions" .!= []
               <*> o .:? "conclusions" .!= []

instance FromJSON Rule where
  parseJSON = withObject "rule" $ \o ->
    Rule <$> o .: "id"
         <*> o .: "ports"

instance FromJSON Port where
  parseJSON = withObject "port" $ \o -> do
    typeS <- o .: "type"
    typ <- case typeS :: T.Text of
        "assumption" -> return PTAssumption
        "conclusion" -> return PTConclusion
        "local hypothesis" -> do
            PTLocalHyp  <$> o .: "consumedBy"
    Port typ <$> o .: "proposition"


instance FromJSON Context where
  parseJSON = withObject "context" $ \o -> do
    Context <$> o .: "proposition"
            <*> o .: "rules"

toProof :: Value -> Either String Proof
toProof = parseEither parseJSON

instance FromJSON Block where
  parseJSON = withObject "block" $ \o -> do
    Block <$> o .: "rule"

instance FromJSON Connection where
  parseJSON = withObject "block" $ \o -> do
    Connection <$> o .: "from" <*> o .: "to"

instance FromJSON PortSpec where
 parseJSON = withObject "port spec" $ \o -> msum
    [ AssumptionPort <$> o .: "assumption"
    , ConclusionPort <$> o .: "conclusion"
    , BlockPort <$> o .: "block" <*> o .: "port"
    ]


instance FromJSON Proof where
  parseJSON = withObject "proof" $ \o -> do
    Proof <$> o .:? "blocks"      .!= M.empty
          <*> o .:? "connections" .!= M.empty

fromAnalysis :: Analysis -> Value
fromAnalysis = toJSON

instance ToJSON Analysis where
    toJSON (Analysis {..}) = object
        [ "connectionPropositions" .= toJSON connectionPropositions
        , "unsolvedGoals" .= toJSON unsolvedGoals
        , "qed" .= toJSON qed
        ]

instance ToJSON PortSpec where
    toJSON (AssumptionPort n) = object [ "assumption" .= n ]
    toJSON (ConclusionPort n) = object [ "conclusion" .= n ]
    toJSON (BlockPort b n)    = object [ "block" .= b, "port" .= n ]
