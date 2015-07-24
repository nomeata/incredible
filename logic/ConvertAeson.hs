{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module ConvertAeson (toContext, toProof, fromAnalysis) where

import qualified Data.Text as T
import Data.Aeson.Types
import qualified Data.Map as M
import Control.Applicative
import Data.List

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
    Connection <$> (toPortSpec <$> o .: "fromBlock" <*> o .: "fromPort")
               <*> (toPortSpec <$> o .: "toBlock" <*> o .: "toPort")

instance FromJSON Proof where
  parseJSON = withObject "proof" $ \o -> do
    Proof <$> o .:? "blocks"      .!= M.empty
          <*> o .:? "connections" .!= M.empty

toPortSpec "proposition" port
    | "assumption" `isPrefixOf` port
    = AssumptionPort $ read $ drop (length ("assumption"::String)) port
    | "conclusion" `isPrefixOf` port
    = ConclusionPort $ read $ drop (length ("conclusion"::String)) port
    | otherwise
    = error "invalid proposition port"
toPortSpec block port = BlockPort block port

fromAnalysis :: Analysis -> Value
fromAnalysis = toJSON

instance ToJSON Analysis where
    toJSON (Analysis {..}) = object
        [ "connectionPropositions" .= toJSON connectionPropositions
        , "unsolvedGoals" .= toJSON unsolvedGoals
        , "qed" .= toJSON qed
        ]

instance ToJSON PortSpec where
    toJSON (AssumptionPort n) = object
        [ "block" .= ("proposition"::String)
        , "port"  .= ("assumption" ++ show n)
        ]
    toJSON (ConclusionPort n) = object
        [ "block" .= ("proposition"::String)
        , "port"  .= ("conclusion" ++ show n)
        ]
    toJSON (BlockPort b n) = object
        [ "block" .= b
        , "port"  .= n
        ]

