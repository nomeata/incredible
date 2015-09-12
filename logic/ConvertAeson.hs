{-# LANGUAGE OverloadedStrings, RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}
module ConvertAeson (toContext, toTask, toProof, fromAnalysis) where

import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Aeson.Types
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Data.List

import Types
import TaggedMap ()
import Propositions

-- Conversion from/to aeson value

toContext :: Value -> Either String Context
toContext = parseEither parseJSON


varList :: Object -> T.Text -> Parser [Var]
varList o field = map (string2Name) <$> o .:? field .!= []


instance FromJSON Rule where
  parseJSON = withObject "rule" $ \o -> do
    f <- varList o "free"
    l <- varList o "local"
    Rule (l++f) f <$> o .: "ports"

instance FromJSON Port where
  parseJSON = withObject "port" $ \o -> do
    typeS <- o .: "type"
    typ <- case typeS :: T.Text of
        "assumption" -> return PTAssumption
        "conclusion" -> return PTConclusion
        "local hypothesis" -> do
            PTLocalHyp  <$> o .: "consumedBy"
        t -> fail $ "Unknown port type \"" ++ T.unpack t ++ "\""
    Port typ <$> o .: "proposition" <*> varList o "scoped"

instance FromJSON Proposition where
    parseJSON = withText "proposition" $ \s ->
        case parseTerm (T.unpack s) of
            Left e -> fail e
            Right p -> return p

{-
instance FromJSON GroundTerm where
    parseJSON = withText "ground term" $ \s ->
        case parseGroundTerm (T.unpack s) of
            Left e -> fail e
            Right p -> return p
-}


instance ToJSON Proposition where
    toJSON = toJSON . printTerm


instance FromJSON Context where
  parseJSON = withObject "context" $ \o -> do
    Context <$> (mapFromList "id" =<< o .: "rules")

mapFromList :: (FromJSON k, FromJSON v, Ord k) => T.Text -> Value -> Parser (M.Map k v)
mapFromList idField = withArray "rules" $ \a -> do
    entries <- forM (V.toList a) $ \v -> do
        k <- withObject "rule" (.: idField) v
        r <- parseJSON v
        return (k,r)
    return $ M.fromList entries

toTask :: Value -> Either String Task
toTask = parseEither parseJSON

instance FromJSON Task where
  parseJSON = withObject "task" $ \o ->
    Task <$> o .:? "assumptions" .!= []
         <*> o .:? "conclusions" .!= []


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
    , pure NoPort
    ]


instance FromJSON Proof where
  parseJSON = withObject "proof" $ \o -> do
    Proof <$> o .:? "blocks"      .!= M.empty
          <*> o .:? "connections" .!= M.empty

fromAnalysis :: Analysis -> Value
fromAnalysis = toJSON

toVarList :: [Var] -> [String]
toVarList = map name2ExternalString

instance ToJSON Port where
    toJSON (Port {..}) = object $
        (case portType of
            PTAssumption ->         [ "type" .= ("assumption" :: T.Text) ]
            PTConclusion ->         [ "type" .= ("conclusion" :: T.Text) ]
            (PTLocalHyp portKey) -> [ "type" .= ("conclusion" :: T.Text)
                                    , "consumedBy" .= toJSON portKey]
        ) ++
        [ "proposition" .= toJSON portProp
        , "scoped" .= toVarList portScopes
        ]

instance ToJSON Rule where
    toJSON (Rule free local ports) = object
        [ "free"  .= toVarList (free \\ local)
        , "local" .= toVarList local
        , "ports" .= ports
        ]

instance ToJSON Analysis where
    toJSON (Analysis {..}) = object
        [ "connectionLabels" .= connectionLabels
        , "unconnectedGoals" .= unconnectedGoals
        , "cycles" .= cycles
        , "escapedHypotheses" .= escapedHypotheses
        , "rule" .= rule
        , "qed" .= qed
        ]

instance ToJSON ConnLabel where
    toJSON Unconnected = object []
    toJSON (Ok prop) = toJSON prop
    toJSON (Mismatch prop1 prop2) = object
        [ "propIn" .= prop1, "propOut" .= prop2, "type" .= ("mismatch"::T.Text) ]
    toJSON (DunnoLabel prop1 prop2) = object
        [ "propIn" .= prop1, "propOut" .= prop2, "type" .= ("dunno"::T.Text) ]

instance ToJSON PortSpec where
    toJSON NoPort             = object []
    toJSON (AssumptionPort n) = object [ "assumption" .= n ]
    toJSON (ConclusionPort n) = object [ "conclusion" .= n ]
    toJSON (BlockPort b n)    = object [ "block" .= b, "port" .= n ]
