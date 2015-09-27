{-# LANGUAGE OverloadedStrings, RecordWildCards, TypeSynonymInstances, FlexibleInstances, MultiWayIf #-}
module ConvertAeson (toContext, toTask, toProof, fromAnalysis) where

import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Aeson.Types
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Control.Applicative
import Control.Monad
import Data.List

import Types
import TaggedMap ()
import Propositions
import Unification (UnificationResult(..))

-- Conversion from/to aeson value

toContext :: Value -> Either String Context
toContext = parseEither parseJSON


varList :: Object -> T.Text -> Parser [Var]
varList o field = map (string2Name) <$> o .:? field .!= []


instance FromJSON Rule where
  parseJSON = withObject "rule" $ \o -> do
    l <- varList o "local"
    f <- varList o "free"
    Rule (f++l) f <$> o .: "ports"

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
  parseJSON = withObject "block" $ \o ->
    if | "rule"       `HM.member` o -> Block <$> o.: "number" <*> o .: "rule"
       | "annotation" `HM.member` o -> AnnotationBlock <$> o.: "number" <*> o .: "annotation"
       | "assumption" `HM.member` o -> AssumptionBlock <$> o.: "number" <*> o .: "assumption"
       | "conclusion" `HM.member` o -> ConclusionBlock <$> o.: "number" <*> o .: "conclusion"


instance FromJSON Connection where
  parseJSON = withObject "block" $ \o -> do
    Connection <$> o .: "sortKey" <*> o .: "from" <*> o .: "to"

instance FromJSON PortSpec where
 parseJSON = withObject "port spec" $ \o ->
    BlockPort <$> o .: "block" <*> o .: "port"

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
            (PTLocalHyp portKey) -> [ "type" .= ("local hypothesis" :: T.Text)
                                    , "consumedBy" .= toJSON portKey]
        ) ++
        [ "proposition" .= toJSON portProp
        , "scoped" .= toVarList portScopes
        ]

instance ToJSON Rule where
    toJSON (Rule {..}) = object
        [ "local" .= toVarList (localVars \\ freeVars)
        , "free"  .= toVarList freeVars
        , "ports" .= ports
        ]

instance ToJSON Analysis where
    toJSON (Analysis {..}) = object
        [ "connectionStatus"  .= connectionStatus
        , "portLabels"        .= portLabels
        , "unconnectedGoals"  .= unconnectedGoals
        , "cycles"            .= cycles
        , "escapedHypotheses" .= escapedHypotheses
        , "rule"              .= rule
        , "qed"               .= qed
        ]

instance ToJSON a => ToJSON (M.Map PortSpec a) where
    toJSON = toJSON . M.fromListWith M.union . map go . M.toList 
      where
        go (BlockPort bk pk, a) = (bk, M.singleton pk a)

instance ToJSON UnificationResult where
    toJSON = toJSON . go
      where
        go :: UnificationResult -> T.Text
        go Failed = "failed"
        go Dunno  = "dunno"
        go Solved = "solved"

instance ToJSON PortSpec where
    toJSON (BlockPort b n)    = object [ "block" .= b, "port" .= n ]
