module Propositions where

import Text.Parsec
import Text.Parsec.Char
import Control.Applicative
import Data.List
import Data.Void

-- This could be made more abstract as in the unification-fd package
data Term f v = Symb f [Term f v] | Var v
    deriving (Eq, Show)

type Proposition = Term String String

mapVar :: (v1 -> v2) -> Term f v1 -> Term f v2
mapVar vf (Symb f ts) = Symb f $ map (mapVar vf) ts
mapVar vf (Var v) = Var $ vf v

-- Horribly inefficient, all that
printTerm :: Proposition -> String
printTerm (Var v) = v
printTerm (Symb f []) = f
printTerm (Symb f args) = f ++ "(" ++ intercalate "," (map printTerm args) ++ ")"

parseTerm :: String -> Either String Proposition
parseTerm = either (Left . show) Right . parse termP ""

termP :: Parsec String () Proposition
termP = do
    hd <- many1 alphaNum
    option (Var hd) $ do
        between (char '(') (char ')') $ do
            Symb hd <$> termP `sepBy1` (char ',')


-- Ground terms are terms with no variables, such as the propositions in the
-- task. We encode that invariant in the type system.
type GroundTerm = Term String Void

parseGroundTerm :: String -> Either String GroundTerm
parseGroundTerm = either (Left . show) Right . parse groundTermP ""


groundTermP :: Parsec String () GroundTerm
groundTermP = do
    hd <- many1 alphaNum
    args <- option [] $ do
        between (char '(') (char ')') $ do
            groundTermP `sepBy1` (char ',')
    return $ Symb hd args

