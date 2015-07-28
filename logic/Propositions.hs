module Propositions where

import Text.Parsec
import Text.Parsec.Char
import Control.Applicative
import Data.List

-- This could be made more abstract as in the unification-fd package
data Term f v = Symb f [Term f v] | Var v
    deriving Show

type PropTerms = Term String String

mapVar :: (v1 -> v2) -> Term f v1 -> Term f v2
mapVar vf (Symb f ts) = Symb f $ map (mapVar vf) ts
mapVar vf (Var v) = Var $ vf v

-- Horribly inefficient, all that
printTerm :: PropTerms -> String
printTerm (Var v) = v
printTerm (Symb f args) =  f ++ "(" ++ intercalate "," (map printTerm args) ++ ")"

parseTerm :: String -> Either String PropTerms
parseTerm = either (Left . show) Right . parse termP ""

termP :: Parsec String () PropTerms
termP = do
    hd <- many1 alphaNum
    option (Var hd) $ do
        between (char '(') (char ')') $ do
            Symb hd <$> termP `sepBy1` (char ',')
