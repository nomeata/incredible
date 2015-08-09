module Propositions where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Data.Functor.Identity
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative hiding ((<|>))
import Data.Maybe
import Data.List
import Data.Void

-- This could be made more abstract as in the unification-fd package
data Term f v = Symb f [Term f v] | Var v
    deriving (Eq, Show)

type Proposition = Term String String

abbreviations :: M.Map String String
abbreviations = M.fromList
    [ ("and", "∧")
    , ("or", "∨")
    , ("imp", "→")
    , ("True", "⊤")
    , ("False", "⊥")
    ]

infixSymbols :: S.Set String
infixSymbols = S.fromList $ words "∧ ∨ →"

mapVar :: (v1 -> v2) -> Term f v1 -> Term f v2
mapVar vf (Symb f ts) = Symb f $ map (mapVar vf) ts
mapVar vf (Var v) = Var $ vf v

-- Pretty printer

-- Horribly inefficient, all that
printTerm :: Proposition -> String
printTerm (Var v) = v
printTerm (Symb f []) = f
printTerm (Symb f [arg1,arg2])
    | f `S.member` infixSymbols
    = printParens (printTerm arg1) ++ f ++ printParens (printTerm arg2)
printTerm (Symb f args) = f ++ printParens (intercalate "," (map printTerm args))

printParens :: String -> String
printParens s = "("++s++")"

-- Parser

parseTerm :: String -> Either String Proposition
parseTerm = either (Left . show) Right . parse (between (return ()) eof termP) ""

-- lexeme
l :: Parser a -> Parser a
l = between (return ()) spaces

termP :: Parser Proposition
termP =  buildExpressionParser table atomP <?> "proposition"

table :: OperatorTable String () Identity Proposition
table = [ [ binary "∧" ["&"]
          , binary "→" ["->"]
          ]
        , [ binary "∨" ["|"]
          ]
        ]
  where
    binary op alts = Infix ((\a b -> Symb op [a,b]) <$ l (choice (map string (op:alts)))) AssocLeft

atomP :: Parser Proposition
atomP = choice
    [ string "⊥" >> return (Symb "⊥" [])
    , string "⊤" >> return (Symb "⊤" [])
    , try (string "False") >> return (Symb "⊥" [])
    , try (string "True") >> return (Symb "⊤" [])
    , between (char '(') (char ')') termP
    , do
        hd <- many1 alphaNum
        let sym = fromMaybe hd $ M.lookup hd abbreviations
        option (Var sym) $ between (char '(') (char ')') $ do
            Symb sym <$> termP `sepBy1` (char ',')
    ]


-- Ground terms are terms with no variables, such as the propositions in the
-- task. We encode that invariant in the type system.
type GroundTerm = Term String Void

parseGroundTerm :: String -> Either String GroundTerm
parseGroundTerm = either (Left . show) (Right . fixVar) . parse termP ""

fixVar :: Term a a -> Term a Void
fixVar (Symb f ts) = Symb f $ map fixVar ts
fixVar (Var v) = Symb v []
