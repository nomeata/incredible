{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-} -- for the ~ type signature trick
module Propositions where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Data.Functor.Identity
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Maybe
import Data.List
import Data.Void
import Data.Tree

import Unbound.LocallyNameless hiding (Infix)

-- This could be made more abstract as in the unification-fd package
data Term = Symb String [Term] | Var (Name Term)| Quant String (Bind (Name Term) Term)
    deriving Show

$(derive [''Term])

instance Alpha Term


type Proposition = Term

infixSymbols :: S.Set String
infixSymbols = S.fromList $ words "∧ ∨ →"

{-
mapVar :: (v1 -> v2) -> Term v1 -> Term v2
mapVar vf (Symb f ts) = Symb f $ map (mapVar vf) ts
mapVar vf (Var v) = Var $ vf v
mapVar vf (Quant q v p) = Quant q (vf v) $ mapVar vf p
-}

-- Pretty printer

printTerm :: Proposition -> String
printTerm p = runLFreshM (prP (0::Int) p) ""
  where
    prP :: Int -> Proposition -> LFreshM (String -> String)
    prP _ (Var v)     = prS (show v)
    prP _ (Symb f []) = prS f
    prP d (Symb f [a1, a2]) | Just p <- isInFix f
        = prParens (p < d) $ prP (p+1) a1 <> prS f <> prP p a2
    prP _ (Symb f args) = prS f <> prS "(" <> prCommas [prP 0 a | a <- args] <> prS ")"
    prP d (Quant q b) = prParens (1 < d) $ lunbind b $ \(v,t) ->
        prS q <> prS (show v) <> prS "." <> prP 1 t

    prParens :: Bool -> LFreshM (String -> String) -> LFreshM (String -> String)
    prParens True  f = prS "(" <> f <> prS ")"
    prParens False f =            f

    prCommas = foldr (<>) (return id) . intersperse (prS ",")

    prS str = return (str++)

    (<>) :: t ~ LFreshM (String -> String) => t -> t -> t
    (<>) = liftM2 (.)


-- Is it infix? What precedences?
isInFix :: String -> Maybe Int
isInFix "∧" = Just 3
isInFix "→" = Just 3
isInFix "∨" = Just 2
isInFix _   = Nothing


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
        sym <- many1 alphaNum
        option (Var (string2Name sym)) $ between (char '(') (char ')') $ do
            Symb sym <$> termP `sepBy1` (char ',')
    ]


-- Ground terms are terms with no variables, such as the propositions in the
-- task. We no longer encode that invariant in the type system.
type GroundTerm = Term 

parseGroundTerm :: String -> Either String GroundTerm
parseGroundTerm = either (Left . show) (Right . fixVar) . parse termP ""

fixVar :: Term -> Term
fixVar = id -- TODO
