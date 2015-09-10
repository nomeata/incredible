{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-} -- for the ~ type signature trick
module Propositions (module Propositions, string2Name) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Data.Functor.Identity
import qualified Data.Set as S
import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.List
import Utils

import Unbound.LocallyNameless hiding (Infix)


type Var = Name Term

-- This could be made more abstract as in the unification-fd package
data Term
    = App Term [Term]
    | V Var
    | C Var
    | Lam (Bind Var Term)
    deriving Show

$(derive [''Term])

instance Alpha Term
instance Eq Term where (==) = aeq

instance Subst Term Term where
   isvar (V v) = Just (SubstName v)
   isvar (C v) = Just (SubstName v)
   isvar _     = Nothing

firstFree :: Alpha a => a -> Integer
firstFree = (1+) . maximum . (0:) . map anyName2Integer . fvAny

type Proposition = Term

absTerm :: [Var] -> Term -> Term
absTerm vs t = foldr mkLam t vs

mkLam :: Var -> Term -> Term
mkLam v t = Lam (bind v (const2Var [v] t))

mkApps :: Term -> [Term] -> Term
mkApps t [] = t
mkApps t ts = App t ts

const2Var :: [Var] -> Term -> Term
const2Var vs = substs [(v, V v) | v <- vs]

-- Pretty printer

printTerm :: Proposition -> String
printTerm p = runLFreshM (prP (0::Int) p) ""
  where
    prP :: Int -> Proposition -> LFreshM (String -> String)
    prP _ (C v) = prN v
    prP _ (V v) = prN v
    prP d (App (C f) [a1, a2]) | Just p <- isInFix (name2String f)
        = prParens (p < d) $ prP (p+1) a1 <> prN f <> prP p a2
    prP d (App (C f) [Lam b]) | isQuant (name2String f)
        = prParens (1 < d) $ lunbind b $ \(v,t) ->
        prN f <> prN v <> prS "." <> prP 1 t
    prP _ (App f args) = prP 4 f <> prS "(" <> prCommas [prP 0 a | a <- args] <> prS ")"
    prP d (Lam b) = prParens (1 < d) $ lunbind b $ \(v,t) ->
        prS "λ" <> prN v <> prS "." <> prP 1 t

    prN n = prS (name2String n)
        <> (if i > 0 then prS (map subscriptify (show i)) else return id)
      where i = name2Integer n

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

isQuant :: String -> Bool
isQuant = (`elem` words "∃ ∀")

-- Parser

parseTerm :: String -> Either String Proposition
parseTerm = either (Left . show) Right . parse (between (return ()) eof termP) ""

-- For Testing and GHCi
readTerm :: String -> Proposition
readTerm = either (error . show) id . parseTerm

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
    binary op alts = Infix ((\a b -> App (C (string2Name op)) [a,b]) <$ l (choice (map string (op:alts)))) AssocLeft

quantifiers :: [(Char, [Char])]
quantifiers =
    [ ('∀', ['!'])
    , ('∃', ['?'])
    , ('λ', ['\\'])
    ]

mkQuant :: String -> Var -> Term -> Term
mkQuant "λ" n t = mkLam n t
mkQuant q   n t = App (C (string2Name q)) [mkLam n t]

quantP :: Parser String
quantP = choice [ (q:"") <$ choice (map char (q:a)) | (q,a) <- quantifiers ]

atomP :: Parser Proposition
atomP = choice
    [ string "⊥" >> return (s "⊥" [])
    , string "⊤" >> return (s "⊤" [])
    , try (string "False") >> return (s "⊥" [])
    , try (string "True") >> return (s "⊤" [])
    , do
        q <- quantP
        vname <- nameP
        _ <- char '.'
        p <- termP
        return $ mkQuant q vname $ p
    , between (char '(') (char ')') termP
    , do
        head <- varOrConstP
        option head $ between (char '(') (char ')') $ do
            App head <$> termP `sepBy1` (char ',')
    ]
  where
    s n = App (C (string2Name n))

varOrConstP :: Parser Term
varOrConstP = do
    -- A hack for the test suite etc: prepending the name of a constant with V
    -- makes it a variable
    con <- option C (V <$ (try (string "V ")))
    n <- nameP
    return $ con n

nameP :: Rep a => Parser (Name a)
nameP = string2Name <$> many1 alphaNum
