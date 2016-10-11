{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-} -- for the ~ type signature trick
module Propositions (module Propositions, string2Name) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Data.Functor.Identity
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

data OpAssoc
    = L
    | R

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

name2ExternalString :: Var -> String
name2ExternalString n
    | name2Integer n == 0 = name2String n
    | otherwise = error $ "name2ExternalString: Invalid external name " ++ show n

-- Pretty printer

printTerm :: Proposition -> String
printTerm p = runLFreshM (avoid (fvAny p) $ prP (0::Int) p) ""
  where
    prP :: Int -> Proposition -> LFreshM (String -> String)
    prP _ (C v) = prN v
    prP _ (V v) = prN v
    prP d (App (C f) [a]) | Just p <- isPrefix (name2String f)
        = prParens (p < d) $ prN f <> prP p a
    prP d (App (C f) [a1, a2]) | Just (p,_assoc) <- isInFix (name2String f)
        = prParens (p < d) $ prP (p+1) a1 <> prN f <> prP (p+1) a2
    prP d (App (C f) [Lam b]) | isQuant (name2String f)
        = prParens (1 < d) $ lunbind b $ \(v,t) ->
        prN f <> prN v <> prS "." <> prP 1 t
    prP _ (App f args) = prP 4 f <> prS "(" <> prCommas [prP 0 a | a <- args] <> prS ")"
    prP d (Lam b) = prParens (1 < d) $ lunbind b $ \(v,t) ->
        prS "Λ" <> prN v <> prS "." <> prP 1 t

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
isInFix :: String -> Maybe (Int, OpAssoc)
isInFix "⋅" = Just (7, L)
isInFix "↑" = Just (5, L)
isInFix "∧" = Just (4, L)
isInFix "∨" = Just (4, L)
isInFix "→" = Just (3, R)
isInFix ":" = Just (2, R)
isInFix _   = Nothing

isQuant :: String -> Bool
isQuant = (`elem` words "∃ ∀ λ")

isPrefix :: String -> Maybe Int
isPrefix "¬" = Just 6
isPrefix _   = Nothing

-- Parser

parseTerm :: String -> Either String Proposition
parseTerm = either (Left . show) Right . parse (between spaces eof termP) ""

-- For Testing and GHCi
readTerm :: String -> Proposition
readTerm = either (error . show) id . parseTerm

-- lexeme
l :: Parser a -> Parser a
l = (<* (spaces <?> ""))

termP :: Parser Proposition
termP =  buildExpressionParser table atomP <?> "proposition"

table :: OperatorTable String () Identity Proposition
table = [ [ binary "⋅" [] AssocLeft
          ]
        , [ binary "↑" ["^"] AssocLeft
          ]
        , [ binary "∧" ["&"] AssocLeft
          ]
        , [ binary "∨" ["|"] AssocLeft
          ]
        , [ binary "→" ["->"] AssocRight
          ]
        , [ binary ":" []     AssocRight
          ]
        ]
  where
    binary op alts assoc = Infix ((\a b -> App (C (string2Name op)) [a,b]) <$ l (choice (map string (op:alts)))) assoc

quantifiers :: [(Char, [Char])]
quantifiers =
    [ ('∀', ['!'])
    , ('∃', ['?'])
    , ('λ', ['\\'])
    , ('Λ', [])
    ]

mkQuant :: String -> Var -> Term -> Term
mkQuant "Λ" n t = mkLam n t
mkQuant q   n t = App (C (string2Name q)) [mkLam n t]

quantP :: Parser String
quantP = l $ choice [ (q:"") <$ choice (map char (q:a)) | (q,a) <- quantifiers ]

atomP :: Parser Proposition
atomP = choice
    [ l $ string "⊥" >> return (c "⊥")
    , l $ string "⊤" >> return (c "⊤")
    , l $ try (string "False") >> return (c "⊥")
    , l $ try (string "True") >> return (c "⊤")
    , do
        _ <- l $ char '¬' <|> char '~'
        p <- atomP
        return $ s "¬" [p]
    , do
        q <- quantP
        vname <- nameP
        _ <- l $ char '.'
        p <- termP
        return $ mkQuant q vname $ p
    , parenthesized termP
    , do
        head <- varOrConstP
        option head $ parenthesized $
            App head <$> termP `sepBy1` (l $ char ',')
    ]
  where
    c n = C (string2Name n)
    s n = App (c n)

    parenthesized = between (l $ char '(') (l $ char ')')

varOrConstP :: Parser Term
varOrConstP = choice
  [ do
    -- A hack for the test suite etc: prepending the name of a constant with V
    -- makes it a variable
    _ <- try (string "V ")
    num <- l $ option 0 (read <$> many1 digit)
    s <- l $ many1 alphaNum
    return $ V $ makeName s num
  , C <$> nameP
  ]


nameP :: Rep a => Parser (Name a)
nameP = l $ string2Name <$> many1 alphaNum
