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

import Unbound.LocallyNameless hiding (Infix)


type Var = Name Term

-- This could be made more abstract as in the unification-fd package
data Term
    = Symb Term [Term]
    | Var Var
    | Quant String (Bind Var Term)
    deriving Show


$(derive [''Term])

instance Alpha Term
instance Eq Term where (==) = aeq

instance Subst Term Term where
   isvar (Var v) = Just (SubstName v)
   isvar _       = Nothing



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
    prP _ (Var v)     = prN v
    prP d (Symb (Var f) [a1, a2]) | Just p <- isInFix (name2String f)
        = prParens (p < d) $ prP (p+1) a1 <> prN f <> prP p a2
    prP _ (Symb f args) = prP 4 f <> prS "(" <> prCommas [prP 0 a | a <- args] <> prS ")"
    prP d (Quant q b) = prParens (1 < d) $ lunbind b $ \(v,t) ->
        prS q <> prN v <> prS "." <> prP 1 t

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
    binary op alts = Infix ((\a b -> Symb (Var (string2Name op)) [a,b]) <$ l (choice (map string (op:alts)))) AssocLeft

quantifiers :: [(Char, [Char])]
quantifiers =
    [ ('∀', ['!'])
    , ('∃', ['?'])
    ]

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
        return $ Quant q (bind vname p)
    , between (char '(') (char ')') termP
    , do
        sym <- nameP
        option (Var sym) $ between (char '(') (char ')') $ do
            Symb (Var sym) <$> termP `sepBy1` (char ',')
    ]
  where
    s n = Symb (Var (string2Name n))

nameP :: Rep a => Parser (Name a)
nameP = string2Name <$> many1 alphaNum



-- Ground terms are terms with no variables, such as the propositions in the
-- task. We no longer encode that invariant in the type system.
type GroundTerm = Term 

parseGroundTerm :: String -> Either String GroundTerm
parseGroundTerm = either (Left . show) (Right . fixVar) . parse termP ""

fixVar :: Term -> Term
fixVar = id -- TODO


subscriptify :: Char -> Char
subscriptify '0' = '₀'
subscriptify '1' = '₁'
subscriptify '2' = '₂'
subscriptify '3' = '₃'
subscriptify '4' = '₄'
subscriptify '5' = '₅'
subscriptify '6' = '₆'
subscriptify '7' = '₇'
subscriptify '8' = '₈'
subscriptify '9' = '₉'
subscriptify _ = error "subscriptify: non-numeral argument"
