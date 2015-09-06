module Unification where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Data.Maybe

import Propositions

-- This could be made more abstract instead of working on data Term, as in the
-- unification-fd package

-- Invariant: If (v1, Var v2), then v1 <= v2
type Assignment = (Var, Term)
type Equality = (Term, Term)

type Bindings = M.Map Var (Term)

emptyBinding :: Bindings
emptyBinding = M.empty

equationToAssignments :: [Var] -> Equality -> Maybe [Assignment]
equationToAssignments uvs (Var v1, Var v2)
    | v1 == v2
    = Just []
    | v1 `elem` uvs && v2 `elem` uvs
    = Just [(min v1 v2, Var (max v1 v2))]
equationToAssignments uvs (Var v,  t)
    | v `elem` uvs
    = Just [(v, t)]
equationToAssignments uvs (t,      Var v)
    | v `elem` uvs
    = Just [(v, t)]
equationToAssignments uvs (Symb f1 args1, Symb f2 args2)
    -- No unification of function symbols yet
    | f1 == f2 && length args1 == length args2
    = concat <$> mapM (equationToAssignments uvs) (zip args1 args2)
{-
equationToAssignments (Quant q1 v1 t1, Quant q2 v2 t2)
    | q1 == q2 && v1 == v2 -- TODO: alpha-rename this!
    = equationToAssignments (t1, t2) -- TODO: This is wrong, variable v1 could escape!
-}
equationToAssignments _ _ = Nothing


addAssignmentToBinding :: [Var] -> Bindings -> Assignment -> Maybe (Bindings)
addAssignmentToBinding uvs bind (v, t) = case M.lookup v bind of
    Just t2 -> addEquationToBinding uvs bind (t, t2)
    Nothing -> justIf bindingOk $ M.insert v t bind

addEquationToBinding :: [Var] -> Bindings -> Equality -> Maybe (Bindings)
addEquationToBinding uvs bind eq = do
    asss <- equationToAssignments uvs eq
    foldM (addAssignmentToBinding uvs) bind asss

unifyEquationsWherePossible :: [Var] -> [Equality] -> Bindings
unifyEquationsWherePossible uvs = foldl consider emptyBinding
  where
    consider bind equation = fromMaybe bind $ addEquationToBinding uvs bind equation

bindingOk :: Bindings -> Bool
bindingOk bind = all (go S.empty) $ M.keys bind
  where
    go seen v = case M.lookup v bind of
        Just t -> goT (S.insert v seen) t
        Nothing -> True

    goT seen (Var v) | v `S.member` seen = False
                     | otherwise         = go seen v
    goT seen (Symb _ args) = all (goT seen) args
    {-
    goT seen (Quant _ v t) | v `S.member` seen = False -- TODO: think this through
                           | otherwise         = goT seen t
    -}


applyBinding :: Bindings -> Term -> Term
applyBinding bind (Var v) = case M.lookup v bind of
    Just t -> applyBinding bind t
    Nothing -> Var v
applyBinding bind (Symb f terms) = Symb f $ map (applyBinding bind) terms
-- applyBinding bind (Quant q v t) = Quant q v $ applyBinding bind t -- TODO: Avoid variable capture!

justIf :: (a -> Bool) -> a -> Maybe a
justIf p x = if p x then Just x else Nothing
