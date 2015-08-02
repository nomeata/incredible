module Unification where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Applicative

import Propositions

-- This could be made more abstract instead of working on data Term, as in the
-- unification-fd package

-- Invariant: If (v1, Var v2), then v1 <= v2
type Assignment f v = (v, Term f v)
type Equality f v = (Term f v, Term f v)

type Bindings f v = M.Map v (Term f v)

emptyBinding :: Bindings f v
emptyBinding = M.empty

equationToAssignments :: (Ord v, Eq f) => Equality f v -> Maybe [Assignment f v]
equationToAssignments (Var v1, Var v2) | v1 == v2  = Just []
                                       | otherwise =  Just [(min v1 v2, Var (max v1 v2))]
equationToAssignments (Var v,  t)      = Just [(v, t)]
equationToAssignments (t,      Var v)  = Just [(v, t)]
equationToAssignments (Symb f1 args1, Symb f2 args2)
    | f1 == f2 && length args1 == length args2
    = concat <$> mapM equationToAssignments (zip args1 args2)
    | otherwise
    = Nothing


addAssignmentToBinding :: (Ord v, Eq f) => Bindings f v -> Assignment f v -> Maybe (Bindings f v)
addAssignmentToBinding bind (v, t) = case M.lookup v bind of
    Just t2 -> addEquationToBinding bind (t, t2)
    Nothing -> justIf bindingOk $ M.insert v t bind

addEquationToBinding :: (Ord v, Eq f) => Bindings f v -> Equality f v -> Maybe (Bindings f v)
addEquationToBinding bind eq = do
    asss <- equationToAssignments eq
    foldM addAssignmentToBinding bind asss

bindingOk :: Ord v => Bindings f v -> Bool
bindingOk bind = all (go S.empty) $ M.keys bind
  where
    go seen v = case M.lookup v bind of
        Just t -> goT (S.insert v seen) t
        Nothing -> True

    goT seen (Var v) | v `S.member` seen = False
                     | otherwise         = go seen v
    goT seen (Symb _ args) = all (goT seen) args



applyBinding :: (Ord v) => Bindings f v -> Term f v -> Term f v
applyBinding bind (Var v) = case M.lookup v bind of
    Just t -> applyBinding bind t
    Nothing -> Var v
applyBinding bind (Symb f terms) = Symb f $ map (applyBinding bind) terms

justIf :: (a -> Bool) -> a -> Maybe a
justIf p x = if p x then Just x else Nothing
