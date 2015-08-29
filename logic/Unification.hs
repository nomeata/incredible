module Unification where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Applicative

import Propositions

-- This could be made more abstract instead of working on data Term, as in the
-- unification-fd package

-- Invariant: If (v1, Var v2), then v1 <= v2
type Assignment v = (v, Term v)
type Equality v = (Term v, Term v)

type Bindings v = M.Map v (Term v)

emptyBinding :: Bindings v
emptyBinding = M.empty

equationToAssignments :: (Ord v) => Equality v -> Maybe [Assignment v]
equationToAssignments (Var v1, Var v2) | v1 == v2  = Just []
                                       | otherwise =  Just [(min v1 v2, Var (max v1 v2))]
equationToAssignments (Var v,  t)      = Just [(v, t)]
equationToAssignments (t,      Var v)  = Just [(v, t)]
equationToAssignments (Symb f1 args1, Symb f2 args2)
    | f1 == f2 && length args1 == length args2
    = concat <$> mapM equationToAssignments (zip args1 args2)
equationToAssignments (Quant q1 v1 t1, Quant q2 v2 t2)
    | q1 == q2 && v1 == v2 -- TODO: alpha-rename this!
    = equationToAssignments (t1, t2) -- TODO: This is wrong, variable v1 could escape!
equationToAssignments _ = Nothing


addAssignmentToBinding :: (Ord v) => Bindings v -> Assignment v -> Maybe (Bindings v)
addAssignmentToBinding bind (v, t) = case M.lookup v bind of
    Just t2 -> addEquationToBinding bind (t, t2)
    Nothing -> justIf bindingOk $ M.insert v t bind

addEquationToBinding :: (Ord v) => Bindings v -> Equality v -> Maybe (Bindings v)
addEquationToBinding bind eq = do
    asss <- equationToAssignments eq
    foldM addAssignmentToBinding bind asss

bindingOk :: Ord v => Bindings v -> Bool
bindingOk bind = all (go S.empty) $ M.keys bind
  where
    go seen v = case M.lookup v bind of
        Just t -> goT (S.insert v seen) t
        Nothing -> True

    goT seen (Var v) | v `S.member` seen = False
                     | otherwise         = go seen v
    goT seen (Symb _ args) = all (goT seen) args
    goT seen (Quant _ v t) | v `S.member` seen = False -- TODO: think this through
                           | otherwise         = goT seen t


applyBinding :: (Ord v) => Bindings v -> Term v -> Term v
applyBinding bind (Var v) = case M.lookup v bind of
    Just t -> applyBinding bind t
    Nothing -> Var v
applyBinding bind (Symb f terms) = Symb f $ map (applyBinding bind) terms
applyBinding bind (Quant q v t) = Quant q v $ applyBinding bind t -- TODO: Avoid variable capture!

justIf :: (a -> Bool) -> a -> Maybe a
justIf p x = if p x then Just x else Nothing
