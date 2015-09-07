{-# LANGUAGE LambdaCase #-}
module Unification where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Data.Maybe

import Propositions

import Unbound.LocallyNameless

{-
General idea: We start from equations between arbitrary terms (type Equality),
and decompose that to either a unification error, or a list of equations
(type Assignment) where the left hand side is headed by a unification variable
(type Application). This is done in equationToAssignments.

Example:
    Input: (with only g in the list of unification variables)
        f(g(t1,t2)) = f(h(t1,i(t2)),c)
    Output
        (g, [t1,t2]) ~ h(t1,h(t2)),c)

From the assignments we build a mapping from variables to (possibly
lambda-abstracted terms), type Bindings.

An assignment for a variable not in the map yet is turned into a lambda
abstraction, by replacing the arguments from the left-hand-side, as they occur
on the right hand side, by temporary variables.

Example:
        g := λx y. h(x,h(y)),c)

This is a heuristic: There might be other lambda expressions solving this
equation!

If the variable already occurs in the map, then we instantiate that lambda
expression with the given arguments and unify it with the right hand side.

Example: Adding to the above map the assignment
        (g, [s1,s2]) ~ s3
we produce the equation
        h(s1,h(s2)),c) = s3
and start again.

-}

type Application = (Var, [Term])
type Assignment = (Application, Term)
type Equality = (Term, Term)

-- Invariant: If (v1, Var v2), then v1 <= v2
type Bindings = M.Map Var AbsTerm

type Unifiable = [Var]

emptyBinding :: Bindings
emptyBinding = M.empty

-- See above
equationToAssignments :: Unifiable -> Equality -> LFreshMT Maybe [Assignment]
equationToAssignments uvs (Var v1, Var v2)
    | v1 == v2
    = return $ []
    | v1 `elem` uvs && v2 `elem` uvs
    = return $ [((min v1 v2, []), Var (max v1 v2))]
equationToAssignments uvs (Var v,  t)
    | v `elem` uvs
    = return $ [((v, []), t)]
equationToAssignments uvs (t,      Var v)
    | v `elem` uvs
    = return $ [((v, []), t)]
equationToAssignments uvs (Symb f1 args1, Symb f2 args2)
    | f1 == f2 && length args1 == length args2
    = do
        concat <$> mapM (equationToAssignments uvs) (zip args1 args2)
equationToAssignments uvs (Symb (Var f1) args1, t2)
    | f1 `elem` uvs
    = return $ [((f1, args1), t2)]
equationToAssignments uvs (t1, Symb (Var f2) args2)
    | f2 `elem` uvs
    = return $ [((f2, args2), t1)]
equationToAssignments uvs (Quant q1 t1, Quant q2 t2)
    | q1 == q2
    = lunbind2 t1 t2 $ \case
        Just (v, body1, _, body2) -> do
            equationToAssignments uvs (body1, body2) -- TODO: This is wrong, variable v could escape!
        Nothing -> error "equationToAssignments: lunbind2 failed"
equationToAssignments _ _ = mzero


addAssignmentToBinding :: Unifiable -> Bindings -> Assignment -> LFreshMT Maybe Bindings
addAssignmentToBinding uvs bind ((v, args), t) = case M.lookup v bind of
    Just (AbsTerm t2) -> lunbind t2 $ \(pats, t3) -> do
            -- TODO: check lengths of args and pats
            let t4 = substs (zip pats args) t3
            addEquationToBinding uvs bind (t, t4)
    Nothing -> do
        t' <- substTerms subst t
        justIf bindingOk $ M.insert v (absTerm tmpVars t') bind
  where
    -- TODO: Use fresh names
    tmpVars = [makeName "tmp" (fromIntegral n) | n <- [1..length args]]
    subst = zip args (map Var tmpVars)

substTerms :: (LFresh m, Applicative m) => [(Term, Term)] -> Term -> m Term
substTerms subst = go
  where go t | Just t' <- lookup t subst = return t'
        go (Symb t ts) = Symb <$> go t <*> mapM go ts
        go (Var v) = return $ Var v
        go (Quant q b) = lunbind b $ \(v,body) ->
            Quant q . bind v <$> go body

addEquationToBinding :: Unifiable -> Bindings -> Equality -> LFreshMT Maybe Bindings
addEquationToBinding uvs bind eq = do
    asss <- equationToAssignments uvs eq
    foldM (addAssignmentToBinding uvs) bind asss

unifyEquationsWherePossible :: Unifiable -> [Equality] -> Bindings
unifyEquationsWherePossible uvs = foldl consider emptyBinding
  where
    consider bind equation = fromMaybe bind $ runLFreshMT $ addEquationToBinding uvs bind equation

bindingOk :: Bindings -> Bool
bindingOk bind = all (go S.empty) $ M.keys bind
  where
    go seen v = case M.lookup v bind of
        Just t ->  all (go' (S.insert v seen)) (fv t)
        Nothing -> True

    go' seen v | v `S.member` seen = False
               | otherwise         = go seen v


applyBinding :: Bindings -> Term -> Term
applyBinding bindings t = runLFreshM $ avoid (map AnyName (M.keys bindings)) $ go [] t
  where
    go args (Var v) | Just (AbsTerm t) <- M.lookup v bindings
                    = lunbind t $ \(pats, body) -> go [] (substs (zip pats args) body)
    go []   (Var v) = return $ Var v
    go args (Var v) = Symb (Var v) <$> mapM (go []) args

    go args (Symb t args') = go (args' ++ args) t

    go [] (Quant q b) = lunbind b $ \(v,body) ->
        Quant q . bind v <$> go [] body

    go args t@(Quant _ _) = error $ "applyBinding: Quanitfier with arguments: " ++ show (t,args)


justIf :: MonadPlus m => (a -> Bool) -> a -> m a
justIf p x = if p x then return x else mzero
