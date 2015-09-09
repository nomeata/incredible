{-# LANGUAGE LambdaCase #-}
module Unification (Equality, Bindings, unifyLiberally, applyBinding) where

import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Debug.Trace
import Data.List

import Propositions

import Unbound.LocallyNameless


type Equality = (Term, Term)

-- Invariant: If (v1, Var v2), then v1 <= v2
type Bindings = M.Map Var AbsTerm

type Unifiable = [Var]

emptyBinding :: Bindings
emptyBinding = M.empty

unifyLiberally :: Unifiable -> [Equality] -> Bindings
unifyLiberally uvs eqns =
    snd $ runLFreshM $ avoid (fvAny eqns) $ foldM (uncurry unif) (uvs, emptyBinding) eqns

-- Code taken from http://www21.in.tum.de/~nipkow/pubs/lics93.html

{-
fun unif (S,(s,t)) = case (devar S s,devar S t) of
        (x\s,y\t) => unif (S,(s,if x=y then t else subst x y t))
      | (x\s,t)   => unif (S,(s,t$(B x)))
      | (s,x\t)   => unif (S,(s$(B x),t))
      | (s,t)     => cases S (s,t)
-}
unif :: LFresh m => [Var] -> Bindings -> (Term, Term) -> m ([Var], Bindings)
unif uvs binds (s,t) = do
    s' <- devar binds s
    t' <- devar binds t
    case (s',t') of
        (Quant q1 b1, Quant q2 b2)
            -> if q1 == q2
               then lunbind2 b1 b2 $ \(Just (_,s,_,t)) -> unif uvs binds (s,t)
               else return (uvs, binds) -- Error-ignoring
        -- no term level lambdas yet
        (s,t) -> cases uvs binds (s,t)

{-
and cases S (s,t) = case (strip s,strip t) of
                      ((V F,ym),(V G,zn)) => flexflex(F,ym,G,zn,S)
                    | ((V F,ym),_)        => flexrigid(F,ym,t,S)
                    | (_,(V F,ym))        => flexrigid(F,ym,s,S)
                    | ((a,sm),(b,tn))     => rigidrigid(a,sm,b,tn,S)
-}
cases :: LFresh m => [Var] -> Bindings -> (Term, Term) -> m ([Var], Bindings)
cases uvs binds (s,t) = do
    case (strip s, strip t) of
        ((Var f, ym), (Var g, zn))
            | f `elem` uvs && g `elem` uvs -> flexflex  uvs binds f ym g zn
        ((Var f, ym), _)
            | f `elem` uvs                 -> flexrigid uvs binds f ym t
        (_, (Var g, zn))
            | g `elem` uvs                 -> flexrigid uvs binds g zn s
        ((a, sm), (b, tn))
            -> rigidrigid uvs binds a sm b tn

{-
fun flexflex1(F,ym,zn,S) =
    if ym=zn then S else (F, hnf(ym, newV(), eqs ym zn)) :: S;

fun flexflex2(F,ym,G,zn,S) =
    if ym subset zn then (G, hnf(zn,V F,ym)) :: S else
    if zn subset ym then (F, hnf(ym,V G,zn)) :: S
    else let val xk  = ym /\ zn and H = newV()
         in (F, hnf(ym,H,xk)) :: (G, hnf(zn,H,xk)) :: S end;

fun flexflex(F,ym,G,zn,S) = if F=G then flexflex1(F,ym,zn,S)
                            else flexflex2(F,ym,G,zn,S);
-}

flexflex :: LFresh m => [Var] -> Bindings -> Var -> [Term] -> Var -> [Term] -> m ([Var], Bindings)
flexflex uvs binds f ym g zn
    | f == g && ym == zn = return (uvs, binds)
    | f == g
    , Just ym' <- mapM fromVar ym
    = do
        newName <- lfresh (string2Name "uni")
        let rhs = absTerm ym' (Symb (Var newName) (intersect ym zn))
        let binds' = M.insert f rhs binds
        return (newName:uvs, binds')
    | f == g = error "flexflex: Non-pattern: What to do?"
    | Just ym' <- mapM fromVar ym
    , Just zn' <- mapM fromVar zn
    = do
        newName <- lfresh (string2Name "uni")
        let rhs1 = absTerm ym' (Symb (Var newName) (intersect ym zn))
        let rhs2 = absTerm zn' (Symb (Var newName) (intersect ym zn))
        let binds' = M.insert f rhs1 $ M.insert g rhs2 binds
        return (newName:uvs, binds')
    | otherwise = error "flexflex: Non-pattern: What to do?"

{-
fun occ F S (V G) = (F=G) orelse
                    (case assoc G S of Some(s) => occ F S s | None => false)
  | occ F S (s$t) = occ F S s orelse occ F S t
  | occ F S (_\s) = occ F S s
  | occ F S (_)   = false;
-}

occ :: Alpha b => Bindings -> Name Term -> b -> Bool
occ binds v t = v `elem` vars || any (maybe False (occ binds v) . (`M.lookup` binds)) vars
   where vars = fv t


{-
fun flexrigid(F,ym,t,S) = if occ F S t then raise Unif
                          else proj (map B1 ym) (((F,abs(ym,t))::S),t);
-}
flexrigid :: LFresh m => [Var] -> Bindings -> Var -> [Term] -> Term -> m ([Var], Bindings)
flexrigid uvs binds f ym t
    | occ binds f t
    = return (uvs, binds) -- Error-ignoring
    | Just vs <- mapM fromVar ym
    = proj vs uvs (M.insert f (absTerm vs t) binds) t
    | otherwise
    = return (uvs, binds) -- not a pattern, ignoring here

{-
fun proj W (S,s) = case strip(devar S s) of
      (x\t,_)  => proj (x::W) (S,t)
    | (C _,ss) => foldl (proj W) (S,ss)
    | (B x,ss) => if x mem W then foldl (proj W) (S,ss) else raise Unif
    | (V F,ss) => if (map B1 ss) subset W then S
                  else (F, hnf(ss, newV(), ss /\ (map B W))) :: S;
-}
proj :: LFresh m => [Var] -> [Var] -> Bindings -> Term -> m ([Var], Bindings)
proj w uvs binds s = do
    s' <- devar binds s
    case strip s' of
        (Quant _ b, _)
            -> lunbind b $ \(x,t) -> proj (x:w) uvs binds t
        (Var v, ss) | v `elem` uvs
            -> case mapM fromVar ss of
                Just vs | all (`elem` w) vs -> return (uvs, binds)
                Just vs -> do
                    newName <- lfresh (string2Name "uni")
                    let rhs = absTerm vs (Symb (Var newName) (ss ++ map Var w))
                    let binds' = M.insert v rhs binds
                    return (newName:uvs, binds')
                Nothing -> error "What to do here?"
        (Var v, ss)
            -> foldM (uncurry (proj w)) (uvs, binds) ss
                -- Todo: How to distinguish constansts from bound variables here?
        (Symb _ _, _) -> error "Unreachable"


fromVar :: Term -> Maybe Var
fromVar (Var n) = Just n
fromVar _       = Nothing

{-
and rigidrigid(a,ss,b,ts,S) = if a <> b then raise Unif
                              else foldl unif (S,zip ss ts);
-}
rigidrigid :: LFresh m => [Var] -> Bindings -> Term -> [Term] -> Term -> [Term] -> m ([Var], Bindings)
rigidrigid uvs binds a sm b tn
    | a `aeq` b && length sm == length tn
    = foldM (uncurry unif) (uvs, binds) (zip sm tn)
    | otherwise -- we are a error-ignoring unification algorithm
    = return (uvs, binds)


{-
fun devar S t = case strip t of
                  (V F,ys) => (case assoc F S of
                                 Some(t) => devar S (red t ys)
                               | None => t)
                | _ => t;
-}
devar :: LFresh m => Bindings -> Term -> m Term
devar binds t = case strip t of
    (Var v,ys)
        | Just t <- M.lookup v binds
        -> redsAbsTerm t ys >>= devar binds
    _   -> return t

redsAbsTerm :: LFresh m => AbsTerm -> [Term] -> m Term
redsAbsTerm (AbsTerm t) args =
    lunbind t $ \(pats, body) -> return $ substs (zip pats args) body

strip :: Term -> (Term, [Term])
strip t = go t []
  where go (Symb t args) args' = go t (args++args')
        go t             args' = (t, args')


applyBinding :: Bindings -> Term -> Term
applyBinding bindings t = runLFreshM $ avoid (map AnyName (M.keys bindings)) $ go [] t
  where
    go args (Var v) | Just t <- M.lookup v bindings
                    = go [] =<< redsAbsTerm t args
    go []   (Var v) = return $ Var v
    go args (Var v) = Symb (Var v) <$> mapM (go []) args

    go args (Symb t args') = go (args' ++ args) t

    go [] (Quant q b) = lunbind b $ \(v,body) ->
        Quant q . bind v <$> go [] body

    go args t@(Quant _ _) = error $ "applyBinding: Quanitfier with arguments: " ++ show (t,args)
