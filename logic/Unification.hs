{-# LANGUAGE LambdaCase, MultiWayIf #-}
module Unification
    ( Equality
    , Bindings
    , Unifiable
    , unifyLiberally
    , applyBinding
    , applyBinding'
    , applyBindingM
    , UnificationResult(..)
    ) where

import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
--import Debug.Trace
import Data.List

import Propositions

import Unbound.LocallyNameless
import Unbound.LocallyNameless.Fresh


type Equality = (Term, Term)

-- Invariant: If (v1, Var v2), then v1 <= v2
type Bindings = M.Map Var Term

type Unifiable = [Var]

emptyBinding :: Bindings
emptyBinding = M.empty


data UnificationResult = Solved | Failed | Dunno
    deriving Show

unifyLiberally :: Unifiable -> [(a,Equality)] -> (Bindings, [(a,UnificationResult)])
unifyLiberally uvs eqns = flip contFreshM highest $
    iter (uvs, emptyBinding) $ map (\(n,e) -> (n,Dunno,e)) eqns
  where
    highest = firstFree (uvs, map snd eqns)

    -- Repeatedly go through the list of equalities until we can solve no more
    iter :: Fresh m => (Unifiable, Bindings) -> [(a, UnificationResult, Equality)] -> m (Bindings, [(a, UnificationResult)])
    iter (uvs, bind) eqns = do
        ((uvs', bind'), eqns') <- runWriterT $ go (uvs,bind) eqns
        if M.size bind' > M.size bind
           then iter (uvs', bind') eqns'
           else return (bind', map (\(n,r,_) -> (n,r)) eqns')
                -- we learned something, so retry


    go :: Fresh m => (Unifiable, Bindings) -> [(a,UnificationResult, Equality)] -> WriterT [(a,UnificationResult, Equality)] m (Unifiable, Bindings)
    go uvs_bind [] = return uvs_bind
    go uvs_bind ((n,Dunno,x):xs) = do
        maybe_uvs_bind' <- lift $ runMaybeT (uncurry unif uvs_bind x)
        case maybe_uvs_bind' of
            Just (uvs',bind') -> do
                solved <- x `solvedBy` bind'
                if solved
                then tell [(n,Solved, x)] >> go (uvs',bind') xs
                -- Discard the results of this dunno,
                -- potentially less complete, but makes the output easier to understand
                else tell [(n,Dunno,  x)] >> go uvs_bind xs
            Nothing ->
                tell [(n, Failed, x)] >> go uvs_bind xs
    -- Do not look at solved or failed equations again
    go uvs_bind ((n,r,x):xs) = tell [(n,r,x)] >> go uvs_bind xs

-- Code taken from http://www21.in.tum.de/~nipkow/pubs/lics93.html

{-
fun unif (S,(s,t)) = case (devar S s,devar S t) of
        (x\s,y\t) => unif (S,(s,if x=y then t else subst x y t))
      | (x\s,t)   => unif (S,(s,t$(B x)))
      | (s,x\t)   => unif (S,(s$(B x),t))
      | (s,t)     => cases S (s,t)
-}
unif :: (MonadPlus m, Fresh m) => [Var] -> Bindings -> (Term, Term) -> m ([Var], Bindings)
unif uvs binds (s,t) = do
    s' <- devar binds s
    t' <- devar binds t
    case (s',t') of
        (Lam b1, Lam b2)
            -> lunbind2' b1 b2 $ \(Just (_,s,_,t)) -> unif uvs binds (s,t)
        (s,t) -> cases uvs binds (s,t)

{-
and cases S (s,t) = case (strip s,strip t) of
                      ((V F,ym),(V G,zn)) => flexflex(F,ym,G,zn,S)
                    | ((V F,ym),_)        => flexrigid(F,ym,t,S)
                    | (_,(V F,ym))        => flexrigid(F,ym,s,S)
                    | ((a,sm),(b,tn))     => rigidrigid(a,sm,b,tn,S)
-}
cases :: (MonadPlus m, Fresh m) => [Var] -> Bindings -> (Term, Term) -> m ([Var], Bindings)
cases uvs binds (s,t) = do
    case (strip s, strip t) of
        ((V f, ym), (V g, zn))
            | f `elem` uvs && g `elem` uvs -> flexflex  uvs binds f ym g zn
        ((V f, ym), _)
            | f `elem` uvs                 -> flexrigid uvs binds f ym t
        (_, (V g, zn))
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

flexflex :: (MonadPlus m, Fresh m) => [Var] -> Bindings -> Var -> [Term] -> Var -> [Term] -> m ([Var], Bindings)
flexflex uvs binds f ym g zn
    | f == g && ym == zn = return (uvs, binds)
    | f == g
    , Just ym' <- allBoundVar uvs ym
    = do
        newName <- fresh (string2Name "uni")
        let rhs = absTerm ym' (App (V newName) (intersect ym zn))
            binds' = M.insert f rhs binds
        return (newName:uvs, binds')
    | f == g = return (uvs, binds) -- non-pattern: Just ignore
    | Just ym' <- allBoundVar uvs ym
    , Just zn' <- allBoundVar uvs zn
    = if | all (`elem` zn') ym' -> do
             let rhs = absTerm zn' (App (V f) ym)
                 binds' = M.insert g rhs binds
             return (uvs, binds')
         | all (`elem` ym') zn' -> do
             let rhs = absTerm ym' (App (V g) zn)
                 binds' = M.insert f rhs binds
             return (uvs, binds')
         | otherwise -> do
             newName <- fresh (string2Name "uni")
             let rhs1 = absTerm ym' (App (V newName) (intersect ym zn))
                 rhs2 = absTerm zn' (App (V newName) (intersect ym zn))
                 binds' = M.insert f rhs1 $ M.insert g rhs2 binds
             return (newName:uvs, binds')
    | otherwise = return (uvs, binds) -- non-pattern: Just ignore

{-
fun occ F S (V G) = (F=G) orelse
                    (case assoc G S of Some(s) => occ F S s | None => false)
  | occ F S (s$t) = occ F S s orelse occ F S t
  | occ F S (_\s) = occ F S s
  | occ F S (_)   = false;
-}

occ :: Alpha b => Bindings -> Name Term -> b -> Bool
occ binds v t = v `elem` vars || any (maybe False (occ binds v) . (`M.lookup` binds)) vars
   where vars = fv t::[Var]


{-
fun flexrigid(F,ym,t,S) = if occ F S t then raise Unif
                          else proj (map B1 ym) (((F,abs(ym,t))::S),t);
-}
flexrigid :: (MonadPlus m, Fresh m) => [Var] -> Bindings -> Var -> [Term] -> Term -> m ([Var], Bindings)
flexrigid uvs binds f ym t
    | occ binds f t
    = mzero
    | Just vs <- allBoundVar uvs ym
    = let binds' = M.insert f (absTerm vs t) binds
      in proj vs uvs binds' t
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
proj :: (MonadPlus m, Fresh m) => [Var] -> [Var] -> Bindings -> Term -> m ([Var], Bindings)
proj w uvs binds s = do
    s' <- devar binds s
    case strip s' of
        (Lam b, _)
            -> lunbind' b $ \(x,t) -> proj (x:w) uvs binds t
        (V v, ss) | v `elem` uvs
            -> case allBoundVar uvs ss of
                Just vs | all (`elem` w) vs -> return (uvs, binds)
                Just vs -> do
                    newName <- fresh (string2Name "uni")
                    let rhs = absTerm vs (App (V newName) (ss ++ map V w))
                    let binds' = M.insert v rhs binds
                    return (newName:uvs, binds')

                -- Found a non-pattern use of a free variable, so recurse into the arguments
                -- (The correctness of this is a guess by Joachim)
                Nothing        -> recurse ss
                  | v `elem` w -> recurse ss
                  | otherwise  -> mzero
        (C _, ss)              -> recurse ss
        (App _ _, _)          -> error "Unreachable"
  where
    recurse = foldM (uncurry (proj w)) (uvs, binds)


allBoundVar :: Unifiable -> [Term] -> Maybe [Var]
allBoundVar _   []                         = return []
allBoundVar uvs (V x:xs) | x `notElem` uvs = (x:) <$> allBoundVar uvs xs
allBoundVar _ _                            = Nothing

{-
and rigidrigid(a,ss,b,ts,S) = if a <> b then raise Unif
                              else foldl unif (S,zip ss ts);
-}
rigidrigid :: (MonadPlus m, Fresh m) => [Var] -> Bindings -> Term -> [Term] -> Term -> [Term] -> m ([Var], Bindings)
rigidrigid uvs binds a sm b tn
    | a `aeq` b && length sm == length tn
    = foldM (uncurry unif) (uvs, binds) (zip sm tn)
    | otherwise
    = mzero


{-
fun devar S t = case strip t of
                  (V F,ys) => (case assoc F S of
                                 Some(t) => devar S (red t ys)
                               | None => t)
                | _ => t;
-}
devar :: Fresh m => Bindings -> Term -> m Term
devar binds t = case strip t of
    (V v,ys)
        | Just t <- M.lookup v binds
        -> redsTerm t ys >>= devar binds
    _   -> return t

redsTerm :: Fresh m => Term -> [Term] -> m Term
redsTerm t [] = return t
redsTerm (Lam b) (x:xs) = lunbind' b $ \(v,body) -> redsTerm (subst v x body) xs
redsTerm t xs = return $ App t xs

strip :: Term -> (Term, [Term])
strip t = go t []
  where go (App t args) args' = go t (args++args')
        go t             args' = (t, args')

solvedBy :: Fresh m => Equality -> Bindings -> m Bool
solvedBy (t1,t2) b = liftM2 aeq (applyBindingM b t1) (applyBindingM b t2)

-- | This is slow. If possible, use 'applyBinding'' or 'applyBindingM'
applyBinding :: Bindings -> Term -> Term
applyBinding bindings t = applyBinding' (firstFree (M.toList bindings,t)) bindings t

applyBinding' :: Integer -> Bindings -> Term -> Term
applyBinding' free bindings t = flip contFreshM free $ applyBindingM bindings t

applyBindingM :: Fresh m => Bindings -> Term -> m Term
applyBindingM bindings t = go [] t
  where
    go args (V v) | Just t <- M.lookup v bindings
                  = go args t
    go []   (V v) = return $ V v
    go args (V v) = App (V v) <$> mapM (go []) args

    go []   (C v) = return $ C v
    go args (C v) = App (C v) <$> mapM (go []) args


    go args (App t args') = go (args' ++ args) t

    go [] (Lam b) = lunbind' b $ \(v,body) -> Lam . bind v <$> go [] body
    go (x:xs) (Lam b) = lunbind' b $ \(v,body) -> go xs (subst v x body)

    -- This can be dropped once we only support GHC-7.10
    infixl 4 <$>
    (<$>) = liftM

lunbind' :: (Fresh m, Alpha p, Alpha t) => Bind p t -> ((p, t) -> m c) -> m c
lunbind' b c = unbind b >>= c

lunbind2' :: (Fresh m, Alpha p1, Alpha p2, Alpha t1, Alpha t2) => Bind p1 t1 -> Bind p2 t2 -> (Maybe (p1, t1, p2, t2) -> m r) -> m r
lunbind2' b1 b2 c = unbind2 b1 b2 >>= c

