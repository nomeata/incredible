{-# LANGUAGE LambdaCase, MultiWayIf, ViewPatterns, PatternSynonyms #-}
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
import Control.Monad.Trans.Except
--import Debug.Trace
import Data.List

import Propositions

import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Internal.Fold


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
    iter ::(Unifiable, Bindings) -> [(a, UnificationResult, Equality)] -> FreshM (Bindings, [(a, UnificationResult)])
    iter (uvs, bind) eqns = do
        ((uvs', bind'), eqns') <- mapAccumM go (uvs,bind) eqns
        if M.size bind' > M.size bind
           then iter (uvs', bind') eqns'
           else return (bind', map (\(n,r,_) -> (n,r)) eqns')
                -- we learned something, so retry


    go :: (Unifiable, Bindings) -> (a,UnificationResult, Equality) -> FreshM ((Unifiable, Bindings), (a,UnificationResult, Equality))
    go uvs_bind (n,Dunno,x) = do
        either_uvs_bind' <- runUnifM (uncurry unif uvs_bind x)
        return $ case either_uvs_bind' of
            Left Dunno -> (uvs_bind,     (n,Dunno, x))
            Left Failed -> (uvs_bind, (n, Failed, x))
            Left Solved -> error "unreachable"
            Right (uvs',bind') -> ((uvs',bind'), (n,Solved, x))
    -- Do not look at solved or failed equations again
    go uvs_bind (n,r,x) = return (uvs_bind, (n,r,x))


mapAccumM :: Monad m => (s -> a -> m (s,b)) -> s -> [a] -> m (s,[b])
mapAccumM f s = go s
  where go s [] = return (s,[])
        go s (x:xs) = do
            (s',y) <- f s x
            (s'',ys) <- go s' xs
            return (s'', y:ys)

type UnifM a = ExceptT UnificationResult FreshM a

runUnifM :: UnifM a -> FreshM (Either UnificationResult a)
runUnifM = runExceptT

-- Code taken from http://www21.in.tum.de/~nipkow/pubs/lics93.html

{-
fun unif (S,(s,t)) = case (devar S s,devar S t) of
        (x\s,y\t) => unif (S,(s,if x=y then t else subst x y t))
      | (x\s,t)   => unif (S,(s,t$(B x)))
      | (s,x\t)   => unif (S,(s$(B x),t))
      | (s,t)     => cases S (s,t)
-}
unif :: [Var] -> Bindings -> (Term, Term) -> UnifM ([Var], Bindings)
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
cases :: [Var] -> Bindings -> (Term, Term) -> UnifM ([Var], Bindings)
cases uvs binds (s,t) = do
    case (strip s, strip t) of
        ((V f, allBoundVar uvs -> Just ym), (V g, allBoundVar uvs -> Just zn))
            | f `elem` uvs && g `elem` uvs -> flexflex  uvs binds f ym g zn
        ((V f, allBoundVar uvs -> Just ym), _)
            | f `elem` uvs                 -> flexrigid uvs binds f ym t
        (_, (V g, allBoundVar uvs -> Just zn))
            | g `elem` uvs                 -> flexrigid uvs binds g zn s
        ((V f, _), _)
            | f `elem` uvs                 -> throwE Dunno
        (_, (V g, _))
            | g `elem` uvs                 -> throwE Dunno
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

eqs :: Eq a => [a] -> [a] -> UnifM [a]
eqs (x:xs) (y:ys) | x == y    = (x :) `liftM` eqs xs ys
                  | otherwise =               eqs xs ys
eqs [] [] = return []
eqs _ _   = throwE Failed

flexflex :: [Var] -> Bindings -> Var -> [Var] -> Var -> [Var] -> UnifM ([Var], Bindings)
flexflex uvs binds f ym g zn
    | f == g && ym == zn = return (uvs, binds)
    | f == g
    = do
        newName <- fresh (string2Name "uni")
        args <- eqs ym zn
        let rhs = absTerm ym $ mkAppVars newName args
            binds' = M.insert f rhs binds
        return (newName:uvs, binds')
    | otherwise
    = if | all (`elem` zn) ym -> do
             let rhs = absTerm zn $ mkAppVars f ym
                 binds' = M.insert g rhs binds
             return (uvs, binds')
         | all (`elem` ym) zn -> do
             let rhs = absTerm ym $ mkAppVars g zn
                 binds' = M.insert f rhs binds
             return (uvs, binds')
         | otherwise -> do
             newName <- fresh (string2Name "uni")
             let body = mkAppVars newName (intersect ym zn)
                 rhs1 = absTerm ym body
                 rhs2 = absTerm zn body
                 binds' = M.insert f rhs1 $ M.insert g rhs2 binds
             return (newName:uvs, binds')

mkAppVars :: Var -> [Var] -> Term
mkAppVars f vs = mkApps (V f) $ map V vs

{-
fun occ F S (V G) = (F=G) orelse
                    (case assoc G S of Some(s) => occ F S s | None => false)
  | occ F S (s$t) = occ F S s orelse occ F S t
  | occ F S (_\s) = occ F S s
  | occ F S (_)   = false;
-}

occ :: Alpha b => Bindings -> Name Term -> b -> Bool
occ binds v t = v `elem` vars || any (maybe False (occ binds v) . (`M.lookup` binds)) vars
   where vars = toListOf fv t::[Var]


{-
fun flexrigid(F,ym,t,S) = if occ F S t then raise Unif
                          else proj (map B1 ym) (((F,abs(ym,t))::S),t);
-}
flexrigid :: [Var] -> Bindings -> Var -> [Var] -> Term -> UnifM ([Var], Bindings)
flexrigid uvs binds f ym t
    | occ binds f t
    = throwE Failed
    | otherwise
    = let binds' = M.insert f (absTerm ym t) binds
      in proj ym uvs binds' t

{-
fun proj W (S,s) = case strip(devar S s) of
      (x\t,_)  => proj (x::W) (S,t)
    | (C _,ss) => foldl (proj W) (S,ss)
    | (B x,ss) => if x mem W then foldl (proj W) (S,ss) else raise Unif
    | (V F,ss) => if (map B1 ss) subset W then S
                  else (F, hnf(ss, newV(), ss /\ (map B W))) :: S;
-}
proj :: [Var] -> [Var] -> Bindings -> Term -> UnifM ([Var], Bindings)
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
                    let rhs = absTerm vs (mkApps (V newName) (ss `intersect` map V w))
                    let binds' = M.insert v rhs binds
                    return (newName:uvs, binds')

                -- Found a non-pattern use of a free variable, so recurse into the arguments
                -- (The correctness of this is a guess by Joachim)
                Nothing        -> recurse ss
                  | v `elem` w -> recurse ss
                  | otherwise  -> throwE Failed
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
rigidrigid :: [Var] -> Bindings -> Term -> [Term] -> Term -> [Term] -> UnifM ([Var], Bindings)
rigidrigid uvs binds a sm b tn
    | a `aeq` b && length sm == length tn
    = foldM (uncurry unif) (uvs, binds) (zip sm tn)
    | otherwise
    = throwE Failed


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
redsTerm t xs = return $ mkApps t xs

strip :: Term -> (Term, [Term])
strip t = go t []
  where go (App t args) args' = go t (args++args')
        go t             args' = (t, args')

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
    go args (V v) = mkApps (V v) <$> mapM (go []) args

    go []   (C v) = return $ C v
    go args (C v) = mkApps (C v) <$> mapM (go []) args


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
