{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Infra
  ( Subst
  , Substitutable
  , Unifiable
  , UnknownError
  , Rewritable
  , message
  , debug
  , substitute
  , unify
  , substituteInMap
  , substituteInSubst
  , unifySubsts
  , unifySubstsMany
  , unifyCtxs
  , throwUnknown
  , rewrite
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Except (MonadError)
import qualified Data.Set as Set
import Control.Monad (foldM)
import qualified Control.Applicative as Subst
import Debug.Trace (trace, traceShowId)
import Control.Monad.IO.Class (liftIO, MonadIO)

message s = liftIO $ putStrLn s

debug s a = liftIO $ putStrLn $ s ++ " " ++ show a

type Subst a = Map Int a

class Substitutable m a b where
  substitute :: Subst a -> b -> m b

class Substitutable m a a => Unifiable m a where
  unify :: a -> a -> m (Subst a)

class Rewritable a where
  rewrite :: (a -> a) -> a -> a

class (MonadError e m) => UnknownError m e a where
  throwUnknown :: String -> m a

substituteInMap :: (Monad m, Substitutable m a b) => Subst a -> Map k b -> m (Map k b)
substituteInMap s = mapM (substitute s)

substituteInSubst :: (Monad m, Unifiable m a) => Subst a -> Subst a -> m (Subst a)
substituteInSubst = substituteInMap

-- applies first substitution to second substitution
unifySubsts :: (MonadIO m, Show a, Unifiable m a, UnknownError m e a, UnknownError m e (a, Subst a)) =>
  Subst a -> Subst a -> m (Subst a)
unifySubsts s1 s2 = do
  let idxs1 = Map.keysSet s1
  let idxs2 = Map.keysSet s2
  let idxs = Set.union idxs1 idxs2
  foldM f Map.empty idxs
  where
    f sum i = do
      (x, s) <- getEntry i
      let sum' = Map.insert i x sum
      if Map.size s > 0
        then unifySubsts sum' s
        else return sum'
    getEntry i = case (Map.lookup i s1, Map.lookup i s2) of
      (Nothing, Nothing) -> throwUnknown "unifySubsts (Nothing, Nothing) impossible case"
      (Nothing, Just x) -> return (x, Map.empty)
      (Just x, Nothing) -> return (x, Map.empty)
      (Just x, Just y) -> do
        s <- unify x y
        x <- substitute s x
        return (x, s)

unifySubstsMany :: (MonadIO m, Show a, Unifiable m a, UnknownError m e a, UnknownError m e (a, Map Int a)) =>
  [Subst a] -> m (Subst a)
unifySubstsMany [] = return Map.empty
unifySubstsMany (s : substs) = foldM unifySubsts s substs

unifyCtxs :: (MonadIO m, Show a, Unifiable m a, UnknownError m e a, UnknownError m e (a, Map Int a), Ord k) =>
  Unifiable m a => Map k a -> Map k a -> m (Map k a)
unifyCtxs s1 s2 = do
  let idxs1 = Map.keysSet s1
  let idxs2 = Map.keysSet s2
  let idxs = Set.union idxs1 idxs2
  (ctxSum, substSum) <- foldM f (Map.empty, Map.empty) idxs
  substituteInMap substSum ctxSum
  where
    f (ctxSum, substSum) i = do
      (x, s) <- getEntry i
      let ctxSum' = Map.insert i x ctxSum
      if Map.size s > 0
        then do
          substSum' <- unifySubsts substSum s
          return (ctxSum', substSum')
        else return (ctxSum', substSum)
    getEntry i = case (Map.lookup i s1, Map.lookup i s2) of
      (Nothing, Nothing) -> throwUnknown "unifySubsts (Nothing, Nothing) impossible case"
      (Nothing, Just x) -> return (x, Map.empty)
      (Just x, Nothing) -> return (x, Map.empty)
      (Just x, Just y) -> do
        s <- unify x y
        x <- substitute s x
        return (x, s)