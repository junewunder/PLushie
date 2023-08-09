{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
{-# LANGUAGE InstanceSigs #-}
module Typecheck
  ( SchemaCtx
  , KindCtx
  , Context (..)
  , State (..)
  , TyError (..)
  , TyChecker
  , runTyChecker
  , checkProgram
  , checkExpr
  , emptyCtx
  , initialState
  ) where

import Infra

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import Syntax
import Control.Monad (zipWithM)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Fail
import Control.Monad.Identity (Identity, IdentityT)
import Control.Monad.Except (ExceptT, runExceptT, throwError, MonadError)
import Control.Monad.State hiding (State)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Foldable
import GHC.TopHandler (runMainIO)
import Data.Char (chr)
import Data.List (sort)

type SchemaCtx = Map X TSchema
type KindCtx = Map X Kind

type TySubst = Subst TType

data Context = Context
  { schCtx :: SchemaCtx
  , kindCtx :: KindCtx
  }
  deriving (Eq, Ord, Show)

data State = State
  { nextTUnifIdx :: Int
  , idk :: Int
  }
  deriving (Eq, Ord, Show)

data TyError
  = TEUnboundVar X
  | TEUnimplemented String
  | TEUnknown String
  | TECannotUnifyTypes TType TType
  | TECannotUnifyKinds Kind Kind
  deriving (Eq, Ord, Show)

newtype TyChecker a = TyChecker
  { unTyChecker :: ReaderT Context (StateT State (ExceptT TyError IO)) a }
  deriving
    ( Applicative
    , Monad
    , Functor
    , MonadReader Context
    , MonadState State
    , MonadError TyError
    , MonadIO
    )

-- runTyChecker :: ReaderT Context (StateT State (ExceptT TyError IO)) a
runTyChecker ::
    TyChecker a
  -> Context
  -> State
  -> IO (Either TyError (a, State))
runTyChecker m ctx st = runMainIO $ runExceptT (runStateT (runReaderT (unTyChecker m) ctx) st)

emptyCtx = Context Map.empty Map.empty

initialState = State 0 0

instance UnknownError TyChecker TyError a where
  throwUnknown = throwError . TEUnknown

instance Substitutable TyChecker Kind Kind where
  substitute s k = return k

instance Substitutable TyChecker TType TType where
  substitute s (TUnif i) = return $ fromMaybe (TUnif i) (Map.lookup i s)
  substitute s (TConstr x tys) = do
    tys <- mapM (substitute s) tys
    return $ TConstr x tys
  substitute s (TVar x) = return $ TVar x
  substitute s (TFunc ty ty') = do
    ty <- substitute s ty
    ty' <- substitute s ty'
    return $ TFunc ty ty'
  substitute s t = throwError $ TEUnimplemented $ "substitution on " ++ show t

instance Substitutable TyChecker TType TSchema where
  substitute :: Subst TType -> TSchema -> TyChecker TSchema
  substitute subst (SMono t) = do
    t <- substitute subst t
    return (SMono t)
  substitute subst (SForall x s) = do
    s <- substitute subst s
    return (SForall x s)

instance Unifiable TyChecker Kind where
  unify KStar KStar = return Map.empty
  unify (KArrow k1l k1r) (KArrow k2l k2r) = do
    s1 <- unify k1l k2l
    s2 <- unify k1r k2r
    unifySubsts s1 s2
  unify k1 k2 = throwError $ TECannotUnifyKinds k1 k2

instance Unifiable TyChecker (Type Int) where
  unify (TUnif i) (TUnif j) = return $ Map.singleton j (TUnif i)
  unify (TUnif i) t = return $ Map.singleton i t
  unify t (TUnif i) = return $ Map.singleton i t
  unify (TFunc i1 o1) (TFunc i2 o2) = do
    s1 <- unify i1 i2
    s2 <- unify o1 o2
    unifySubsts s1 s2
  unify (TConstr x ts1) (TConstr y ts2) = do
    if x /= y then
      throwError (TECannotUnifyTypes (TConstr x ts1) (TConstr y ts2))
    else do
      substs <- zipWithM unify ts1 ts2
      unifySubstsMany substs
  unify t1 t2 = throwError $ TECannotUnifyTypes t1 t2

getSchCtx :: TyChecker SchemaCtx
getSchCtx = asks schCtx

getKindCtx :: TyChecker KindCtx
getKindCtx = asks kindCtx

getVarSchema :: X -> TyChecker TSchema
getVarSchema x = do
  env <- getSchCtx
  let t = Map.lookup x env
  case t of
    Just ty -> return ty
    Nothing -> throwError $ TEUnboundVar x

extendSchemaCtx :: X -> TSchema -> Context -> Context
extendSchemaCtx x s ctx = ctx { schCtx = Map.insert x s $ schCtx ctx }

withExtendedSchemaCtx :: X -> TSchema -> TyChecker a -> TyChecker a
withExtendedSchemaCtx x s m = do
  local (extendSchemaCtx x s) m

withExtendedSchemaCtxMany :: [X] -> [TSchema] -> TyChecker a -> TyChecker a
withExtendedSchemaCtxMany xs ts m = do
  let xts = zip xs ts
  foldl' (\sum (x, t) -> withExtendedSchemaCtx x t sum) m xts

withSubstitutedSchemaCtx :: TySubst -> TyChecker a -> TyChecker a
withSubstitutedSchemaCtx s m = do
  ctx <- ask
  schCtx <- substituteInMap s (schCtx ctx)
  local (const (ctx { schCtx = schCtx })) m

withExtendedCtx :: Context -> TyChecker a -> TyChecker a
withExtendedCtx next m = do
  current <- ask
  let nextSchCtx = Map.union (schCtx next) (schCtx current)
  let nextKindCtx = Map.union (kindCtx next) (kindCtx current)
  let ctx = current { kindCtx = nextKindCtx , schCtx = nextSchCtx }
  local (const ctx) m

newUnifIdx :: TyChecker Int
newUnifIdx = do
  st <- get
  let i = nextTUnifIdx st
  put (st { nextTUnifIdx = nextTUnifIdx st + 1 })
  return i

substituteTyVar :: X -> Type u -> Type u -> Type u
substituteTyVar x replacement =
  rewrite (\t -> case t of
    TVar y | x == y -> replacement
    _ -> t)

instSch :: TSchema -> TyChecker TType
instSch (SMono t) = return t
instSch (SForall x s) = do
  i <- newUnifIdx
  t <- instSch s
  return $ substituteTyVar x (TUnif i) t

generalize :: TType -> TyChecker TSchema
generalize ty = do
  message "generalize"
  ctxUnifVars <- ctxUnifVars
  let unifIdxsSet = unifVars ty Set.\\ ctxUnifVars
  let unifIdxs = sort $ toList $ unifVars ty
  debug "unifIdxs" unifIdxs
  let unifs = [97..97 + length unifIdxs - 1] -- TODO: handle more than 26 unif vars
  let tyVarNames = map (XId . (:[]) . chr) unifs
  let tyVars :: [TType]
      tyVars = map TVar tyVarNames
  let namesAndVars = zip tyVarNames tyVars
  let subst = Map.fromList $ zip unifIdxs tyVars
  ty <- substitute subst ty
  message "/generalize"
  return $ foldr SForall (SMono ty) tyVarNames
  where
    unifVars (TUnif i) = Set.singleton i
    unifVars (TVar _) = Set.empty
    unifVars (TFunc i o) = unifVars i `Set.union` unifVars o
    unifVars (TConstr _ ts) = Set.unions $ map unifVars ts

    unifVarsSch = unifVars . getTyFromSch

    ctxUnifVars = do
      ctx <- getSchCtx
      let unifSets = map unifVarsSch (Map.elems ctx)
      return $ Set.unions unifSets

checkProgram :: PProgram -> TyChecker Context
checkProgram [] = do ask
checkProgram (d : ds) = do
  (ext, subst) <- checkDecl d
  withExtendedCtx ext (
    withSubstitutedSchemaCtx subst
      (checkProgram ds))

checkDecl :: PDecl -> TyChecker (Context, TySubst)
checkDecl (DVal name pt body) = do
  (bodyTy, subst) <- checkExpr body
  bodySch <- generalize bodyTy
  let ctx = extendSchemaCtx name bodySch emptyCtx
  return (ctx, subst)
checkDecl d = throwError $ TEUnimplemented $ show d

-- checkExpr :: PExprSource -> TyChecker (TType, TExprSource)
-- checkExpr e =

-- idea: in the state have a Map from source locations to types, potentially
-- give all exprs an ID, when need to find type of expr look up in state Map in
-- addition: could make initial pass over all expr IDs and give each a
-- unification variable in this context

checkExpr :: PExpr -> TyChecker (TType, TySubst)
checkExpr EUnit = return (TConstr (XId "unit") [], Map.empty)
checkExpr (EDouble _) = return (TConstr (XId "double") [], Map.empty)
checkExpr (EStr _) = return (TConstr (XId "string") [], Map.empty)

checkExpr (EVar x) = do
  schema <- getVarSchema x
  ty <- instSch schema
  return (ty, Map.empty)

checkExpr (ELam x body) = do
  i <- newUnifIdx
  let xTy = TUnif i
  withExtendedSchemaCtx x (SMono xTy) $ do
    (bodyTy, subst) <- checkExpr body
    let fnTy = TFunc xTy bodyTy
    fnTy <- substitute subst fnTy
    return (fnTy, subst)

checkExpr (EApp func arg) = do
  debug "checkExpr EApp:" (EApp func arg)
  (funcTy, funcSubst) <- checkExpr func
  debug "checkExpr EApp funcTy:" funcTy
  debug "checkExpr EApp funcSubst:" (Map.keys funcSubst)
  (argTy, argSubst) <- checkExpr arg
  debug "checkExpr EApp argTy:" argTy
  debug "checkExpr EApp argSubst:" (Map.keys argSubst)
  subst <- unifySubsts funcSubst argSubst
  debug "checkExpr EApp subst:" subst
  i <- newUnifIdx
  j <- newUnifIdx
  let inTy = TUnif i
  let outTy = TUnif j
  funcUnifSubst <- unify (TFunc inTy outTy) funcTy
  subst <- unifySubstsMany [funcSubst, argSubst, funcUnifSubst]
  inTy <- substitute subst inTy
  outTy <- substitute subst outTy
  inTySubst <- unify inTy argTy
  subst <- unifySubsts subst inTySubst
  return (TFunc inTy outTy, subst)

-- checkExpr (ELet boundX bound body) = do

--   -- DON"T TRY TO BE TOO CLEVER HERE
--   -- TODO: VALUES CANNOT NOT BE RECURSIVE

--   -- i <- newUnifIdx
--   -- let boundTyUnif = TUnif i
--   (boundTy, boundSubst) <- checkExpr bound
--   throwError $ TEUnimplemented $ show (ELet boundX bound body)
--   -- boundUnifSubst <- unify boundTyUnif boundTy
--   -- boundSubst <- unifySubsts boundSubst boundUnifSubst
--   -- boundTy <- substitute boundSubst boundTy
--   -- boundSch <- generalize boundTy
--   -- (bodyTy, bodySubst) <- withExtendedSchemaCtx boundX boundSch (checkExpr body)
--   -- subst <- unifySubsts boundSubst bodySubst
--   -- bodyTy <- substitute subst bodyTy
--   -- return (bodyTy, subst)

-- -- checkExpr (EIf ex ex' ex2) = _

checkExpr e = throwError $ TEUnimplemented $ show e