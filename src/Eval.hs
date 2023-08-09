module Eval where

import Syntax
import qualified Data.Map as Map
import Data.Maybe
import Text.Printf
import Debug.Trace (trace)

----------
-- EVAL --
----------

-- type Env = Map.Map String Value

-- evalExpr :: Expr -> Env -> Either RunTimeError Value
-- evalExpr (EStr s) env = Right $ VStr s
-- evalExpr (EDouble n) env = Right $ VDouble n
-- evalExpr (EVar s) env = case Map.lookup s env of
--   Nothing -> Left $ EUnboundVar s
--   Just v -> Right v
-- evalExpr (EApp f x) env = do
--   f <- evalExpr f env
--   x <- evalExpr x env
--   case f of
--     VLam fName arg body lamEnv ->
--       let lamEnv' = Map.insert arg x lamEnv
--           lamEnv'' = maybe lamEnv' (\fName -> Map.insert fName f lamEnv') fName
--       in evalExpr body lamEnv''
--     VPrim _ f -> f x
--     _ -> Left ETyErr
-- evalExpr (ELam arg body) env = Right $ VLam Nothing arg body env
-- evalExpr (ELet [] body) env = evalExpr body env
-- evalExpr (ELet ((Decl name ty rhs) : decls) body) env = do
--   v <- evalExpr rhs env
--   let v' = case v of
--         VLam Nothing x body env -> VLam (Just name) x body env
--         _ -> v
--   let env' = Map.insert name v' env
--   evalExpr (ELet decls body) env'
-- evalExpr (EIf pred bTrue bFalse) env = do
--   v <- evalExpr pred env
--   case v of
--     VBool True -> evalExpr bTrue env
--     VBool False -> evalExpr bFalse env
--     _ -> Left ETyErr
-- evalExpr e env = Left $ EUnimplemented (show e)

------------
-- ERRORS --
------------

-- data RunTimeError
--   = EUnboundVar String
--   | ETyErr
--   | EUnimplemented String
--   deriving (Eq, Ord, Show)

------------
-- VALUES --
------------

-- data Value
--   = VUnit
--   | VInt Int
--   | VDouble Double
--   | VBool Bool
--   | VStr String
--   | VLam (Maybe String) String Expr Env
--   | VPrim String (Value -> Either RunTimeError Value)

-- instance Show Value where
--   show VUnit = "()"
--   show (VInt n) = show n
--   show (VDouble x) = show x
--   show (VBool b) = show b
--   show (VStr s) = s
--   show (VLam name arg body env) =
--     let name' = fromMaybe "anon" name
--      in printf "<fn %s %s %s>" name' arg (show (Map.keys env))
--   show (VPrim s f) = printf "<prim %s>" s
