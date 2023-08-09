module Main where

-- import Parse
-- import Text.Parsec (parse)
import Syntax
-- import Eval
import qualified Data.Map as Map
import Debug.Trace ( trace )
import Typecheck
import Typecheck(Context)
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.State (StateT(runStateT))
import Control.Monad.Except (runExceptT)

-- tracePrim :: Value -> Either RunTimeError Value
-- tracePrim x = Right $ trace (show x) x

-- binopDoublePrim :: String -> (Double -> Double -> Double) -> (String, Value)
-- binopDoublePrim name op = (name, VPrim (name ++ "1") f1)
--   where
--     f1 (VDouble x) = Right $ VPrim (name ++ "2") (f2 x)
--     f1 _ = Left ETyErr

--     f2 x (VDouble y) = Right $ VDouble (op x y)
--     f2 x _ = Left ETyErr

-- binopDoubleBoolPrim :: String -> (Double -> Double -> Bool) -> (String, Value)
-- binopDoubleBoolPrim name op = (name, VPrim (name ++ "1") f1)
--   where
--     f1 (VDouble x) = Right $ VPrim (name ++ "2") (f2 x)
--     f1 _ = Left ETyErr

--     f2 x (VDouble y) = Right $ VBool (op x y)
--     f2 x _ = Left ETyErr

-- env :: Env
-- env =
--   Map.fromList
--     [ ("trace", VPrim "trace" tracePrim)
--     , binopDoublePrim "add" (+)
--     , binopDoublePrim "sub" (-)
--     , binopDoublePrim "mul" (*)
--     , binopDoublePrim "div" (/)
--     , binopDoubleBoolPrim "eq" (==)
--     ]

tyCheckFile file = do
  -- program <- readFile file
  -- let ast = case parse programP "" program of
  --       Left err -> error $ show err
  --       Right e -> e
  -- print ast
  let ast =
        [ DVal (XId "foo") Nothing (EVar (XId "bar"))
        , DVal (XId "id") Nothing (ELam (XId "x") (EVar . XId $ "x"))
      --   , DVal (XId "baz") Nothing (ELet (XId "flop") (EVar $ XId "flop") (EVar (XId "")))
        , DVal (XId "addd") Nothing (ELam (XId "x") (ELam (XId "y") (EApp (EApp (EVar . XId $ "add") (EVar . XId $ "x")) (EVar . XId $ "y") )))
        ]
  let initSchCtx = Map.fromList
        [ (XId "bar", SForall (XId "a") (SMono $ TVar (XId "a")))
        , (XId "add", SMono (TFunc (TConstr (XId "int") []) (TFunc (TConstr (XId "int") []) (TConstr (XId "int") []))))
        ]
  let initKindCtx = Map.fromList
        [ (XId "double", KStar)
        ]
  let ctx = Context initSchCtx initKindCtx
  let st = initialState
  idk <- runTyChecker (checkProgram ast) ctx st
  print idk
  -- let v = eval ast env
  -- print v

main :: IO ()
main = do
  -- evalFile "examples/peano.lush"
  -- evalFile "examples/recursion.lush"
  tyCheckFile "examples/types.lush"
