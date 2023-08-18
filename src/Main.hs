module Main where

import Parse
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
import Control.Monad.Identity (Identity(runIdentity))
import Text.Megaparsec
import GHC.Base (failIO)
import Infra (debug, message)

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

tyCheckProgram ctx st prog = do
  res <- runTyChecker (checkProgram prog) ctx st
  case res of
    Right (ctx, st) -> do
      print st
      print ctx
      print $ schCtx ctx Map.! XId "_"
    Left e -> do print e


tyCheckFile file = do
  putStrLn ""
  parseTestExpr "if () then () else ()"
  putStrLn ""
  parseTestExpr "() () () ()"
  putStrLn ""
  parseTestExpr "let u = () in ()"
  putStrLn ""
  parseTestExpr "let x = () in \n let y = () in \n ()"
  putStrLn ""
  parseTestExpr "let x = \\blop. hey in \n let y = () in \n ()"
  putStrLn ""

  program <- readFile file
  case parseProgram (show file) program of
    Left err -> do
      putStrLn $ errorBundlePretty err
      failIO ""
    Right ast -> do
      print ast
      let initSchCtx = Map.fromList
            [ (XId "bar", SForall (XId "a") (SMono $ TVar (XId "a")))
            , (XId "add", SMono (TFunc (TConstr (XId "int") []) (TFunc (TConstr (XId "int") []) (TConstr (XId "int") []))))
            ]
      let initKindCtx = Map.fromList
            [ (XId "double", KStar)
            ]
      let ctx = Context initSchCtx initKindCtx
      let st = initialState
      -- idk <- runTyChecker (checkProgram ast) ctx st
      message ""
      res <- runTyChecker (checkProgram ast) ctx st
      case res of
        Right (ctx, st) ->
          mapM_ (\((XId k), v) -> putStrLn $ k ++ " = " ++ show v) (Map.assocs $ schCtx ctx)
        Left err -> do print err

main :: IO ()
main = do
  tyCheckFile "examples/peano.lush"
  -- evalFile "examples/recursion.lush"
  -- tyCheckFile "examples/types.lush"
