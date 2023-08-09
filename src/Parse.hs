module Parse where

import Text.Megaparsec
import Syntax
-- import Text.Megaparsec.String

-- import qualified Text.Megaparsec.Token as T
-- import Text.Megaparsec.Language
-- import Text.Megaparsec.Expr
-- import Debug.Trace
-- import Data.Functor
-- import Data.Char (isLower)

-- lexer = T.makeTokenParser haskellDef
-- parens = T.parens lexer
-- brackets = T.brackets lexer
-- braces = T.braces lexer
-- colon = T.colon lexer
-- identifier = T.identifier lexer
-- reserved = T.reserved lexer
-- reservedOp = T.reservedOp lexer
-- symbol = T.symbol lexer
-- commaSep = T.commaSep lexer
-- integer = T.integer lexer
-- whiteSpace = T.whiteSpace lexer

-- -- tyConstructor = do
-- --   name <- identifier
-- --   do

-- --   _
-- --   -- runParserT (do satisfy isLower) _ _ _
-- --   return name

-- --   -- if isLower $ head name then _ else _


-- -- tyVar = XId <$> identifier

-- programP :: Parser [PDecl]
-- programP =
--   many declP

-- declP :: Parser PDecl
-- declP = do
--   name <- identifier
--   colon
--   ty <- typeP
--   reservedOp "="
--   e <- exprP
--   reserved ";"
--   return $ DVal (XId name) (Just ty) e

-- typeP :: Parser ParsedType
-- typeP = buildExpressionParser tyOpTable tyTermP

-- tyTermP :: Parser ParsedType
-- tyTermP =
--   parens typeP
--   <|> primTypeP "double"
--   <|> primTypeP "string"
--   <|> primTypeP "bool"
--   <|> primTypeP "unit"
--   <|> TVar . XId <$> identifier
--   <|> do
--     constr <- identifier

--     _


-- primTypeP :: String -> Parser ParsedType
-- primTypeP name = do reserved name; return $ TConstr (XId name) []

-- tyOpTable =
--   [ [binary "->" TFunc AssocRight]
--   ]
--   where
--     binary name label = Infix (do
--       reservedOp name
--       return label)
--     -- constr =
--     -- app = Infix space AssocLeft
--     -- space =
--     --   whiteSpace
--     --     *> notFollowedBy (choice . map reservedOp $ allReserved) $> _
--     -- allReserved = T.reservedOpNames haskellDef ++ T.reservedNames haskellDef

-- exprP :: Parser PExpr
-- exprP = buildExpressionParser opTable termP

-- termP :: Parser PExpr
-- termP =

--     identP
--     <|> unitP
--     <|> doubleP
--     <|> stringP
--     <|> lamP
--     <|> letP
--     <|> ifP
--     <|> parens exprP

-- opTable =
--   [ [app]
--   ]
--   where
--     binary name label = Infix (do
--       reservedOp name
--       return label)
--     app = Infix space AssocLeft
--     space =
--       (whiteSpace
--         *> notFollowedBy (choice . map reservedOp $ allReserved)) $> EApp
--     allReserved = T.reservedOpNames haskellDef ++ T.reservedNames haskellDef

-- unitP = reserved "()" >> return EUnit
-- identP = EVar . XId <$> identifier
-- doubleP = do
--   n <- T.naturalOrFloat lexer
--   return $ case n of
--     Right n -> EDouble n
--     Left n -> EDouble $ fromIntegral n

-- stringP =
--   EStr <$> T.stringLiteral lexer

-- curryLam xs body =
--   let xs' = map XId xs in
--   foldr ELam body xs'

-- -- argP = do
-- --   identifier <$> AUntyped
-- --   <|> parens $ do
-- --     x <- identifier
-- --     reservedOp ":"
-- --     t <- identifier
-- --     return $ ATyped x t

-- lamP = do
--   reservedOp "\\"
--   xs <- many1 identifier
--   reserved "."
--   curryLam xs <$> exprP

-- letP = do
--   reserved "let"
--   x <- identifier
--   args <- many identifier
--   reserved "="
--   e1 <- exprP
--   reserved "in"
--   e2 <- exprP
--   let e1' = curryLam args e1
--   return $ ELet (XId x) e1' e2

-- ifP = do
--   reserved "if"
--   e1 <- exprP
--   reserved "then"
--   e2 <- exprP
--   reserved "else"
--   EIf e1 e2 <$> exprP

