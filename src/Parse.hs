module Parse where

import Text.Megaparsec
import Control.Monad.Combinators
import Control.Monad.Combinators.Expr
import Syntax
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Identity (Identity (runIdentity), IdentityT (runIdentityT))
import Text.Megaparsec.Char (char, string, space1, letterChar, alphaNumChar)

import qualified Data.Set as Set
import Data.Char (isUpper)
import Data.Foldable (Foldable(foldl'))


add x y = 0 + x + y

id x = x

zero f x = x
suc n f x = f (n f x)

pAdd n m f x = n f (m f x)
pMul n m = n (pAdd m) zero

three = suc (suc (suc zero))
six = pAdd three three

x = pMul three six (add 1) 0



-- import Data.Foldable

-- identUpper =
-- identLower


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

instance ShowErrorComponent () where
  showErrorComponent () = "()"

type Parser a = ParsecT () String Identity a
type ExprParser = Parser PExpr

parseProgram file s = parse programP file s
parseDecl file s = parse declP file s
parseExpr file s = parse exprP file s

parseTestExpr s = parseTest (exprP <* eof) s

lineComment = L.skipLineComment "//"

blockComment =  L.skipBlockCommentNested "/*" "*/"

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment

lexeme = L.lexeme spaceConsumer
integer = lexeme L.decimal

symbol = L.symbol spaceConsumer

keyword s = lexeme (string s <* notFollowedBy alphaNumChar)

parens = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."

reservedList =
  [ "if"
  , "then"
  , "else"
  , "let"
  , "in"
  , "where"
  ]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
 where
   p       = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
   check x =
     if x `elem` reservedList
     then fail $ "keyword " ++ show x ++ " cannot be an identifier"
     else return x

-- tyConstructor :: Parser String
-- tyConstructor = _ satisfy (isUpper . head)


programP :: Parser Program
programP = many declP

declP :: Parser PDecl
declP = dValP

dValP = do
  keyword "let"
  binder <- identifier
  xs <- many identifier -- TODO: allow type annotations
  symbol "="
  body <- exprP
  let curried = curryLam xs body
  return $ DVal (XId binder) Nothing curried

exprP :: ExprParser
exprP =
  try appP
  <|> try closedP
  <|> try letP
  <|> try ifThenElseP
  <|> lamP
  <?> "expression"

-- exprP :: ExprParser
-- exprP = makeExprParser termP opTable <?> "expression"

closedP :: ExprParser
closedP =
  try unitP
  <|> try identP
  -- <|> try doubleP
  -- <|> try stringP
  <|> try (parens exprP)

unitP = symbol "()" >> return EUnit
identP = EVar . XId <$> identifier

-- doubleP = do
--   n <- T.naturalOrFloat lexer
--   return $ case n of
--     Right n -> EDouble n
--     Left n -> EDouble $ fromIntegral n

-- stringP =
--   EStr <$> string

curryLam xs body =
  let xs' = map XId xs in
  foldr ELam body xs'

appP = do
  f <- closedP
  es <- many closedP
  return $ foldl' EApp f es

lamP = do
  symbol "\\"
  xs <- some identifier
  symbol "."
  body <- exprP
  return $ curryLam xs body

letP = do
  keyword "let"
  x <- identifier
  args <- many identifier
  symbol "="
  e1 <- exprP
  keyword "in"
  e2 <- exprP
  let e1' = curryLam args e1
  return $ ELet (XId x) e1' e2

ifThenElseP = do
  keyword "if"
  cond <- exprP
  keyword "then"
  branchTrue <- exprP
  keyword "else"
  branchFalse <- exprP
  return $ EIf cond branchTrue branchFalse

-- expr = makeExprParser term table <?> "expression"

-- term = parens expr <|> integer <?> "term"

-- table = [ [ prefix  "-"  negate
--           , prefix  "+"  id ]
--         , [ postfix "++" (+1) ]
--         , [ binary  "*"  (*)
--           , binary  "/"  div  ]
--         , [ binary  "+"  (+)
--           , binary  "-"  (-)  ] ]

-- binary  name f = InfixL  (f <$ symbol name)
-- prefix  name f = Prefix  (f <$ symbol name)
-- postfix name f = Postfix (f <$ symbol name)

-- opTable :: [[Operator (ParsecT () String Identity) PExpr]]
-- opTable =
--   [ [appP]
--   , [letP]
--   , [ifThenElseP]
--   , [lamP]
--   ]
--   where
--     -- app = infixL space
--     -- space = _
--     --   (whiteSpace
--     --     *> notFollowedBy (choice . map reservedOp $ allReserved)) $> EApp
--     -- allReserved = T.reservedOpNames haskellDef ++ T.reservedNames haskellDef

-- infixN  name f = InfixN  (f <$ symbol name)
-- infixL  name f = InfixL  (f <$ symbol name)
-- infixR  name f = InfixR  (f <$ symbol name)
-- prefix  name f = Prefix  (f <$ symbol name)
-- postfix name f = Postfix (f <$ symbol name)


-- argP = do
--   identifier <$> AUntyped
--   <|> parens $ do
--     x <- identifier
--     reservedOp ":"
--     t <- identifier
--     return $ ATyped x t


-- appP = InfixL $ try $ do
--   spaceConsumer
--   return EApp

-- lamP = Prefix $ try $ do
--   symbol "\\"
--   xs <- some identifier
--   symbol "."
--   return $ curryLam xs

-- letP = Prefix $ try $ do
--   keyword "let"
--   x <- identifier
--   args <- many identifier
--   symbol "="
--   e1 <- exprP
--   keyword "in"
--   let e1' = curryLam args e1
--   return $ ELet (XId x) e1'

-- ifThenElseP = Prefix $ try $ do
--   keyword "if"
--   cond <- exprP
--   keyword "then"
--   branchTrue <- exprP
--   keyword "else"
--   return $ EIf cond branchTrue
