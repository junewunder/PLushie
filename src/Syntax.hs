module Syntax where

import Data.List
import Data.Void (Void)
import Infra
import qualified Data.List as List

data X
  = XId String
  | XDot String X
  deriving (Eq, Ord, Show)

data Kind
  = KStar
  | KArrow Kind Kind
  deriving (Eq, Ord, Show)

-- "u" is the parameter for type unification variables
data Type u
  = TVar X
  | TUnif u
  | TConstr X [Type u]
  | TFunc (Type u) (Type u)
  deriving (Eq, Ord, Show)

type ParsedType = PType

type SchemaSource tu = Annot (Schema tu)
data Schema tu
  = SMono (Type tu)
  | SForall X (Schema tu)
  deriving (Eq, Ord, Show)

type ExprSource = Annot Expr
data Expr
  = EUnit
  | EStr String
  | EDouble Double
  | EVar X
  | EApp Expr Expr
  | ELam X Expr
  | ELet X Expr Expr
  | EIf Expr Expr Expr
  deriving (Eq, Ord, Show)

type DeclSource = Annot Decl
data Decl
  = DVal X (Maybe ParsedType) Expr -- arguments should be unfolded at the parser level as syntax sugar
  | DRecType X
  deriving (Eq, Ord, Show)

data Annot a = Annot
  { val :: a
  , loc :: ()
  }
  deriving (Eq, Ord, Show)

type Program = [Decl]

type PProgram = Program
type PDecl = Decl
type PExpr = Expr
type PSchema = Schema ()
type PType = Type ()

type TProgram = Program
type TDecl = Decl
type TExpr = Expr
type TSchema = Schema Int
type TType = Type Int

type CProgram = Program
type CDecl = Decl
type CExpr = Expr
type CSchema = Schema Void
type CType = Type Void

instance Rewritable (Type u) where
  rewrite f (TVar x) = f (TVar x)
  rewrite f (TUnif u) = f (TUnif u)
  rewrite f (TConstr x tys) =
    f (TConstr x (List.map (rewrite f) tys))
  rewrite f (TFunc ty1 ty2) =
    f (TFunc (rewrite f ty1) (rewrite f ty2))

getTyFromSch (SMono t) = t
getTyFromSch (SForall _ s) = getTyFromSch s
