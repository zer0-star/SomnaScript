module Language.SomnaScript.AST where

import           Data.Text                      ( Text )

data SmnExpr = SEInteger Integer
             | SEFloat Double
             | SEText Text
             | SEChar Char
             | SEBool Bool
             | SECall SmnExpr [SmnExpr]
             | SEVar Text
             | SEInfix SmnExpr SmnOp SmnExpr
             deriving Show

data SmnOp = SOPlus
           | SOMinus
           | SOMultiply
           | SODivide
           | SOEq
           | SOGt
           | SOGe
           | SOLt
           | SOLe
           | SOAnd
           | SOOr
           deriving Show

data SmnStmt = SSExpr SmnExpr
             | SSLet Text SmnExpr
             | SSVar Text SmnExpr
             | SSReturn SmnExpr
             | SSIfElse [SmnIfClause]
             | SSFunc Text [Text] [SmnStmt]

data SmnIfClause = SICIf SmnExpr [SmnStmt]
                 | SICElse [SmnStmt]
