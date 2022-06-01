module Language.SomnaScript.Value where

import           Data.Text                      ( Text )

import           Language.SomnaScript.AST

data SmnValue = SVInteger Integer
              | SVFloat Double
              | SVText Text
              | SVChar Char
              | SVBool Bool
              | SVUnit
              | SVFunction [Text] SmnExpr
              | SVPrimitive SmnPrimitive
              deriving (Show)

data SmnPrimitive = SPPrint
                  | SPPrintln
                  deriving (Show)
