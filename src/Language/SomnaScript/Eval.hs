module Language.SomnaScript.Eval where

import qualified Data.Map                      as M
import           Data.Text                      ( Text )

import           Language.SomnaScript.AST
import           Language.SomnaScript.Value


type Environment = M.Map Text (SmnValue, Bool)

initialEnv :: Environment
initialEnv = M.fromList
  [primitive "pritn" SPPrint, primitive "println" SPPrintln]
  where primitive ident p = (ident, (SVPrimitive p, False))

eval :: Environment -> SmnExpr -> SmnValue
eval env (SEInteger v) = SVInteger v
eval env (SEFloat   v) = SVFloat v
eval env (SEText    v) = SVText v
eval env (SEChar    v) = SVChar v
eval env (SEBool    v) = SVBool v
eval env (SEVar     x) = case M.lookup x env of
  Just (v, _) -> v
  Nothing     -> error $ "error: undefined variable " <> show x
eval env (SECall f args) =
  let f'    = eval env f
      args' = map (eval env) args
  in  case f' of
        SVFunction xs body -> if length xs == length args
          then
            let env' =
                  M.union (M.fromList $ zip xs (zip args' $ repeat True)) env
            in  eval env' body
          else
            error
            $  "error: expected "
            <> show (length xs)
            <> " argument(s) but got "
            <> show (length args)
        _ -> error "error: type mismatch: expected function"
eval env (SEInfix e1 op e2) = case (eval env e1, op, eval env e2) of
  (SVInteger v1, SOPlus    , SVInteger v2) -> SVInteger $ v1 + v2
  (SVFloat   v1, SOPlus    , SVFloat v2  ) -> SVFloat $ v1 + v2
  (SVText    v1, SOPlus    , SVText v2   ) -> SVText $ v1 <> v2
  (SVInteger v1, SOMinus   , SVInteger v2) -> SVInteger $ v1 - v2
  (SVFloat   v1, SOMinus   , SVFloat v2  ) -> SVFloat $ v1 - v2
  (SVInteger v1, SOMultiply, SVInteger v2) -> SVInteger $ v1 * v2
  (SVFloat   v1, SOMultiply, SVFloat v2  ) -> SVFloat $ v1 * v2
  (SVInteger v1, SODivide  , SVInteger v2) -> SVInteger $ v1 `div` v2
  (SVFloat   v1, SODivide  , SVFloat v2  ) -> SVFloat $ v1 / v2
  (SVBool    v1, SOAnd     , SVBool v2   ) -> SVBool $ v1 && v2
  (SVBool    v1, SOOr      , SVBool v2   ) -> SVBool $ v1 || v2
  (SVInteger v1, SOEq      , SVInteger v2) -> SVBool $ v1 == v2
  (SVFloat   v1, SOEq      , SVFloat v2  ) -> SVBool $ v1 == v2
  (SVText    v1, SOEq      , SVText v2   ) -> SVBool $ v1 == v2
  (SVChar    v1, SOEq      , SVChar v2   ) -> SVBool $ v1 == v2
  (SVBool    v1, SOEq      , SVBool v2   ) -> SVBool $ v1 == v2
  (SVInteger v1, SOGt      , SVInteger v2) -> SVBool $ v1 > v2
  (SVFloat   v1, SOGt      , SVFloat v2  ) -> SVBool $ v1 > v2
  (SVInteger v1, SOGe      , SVInteger v2) -> SVBool $ v1 >= v2
  (SVFloat   v1, SOGe      , SVFloat v2  ) -> SVBool $ v1 >= v2
  (SVInteger v1, SOLt      , SVInteger v2) -> SVBool $ v1 < v2
  (SVFloat   v1, SOLt      , SVFloat v2  ) -> SVBool $ v1 < v2
  (SVInteger v1, SOLe      , SVInteger v2) -> SVBool $ v1 <= v2
  (SVFloat   v1, SOLe      , SVFloat v2  ) -> SVBool $ v1 <= v2
  (_, _, _) -> error "error: type mismatch at infix"
