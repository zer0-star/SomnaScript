module Language.SomnaScript.Eval where

import           Control.Monad.Cont
import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Language.SomnaScript.AST
import           Language.SomnaScript.Value


type Environment = M.Map Text (SmnValue, Bool)

initialEnv :: Environment
initialEnv = M.fromList
  [primitive "print" SPPrint, primitive "println" SPPrintln]
  where primitive ident p = (ident, (SVPrimitive p, False))

eval :: Environment -> SmnExpr -> IO SmnValue
eval env (SEInteger v) = return $ SVInteger v
eval env (SEFloat   v) = return $ SVFloat v
eval env (SEText    v) = return $ SVText v
eval env (SEChar    v) = return $ SVChar v
eval env (SEBool    v) = return $ SVBool v
eval env (SEVar     x) = case M.lookup x env of
  Just (v, _) -> return v
  Nothing     -> error $ "error: undefined variable " <> show x
eval env (SECall f args) = do
  f'    <- eval env f
  args' <- mapM (eval env) args
  case f' of
    SVFunction xs body -> if length xs == length args'
      then
        let env' = M.union (M.fromList $ zip xs (zip args' $ repeat True)) env
        in  execFunc env' body
      else
        error
        $  "error: expected "
        <> show (length xs)
        <> " argument(s) but got "
        <> show (length args)
    SVPrimitive p -> evalPrimitive p args'
    _             -> error "error: type mismatch: expected function"
eval env (SEInfix e1 op e2) = do
  e1' <- eval env e1
  e2' <- eval env e2
  return case (e1', op, e2') of
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

toString :: SmnValue -> String
toString (SVInteger v)   = show v
toString (SVFloat   v)   = show v
toString (SVText    v)   = T.unpack v
toString (SVChar    v)   = [v]
toString (SVBool    v)   = if v then "true" else "false"
toString SVUnit          = "()"
toString SVFunction{}    = "<Function>"
toString (SVPrimitive _) = "<Primitive>"

evalPrimitive :: SmnPrimitive -> [SmnValue] -> IO SmnValue
evalPrimitive SPPrint args = do
  putStr . unwords $ map toString args
  return SVUnit
evalPrimitive SPPrintln args = do
  putStrLn . unwords $ map toString args
  return SVUnit

execFunc :: Environment -> [SmnStmt] -> IO SmnValue
execFunc env body = runContT (exec env body) return

exec :: Environment -> [SmnStmt] -> ContT SmnValue IO SmnValue
exec env []                     = return SVUnit
exec env (SSEmpty       : rest) = exec env rest
exec env (SSExpr e      : rest) = lift (eval env e) >> exec env rest
exec env (SSLet ident e : rest) = do
  v <- lift $ eval env e
  let env' = M.insert ident (v, False) env
  exec env' rest
exec env (SSVar ident e : rest) = do
  v <- lift $ eval env e
  let env' = M.insert ident (v, True) env
  exec env' rest
exec env (SSReturn e : rest) = ContT . const $ eval env e
exec env (SSIfElse ifs el : rest) =
  let go [] | Just el' <- el = exec env el'
            | otherwise      = return SVUnit
      go ((cond, body) : rest) = do
        cond' <- lift $ eval env cond
        case cond' of
          SVBool v -> if v then exec env body else go rest
          _        -> error "error: type mismatch on condition"
  in  go ifs >> exec env rest
exec env (SSFunc name args body : rest) =
  let env' = M.insert name (SVFunction args body, False) env in exec env' rest
exec env (SSAssign var e : rest) = case M.lookup var env of
  Just (_, True) -> do
    v <- lift $ eval env e
    let env' = M.insert var (v, True) env
    exec env' rest
  Just (_, False) -> error "error: cannot reassign to immutable variable"
  Nothing -> error $ "error: undefined variable " <> show var

topLevel :: [SmnStmt] -> Environment
topLevel = foldl execTopLevelStmt initialEnv
 where
  execTopLevelStmt env (SSFunc name args body) =
    M.insert name (SVFunction args body, False) env
  execTopLevelStmt env _ =
    error "error: only function declarations are allowed in top level"

runProgram :: [SmnStmt] -> IO ()
runProgram stmts =
  let env = topLevel stmts in void $ eval env (SECall (SEVar "main") [])
