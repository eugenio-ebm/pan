module Pan.Typechecker (typecheck) where

import Debug.Trace (traceShowWith)

import Control.Monad.Fix (mfix)
import Control.Monad.State (State, evalState, get, gets, put)

import qualified Data.Map as Map
import Data.Map (Map)

import Pan.Expr (Expr(..), Op(..), Pat(..))

import qualified Pan.Type as T
import Pan.Type (Type)

tInt :: Type
tInt = T.Var "Int"

type Typecheck = State (Int, Map String Type)

lookupEnv :: String -> Typecheck (Maybe Type)
lookupEnv x = do
  env <- gets snd
  return (Map.lookup x env)

fresh :: String -> Typecheck Type
fresh x = do
  (i, env) <- get
  let t = T.EVar $ "$t" ++ show i
  put (i + 1, Map.insert x t env)
  return t

freshEVar :: Typecheck Type
freshEVar = do
  (i, env) <- get
  let t = T.EVar $ "$t" ++ show i
  put (i + 1, env)
  return t

replaceEVar :: String -> Type -> Typecheck ()
replaceEVar x t = do
  (i, env) <- get
  let env' = Map.map (T.sub (T.EVar x) t) env
  put (i, env')

-- See "A mechanical formalization of higher-ranked polymorphic type inference"
-- DOI: 10.1145/3341716

typecheck :: Expr -> Type
typecheck e = evalState (infer e) (0, Map.empty)

-- env |- e <= t
check :: Expr -> Type -> Typecheck Type
check e t = do
  t' <- infer e
  case (t', t) of
    (_, T.EVar x) -> do
      replaceEVar x t'
      return t'
    (T.EVar x, _) -> do
      replaceEVar x t
      return t
    _ -> return t'

-- env |- e => t
infer :: Expr -> Typecheck Type
infer (Let lhs rhs body) = do
  tLhs <- fresh lhs
  tRhs <- check rhs tLhs
  case T.getEVars tRhs of
    [] -> infer body
    xs -> do
      let xs' = [ x | T.EVar x <- xs ]
          t = foldr (\x t' -> T.Forall x (T.sub (T.EVar x) (T.Var x) t')) tRhs xs'
      return t
infer (Letrec lhs rhs body) = do
  tLhs <- fresh lhs
  _ <- check rhs tLhs
  infer body
infer (Lam param body) = do
  _ <- fresh param
  tBody <- infer body
  t <- lookupEnv param
  case t of
     Just tParam' -> return (T.Func tParam' tBody)
infer (Case e pats) = do
  t <- infer e
  -- TODO: check (env |- pat <= t)
  let exprs = map snd pats
  ts <- mapM infer exprs
  case ts of
    [] -> error "empty case"
    t' : _ -> return t'
infer (App f arg) = do
  tF <- infer f
  inferApp tF arg
infer (BinOp op lhs rhs) = do
  let (lhsT, rhsT, retT) =
        case op of
          Add -> (tInt, tInt, tInt)
          Sub -> (tInt, tInt, tInt)
  _ <- check lhs lhsT
  _ <- check rhs rhsT
  -- TODO add warnings
  return retT
infer (Cons cons args) = do
  tArgs <- mapM infer args
  return (T.Cons cons tArgs)
infer (Var x) = do
  t <- lookupEnv x
  case t of
    Just t' -> return t'
    Nothing -> do
      t' <- fresh x
      return t'

infer (Int _) = return tInt


-- env |- (t . e) => t'
inferApp :: Type -> Expr -> Typecheck Type
inferApp (T.Forall x t) e = do
  tArg <- freshEVar
  let t' = T.sub (T.Var x) tArg t
  inferApp t' e
inferApp (T.Func tArg tBody) e = do
  tArg' <- check e tArg
  case tArg of
    T.EVar _ -> return (T.sub tArg tArg' tBody)
    _ -> return tBody
inferApp (T.EVar x) e = do
  tParam <- freshEVar
  tBody <- freshEVar
  let t = (T.Func tParam tBody)
  replaceEVar x t
  inferApp t e
