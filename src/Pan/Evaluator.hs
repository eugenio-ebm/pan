module Pan.Evaluator (eval) where

import Control.Monad (zipWithM)
import Control.Monad.Fix (mfix)
import Control.Monad.Reader (ReaderT, runReaderT, ask, local)

import Pan.Expr (Expr(..), Op(..), Pat(..))

type Env = [(String, Val)]

data Val
  = VClo String Expr Env
  | VCons String [Val]
  | VInt Int
  deriving (Eq, Show)

eval :: Expr -> IO Val
eval ts = runReaderT (eval' ts) []
  where
    eval' :: Expr -> ReaderT Env IO Val
    eval' (Let lhs rhs body) = do
      rhs' <- eval' rhs
      local ((lhs, rhs') :) (eval' body)
    eval' (Letrec lhs rhs body) = do
      rhs' <- mfix (\rhs' -> local ((lhs, rhs') :) (eval' rhs))
      local ((lhs, rhs') :) (eval' body)
    eval' (Lam param body) = VClo <$> pure param <*> pure body <*> ask
    eval' (Case e pats) = do
      e' <- eval' e
      evalClauses e' pats
    eval' (App f arg) = do
      arg' <- eval' arg
      f' <- eval' f
      case f' of
        VClo param body env ->
          local (const $ (param, arg') : env) (eval' body)
        _ -> error "Type error"
    eval' (BinOp op lhs rhs) = do
      lhs' <- eval' lhs
      rhs' <- eval' rhs
      case (op, lhs', rhs') of
        (Add, VInt l, VInt r) -> return $ VInt (l + r)
        (Sub, VInt l, VInt r) -> return $ VInt (l - r)
        _ -> error "Type error"
    eval' (Cons cons args) = do
      args' <- mapM eval' args
      return (VCons cons args')
    eval' (Var x) = do
      env <- ask
      case lookup x env of
        Just v -> return v
        Nothing -> error $ "Unbound variable '" ++ x ++ "'"
    eval' (Int v) = return (VInt v)

    evalClauses :: Val -> [(Pat, Expr)] -> ReaderT Env IO Val
    evalClauses v = go
      where
        go :: [(Pat, Expr)] -> ReaderT Env IO Val
        go [] = error "Exhausted case-expression"
        go ((pat, expr) : pats') =
          case matchPat pat v of
            Just env' -> local (env' ++) (eval' expr)
            Nothing -> go pats'

matchPat :: Pat -> Val -> Maybe Env
matchPat (PCons cons args) (VCons cons' args')
  | cons == cons'
  , length args == length args' = do
      envs <- zipWithM matchPat args args'
      return (concat envs)
matchPat (PVar x) v = Just [(x, v)]
matchPat (PInt x') (VInt x) | x == x' = Just []
matchPat _ _ = Nothing
