module Pan.Type (Type(..), sub, getEVars) where

import Data.List (nub)

data Type
  = Forall String Type
  | Func Type Type
  | Cons String [Type]
  | Var String
  | EVar String
  deriving (Eq, Show)


-- capture-avoiding substitution
sub :: Type -> Type -> Type -> Type
sub old new = go
  where
    go :: Type -> Type
    go (Forall x t)
      | Var x' <- old
      , x == x' = Forall x t
      | otherwise = Forall x (go t)
    go (Func lhs rhs) = Func (go lhs) (go rhs)
    go (Cons cons args) = Cons cons (map go args)
    go t
      | t == old = new
      | otherwise = t

getEVars :: Type -> [Type]
getEVars (Forall _ t) = getEVars t
getEVars (Func lhs rhs) = nub $ getEVars lhs ++ getEVars rhs
getEVars (Cons _ args) = nub $ concatMap getEVars args
getEVars t
  | EVar _ <- t = [t]
  | otherwise = []
