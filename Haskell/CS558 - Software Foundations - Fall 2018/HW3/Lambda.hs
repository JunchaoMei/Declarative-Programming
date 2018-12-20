module Lambda where


type Name = String

data Term
  = Var Name                           --variable
  | Lam Name Term                      --abstraction
  | App Term Term deriving (Eq, Show)  --application
 
subst :: Term -> Term -> Term -> Term
subst (Var x) t (Var y) = if y==x then t else (Var y)
subst (Var x) t (App t1 t2) = App (subst (Var x) t t1) (subst (Var x) t t2)
subst (Var x) t (Lam y tp) = if y==x then (Lam x tp) else (Lam y (subst (Var x) t tp))

isValue :: Term -> Bool
isValue (Var x) = False
isValue (Lam x t) = True
isValue (App t1 t2) = False

eval1 :: Term -> Maybe Term
eval1 (App (Lam x t12) v2) | isValue v2 = Just (subst (Var x) v2 t12)
eval1 (App v1 t2) | isValue v1 = case eval1 t2 of Just t2p -> Just (App v1 t2p)
                                                  Nothing -> Nothing
eval1 (App t1 t2) = case eval1 t1 of Just t1p -> Just (App t1p t2)
                                     Nothing -> Nothing
eval1 _ = Nothing

eval :: Term -> Term
eval t = case eval1 t of Just tp -> eval tp
                         Nothing -> t
