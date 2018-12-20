module STLC where


type Name = String

type Env = [(Name, Type)]

data Type = TNat
          | TBool
          | TArr Type Type
          deriving (Eq, Read, Show)

data Term = Tru
          | Fls
          | Zero
          | Succ Term
          | Pred Term
          | IsZero Term
          | If Term Term Term
          | Var Name
          | Lam Name Type Term
          | App Term Term
          deriving (Eq, Show)

isNumericValue :: Term -> Bool
isNumericValue Zero = True
isNumericValue (Succ t) = isNumericValue t
isNumericValue _ = False

isValue :: Term -> Bool
isValue Tru = True
isValue Fls = True
isValue (Lam x ty t) = True
isValue t = isNumericValue t

subst :: Name -> Term -> Term -> Term
subst x t Tru = Tru
subst x t Fls = Fls
subst x t Zero = Zero
subst x t (Succ tp) = Succ (subst x t tp)
subst x t (Pred tp) = Pred (subst x t tp)
subst x t (IsZero tp) = IsZero (subst x t tp)
subst x t (If t1 t2 t3) = If (subst x t t1) (subst x t t2) (subst x t t3)
subst x t (App t1 t2) = App (subst x t t1) (subst x t t2)
subst x t (Var y) = if y==x then t else (Var y)
subst x t (Lam y ty tp) = if y==x then (Lam x ty tp) else (Lam y ty (subst x t tp))

eval1 :: Term -> Maybe Term
eval1 (If Tru t2 t3) = Just t2
eval1 (If Fls t2 t3) = Just t3
eval1 (Pred Zero) = Just Zero
eval1 (Pred (Succ nv)) | isNumericValue nv = Just nv
eval1 (IsZero Zero) = Just Tru
eval1 (IsZero (Succ nv)) | isNumericValue nv = Just Fls
eval1 (If t1 t2 t3) = case eval1 t1 of Just t1p -> Just (If t1p t2 t3)
                                       Nothing -> Nothing
eval1 (Succ t1) = case eval1 t1 of Just t1p -> Just (Succ t1p)
                                   Nothing -> Nothing
eval1 (Pred t1) = case eval1 t1 of Just t1p -> Just (Pred t1p)
                                   Nothing -> Nothing
eval1 (IsZero t1) = case eval1 t1 of Just t1p -> Just (IsZero t1p)
                                     Nothing -> Nothing
eval1 (App (Lam x ty11 t12) v2) | isValue v2 = Just (subst x v2 t12)
eval1 (App v1 t2) | isValue v1 = case eval1 t2 of Just t2p -> Just (App v1 t2p)
                                                  Nothing -> Nothing
eval1 (App t1 t2) = case eval1 t1 of Just t1p -> Just (App t1p t2)
                                     Nothing -> Nothing
eval1 _ = Nothing

eval :: Term -> Term
eval t = case eval1 t of Just tp -> eval tp
                         Nothing -> t

extend :: (Name,Type) -> Env -> Env
extend xt env = xt : env

lookupVar :: Name -> Env -> Maybe Type
lookupVar v [] = Nothing
lookupVar v (x:xs) = if v==(fst x) then Just (snd x) else (lookupVar v xs)

findingType :: Env -> Term -> Maybe Type
findingType env Tru = Just TBool
findingType env Fls = Just TBool
findingType env Zero = Just TNat
findingType env (Var x) = lookupVar x env
findingType env (Succ t) = case findingType env t of Just TNat -> Just TNat
                                                     _ -> Nothing
findingType env (Pred t) = case findingType env t of Just TNat -> Just TNat
                                                     _ -> Nothing
findingType env (IsZero t) = case findingType env t of Just TNat -> Just TBool
                                                       _ -> Nothing
findingType env (If t1 t2 t3) = case findingType env t1 of
                                    Just TBool -> case findingType env t2 of
                                        Just t2p -> case findingType env t3 of
                                            Just t3p -> if t2p==t3p then Just t2p else Nothing
                                            _ -> Nothing
                                        _ -> Nothing
                                    _ -> Nothing
findingType env (App t1 t2) = case findingType env t1 of
                                  Just (TArr ty11 ty12) -> case findingType env t2 of
                                      Just ty11 -> Just ty12
                                      _ -> Nothing
                                  _ -> Nothing
findingType env (Lam x ty1 t2) = case lookupVar x env of
                                     Nothing -> case findingType (extend (x,ty1) env) t2 of
                                         Just ty2 -> Just (TArr ty1 ty2)
                                         _ -> Nothing
                                     _ -> Nothing

findType :: Env -> Term -> Type
findType env t = case findingType env t of Just ty -> ty
                                           Nothing -> error "untypeable term"
