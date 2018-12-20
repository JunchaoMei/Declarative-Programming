ghci> :l STLC.hs
[1 of 1] Compiling STLC             ( STLC.hs, interpreted )
Ok, modules loaded: STLC.


---- 4(e) test ----
ghci> let envir0 = []
ghci> let envir1 = extend ("true",TBool) envir0
ghci> let envir2 = extend ("345",TNat) envir1
ghci> envir0
[]
ghci> envir1
[("true",TBool)]
ghci> envir2
[("345",TNat),("true",TBool)]
ghci> lookupVar "345" envir0
Nothing
ghci> lookupVar "345" envir1
Nothing
ghci> lookupVar "345" envir2
Just TNat
ghci> lookupVar "true" envir0
Nothing
ghci> lookupVar "true" envir1
Just TBool
ghci> lookupVar "true" envir2
Just TBool


--- terms used for tests ---
ghci> let x = Var "X"
ghci> let y = Var "Y"
ghci> let z = Var "Z"
ghci> let tru = Tru
ghci> let fls = Fls
ghci> let zero = Zero
ghci> let one = Succ zero
ghci> let two = Succ one
ghci> let t1 = IsZero (Pred two)
ghci> let t2 = If (IsZero (Pred (Succ (Pred zero)))) x y
ghci> let t3 = If t1 one zero
ghci> let t4 = Lam "X" TNat (If (IsZero x) one zero)
ghci> let t5 = App t4 two
ghci> let t6 = App t4 (Lam "Y" TBool t2)
ghci> let t7 = App t4 t2
ghci> let t8 = App t4 (Pred (Succ zero))
ghci> let tc = App (Lam "X" TNat (IsZero (Pred (Pred x)))) (Succ (Pred zero))
ghci> let tf1 = tc
ghci> let tf2 = App (Lam "X" TNat (If x zero one)) (Succ (Pred zero))


---- 4(c) & 4(f) test ----
ghci> x
Var "X"
ghci> eval x
Var "X"
ghci> findType [] x
*** Exception: untypeable term
ghci> y
Var "Y"
ghci> eval y
Var "Y"
ghci> findType [] y
*** Exception: untypeable term
ghci> z
Var "Z"
ghci> eval z
Var "Z"
ghci> findType [] z
*** Exception: untypeable term
ghci> tru
Tru
ghci> eval tru
Tru
ghci> findType [] tru
TBool
ghci> fls
Fls
ghci> eval fls
Fls
ghci> findType [] fls
TBool
ghci> zero
Zero
ghci> eval zero
Zero
ghci> findType [] zero
TNat
ghci> one
Succ Zero
ghci> eval one
Succ Zero
ghci> findType [] one
TNat
ghci> two
Succ (Succ Zero)
ghci> eval two
Succ (Succ Zero)
ghci> findType [] two
TNat
ghci> t1
IsZero (Pred (Succ (Succ Zero)))
ghci> eval t1
Fls
ghci> findType [] t1
TBool
ghci> t2
If (IsZero (Pred (Succ (Pred Zero)))) (Var "X") (Var "Y")
ghci> eval t2
Var "X"
ghci> findType [] t2
*** Exception: untypeable term
ghci> t3
If (IsZero (Pred (Succ (Succ Zero)))) (Succ Zero) Zero
ghci> eval t3
Zero
ghci> findType [] t3
TNat
ghci> t4
Lam "X" TNat (If (IsZero (Var "X")) (Succ Zero) Zero)
ghci> eval t4
Lam "X" TNat (If (IsZero (Var "X")) (Succ Zero) Zero)
ghci> findType [] t4
TArr TNat TNat
ghci> t5
App (Lam "X" TNat (If (IsZero (Var "X")) (Succ Zero) Zero)) (Succ (Succ Zero))
ghci> eval t5
Zero
ghci> findType [] t5
TNat
ghci> t6
App (Lam "X" TNat (If (IsZero (Var "X")) (Succ Zero) Zero)) (Lam "Y" TBool (If (IsZero (Pred (Succ (Pred Zero)))) (Var "X") (Var "Y")))
ghci> eval t6
If (IsZero (Lam "Y" TBool (If (IsZero (Pred (Succ (Pred Zero)))) (Var "X") (Var "Y")))) (Succ Zero) Zero
ghci> findType [] t6
*** Exception: untypeable term
ghci> t7
App (Lam "X" TNat (If (IsZero (Var "X")) (Succ Zero) Zero)) (If (IsZero (Pred (Succ (Pred Zero)))) (Var "X") (Var "Y"))
ghci> eval t7
App (Lam "X" TNat (If (IsZero (Var "X")) (Succ Zero) Zero)) (Var "X")
ghci> findType [] t7
*** Exception: untypeable term
ghci> t8
App (Lam "X" TNat (If (IsZero (Var "X")) (Succ Zero) Zero)) (Pred (Succ Zero))
ghci> eval t8
Succ Zero
ghci> findType [] t8
TNat
ghci> tc
App (Lam "X" TNat (IsZero (Pred (Pred (Var "X"))))) (Succ (Pred Zero))
ghci> eval tc
Tru
ghci> findType [] tc
TBool
ghci> tf1
App (Lam "X" TNat (IsZero (Pred (Pred (Var "X"))))) (Succ (Pred Zero))
ghci> eval tf1
Tru
ghci> findType [] tf1
TBool
ghci> tf2
App (Lam "X" TNat (If (Var "X") Zero (Succ Zero))) (Succ (Pred Zero))
ghci> eval tf2
If (Succ Zero) Zero (Succ Zero)
ghci> findType [] tf2
*** Exception: untypeable term
