ghci> :l Lambda.hs 
[1 of 1] Compiling Lambda           ( Lambda.hs, interpreted )
Ok, modules loaded: Lambda.


-- 3(b) test --
ghci> let x = Var "X"
ghci> let y = Var "Y"
ghci> let z = Var "Z"
ghci> let w = Var "W"
ghci> let t1 = (Lam "t1" x)
ghci> t1
Lam "t1" (Var "X")
ghci> let t2 = (App y t1)
ghci> t2
App (Var "Y") (Lam "t1" (Var "X"))
ghci> subst x y z
Var "Z"
ghci> subst x y x
Var "Y"
ghci> subst y w x
Var "X"
ghci> subst y w (App x y)
App (Var "X") (Var "W")
ghci> subst x y (App (Lam "Z" x) w)
App (Lam "Z" (Var "Y")) (Var "W")
ghci> subst x (App (Lam "Z" z) w) (Lam "Y" x)
Lam "Y" (App (Lam "Z" (Var "Z")) (Var "W"))
ghci> subst x z (Lam "X" x)
Lam "X" (Var "X")
ghci> subst x (App t1 t2) (App (Lam "X" x) y)
App (Lam "X" (Var "X")) (Var "Y")
ghci> subst x (App t1 t2) (App (Lam "Y" x) y)
App (Lam "Y" (App (Lam "t1" (Var "X")) (App (Var "Y") (Lam "t1" (Var "X"))))) (Var "Y")
ghci> subst x z (Lam "Z" x)
Lam "Z" (Var "Z")


-- 3(c) test --
ghci> let x = Var "X"
ghci> let y = Lam "X" x
ghci> y
Lam "X" (Var "X")
ghci> let z = App y x
ghci> z
App (Lam "X" (Var "X")) (Var "X")
ghci> isValue x
False
ghci> isValue y
True
ghci> isValue z
False


-- 3(d) test --
ghci> let t1 = Var "t1"
ghci> let t2 = App (Var "a") (Var "b")
ghci> let v1 = Lam "X" (Var "1")
ghci> let v2 = Lam "X" (Var "2")
ghci> let t12 = App v1 v2
ghci> t12
App (Lam "X" (Var "1")) (Lam "X" (Var "2"))
ghci> eval1 t1
Nothing
ghci> eval1 t2
Nothing
ghci> eval1 v1
Nothing
ghci> eval1 v2
Nothing
ghci> eval1 t12
Just (Var "1")
ghci> eval1 (App t1 t2)
Nothing
ghci> eval1 (App v1 t2)
Nothing
ghci> eval1 (App (Lam "X" t12) v2)
Just (App (Lam "X" (Var "1")) (Lam "X" (Var "2")))


-- 3(e) test --
ghci> let x = Var "X"
ghci> let y = Var "Y"
ghci> let z = Var "Z"
ghci> let q = Var "Q"
ghci> let t1 = Lam "X" (Lam "Y" (App x y))
ghci> t1
Lam "X" (Lam "Y" (App (Var "X") (Var "Y")))
ghci> let t2 = App (Lam "X" (Var "1")) (Lam "X" (Var "2"))
ghci> t2
App (Lam "X" (Var "1")) (Lam "X" (Var "2"))
ghci> let t3 = App (Lam "X" (App (Lam "X" (Var "1")) (Lam "X" (Var "2")))) (Lam "X" (Var "2"))
ghci> t3
App (Lam "X" (App (Lam "X" (Var "1")) (Lam "X" (Var "2")))) (Lam "X" (Var "2"))
ghci> eval x
Var "X"
ghci> eval y
Var "Y"
ghci> eval z
Var "Z"
ghci> eval q
Var "Q"
ghci> eval t1
Lam "X" (Lam "Y" (App (Var "X") (Var "Y")))
ghci> eval t2
Var "1"
ghci> eval t3
Var "1"


-- 3(f) test --
-- x y z q are inherited from 3(e)
ghci> let f = App (Lam "X" (App x (Lam "Q" (App q q)))) (Lam "Y" y)
ghci> f
App (Lam "X" (App (Var "X") (Lam "Q" (App (Var "Q") (Var "Q"))))) (Lam "Y" (Var "Y"))
ghci> eval f
Lam "Q" (App (Var "Q") (Var "Q"))
