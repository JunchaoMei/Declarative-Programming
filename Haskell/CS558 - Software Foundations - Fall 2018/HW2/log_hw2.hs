ghci> :l funs_hw2.hs 
[1 of 1] Compiling Main             ( funs_hw2.hs, interpreted )
Ok, modules loaded: Main.


-- 2.1 (a) --
isEven :: Int -> Bool
isEven n = n`mod`2==0

filterEven :: [Int] -> [Int]
filterEven arr = filter isEven arr

f :: [[Int]] -> [[Int]]
f arrarr = map filterEven arrarr

--test
ghci> f [[1,2,3,4],[5,6],[7]]
[[2,4],[6],[]]
ghci> f [[8,13,27,30],[],[43242,56867]]
[[8,30],[],[43242]]
ghci> f [[159,172,666],[147],[-358,-897],[0]]
[[172,666],[],[-358],[0]]


-- 2.1 (b) --
my_append :: [a] -> [a] -> [a]
my_append xs ys = foldr (:) ys xs

--test
ghci> my_append [1,2,3] [4,5]
[1,2,3,4,5]
ghci> my_append [1,2,3] []
[1,2,3]
ghci> my_append [] [4,5]
[4,5]
ghci> my_append [] []
[]
ghci> my_append [47,29,4567] [18,72]
[47,29,4567,18,72]
ghci> my_append [-1,-2,-3] [0,5]
[-1,-2,-3,0,5]


-- 2.2 (a) --
data MTree a = MLeaf (Maybe a)
             | MNode (Maybe a) (MTree a) (MTree a) deriving (Show)

{-
This polymorphic Haskell datatype, MTree, represents the data structure of a tree with element datatype a. The leaf of this tree may contain a possible element with datatype a (or nothing), while the node of this tree contain two sub-Mtrees and a possible element with datatype a.
-}

--regular tree--
data Tree a = Leaf a
            | Node (Tree a) (Tree a) deriving (Show)

treeMap :: (a -> b) -> (Tree a) -> (Tree b)
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Node t1 t2) = Node (treeMap f t1) (treeMap f t2)

--test
ghci> let t = Node (Leaf 1) (Node (Leaf 2) (Leaf 3)) :: Tree Int
ghci> treeMap (\n -> n+1) t
Node (Leaf 2) (Node (Leaf 3) (Leaf 4))
ghci> let t = Node (Node (Leaf 1.1) (Leaf 2.2)) (Node (Leaf 3.3) (Leaf 4.4)) :: Tree Double
ghci> treeMap (\n -> n+1) t
Node (Node (Leaf 2.1) (Leaf 3.2)) (Node (Leaf 4.3) (Leaf 5.4))


-- 2.2 (b) --
maybeFunctn :: (a -> b) -> (Maybe a) -> (Maybe b)
maybeFunctn f Nothing = Nothing
maybeFunctn f (Just x) = Just (f x)

mTreeMap :: (a -> b) -> (MTree a) -> (MTree b)
mTreeMap f (MLeaf x) = MLeaf (maybeFunctn f x)
mTreeMap f (MNode x t1 t2) = MNode (maybeFunctn f x) (mTreeMap f t1) (mTreeMap f t2)

--test
ghci> maybeFunctn (\n -> n+1) (Just 10)
Just 11
ghci> maybeFunctn (\n -> n+1) Nothing
Nothing
ghci> let t = MNode (Just 11) (MLeaf (Just 21)) (MLeaf (Just 22)) :: MTree Int
ghci> mTreeMap (\n -> n+1) t
MNode (Just 12) (MLeaf (Just 22)) (MLeaf (Just 23))
ghci> let t = MNode (Just 1.1) (MLeaf (Just 2.1)) (MNode Nothing (MLeaf (Just 3.1)) (MLeaf Nothing)) :: MTree Double
ghci> mTreeMap (\n -> n+1) t
MNode (Just 2.1) (MLeaf (Just 3.1)) (MNode Nothing (MLeaf (Just 4.1)) (MLeaf Nothing))


-- 2.2 (c) --
maybePred :: (a -> Bool) -> (Maybe a) -> (Maybe a)
maybePred p Nothing = Nothing
maybePred p (Just x) = if p x then (Just x) else Nothing

mTreeFilter :: (a -> Bool) -> MTree a -> MTree a
mTreeFilter p (MLeaf x) = MLeaf (maybePred p x)
mTreeFilter p (MNode x t1 t2) = MNode (maybePred p x) (mTreeFilter p t1) (mTreeFilter p t2)

--test
ghci> maybePred isEven Nothing
Nothing
ghci> maybePred isEven (Just 11)
Nothing
ghci> maybePred isEven (Just 12)
Just 12
ghci> let t = MNode (Just 10) (MLeaf (Just 21)) (MLeaf (Just 22)) :: MTree Int
ghci> mTreeFilter isEven t
MNode (Just 10) (MLeaf Nothing) (MLeaf (Just 22))
ghci> let t = MNode (Just 11) (MLeaf (Just 22)) (MNode (Just 30) (MLeaf (Just 31)) (MLeaf Nothing)) :: MTree Int
ghci> mTreeFilter isEven t
MNode Nothing (MLeaf (Just 22)) (MNode (Just 30) (MLeaf Nothing) (MLeaf Nothing))


-- 2.3 --
{-

Prove: For any functions f & g, any list l, and any x within the definition domains of f and g, given f (g x) = g (f x), prove the property map f (map g l) = map g (map f l) holds.

<A> Definition of map
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = (f x):(map f xs)
<B> Inductive Hypothesis (I.H.)
f (g x) = g (f x)
<C> Assumption
length xs == 1
<D> Left Hand Side (LHS)
map f (map g l)
<E> Right Hand Side (RHS)
map g (map f l)

(1) Base Case: l=[]
     LHS = map f (map g [])
(by <A>) = map f []
(by <A>) = []
     RHS = map g (map f [])
(by <A>) = map g []
(by <A>) = []
  => LHS = RHS

(2) Inductive Case: l=x:xs
     LHS = map f (map g (x:xs))
(by <A>) = map f ((g x):(map g xs))
(by <A>) = (f (g x)):(map f (map g xs))
(by <C>) = (f (g x)):(map f ((g xs):[]))
         = (f (g x)):(map (f (g xs)))

     RHS = map g (map f (x:xs))
(by <A>) = map g ((f x):(map f xs))
(by <A>) = (g (f x)):(map g (map f xs))
(by <C>) = (g (f x)):(map g ((f xs):[]))
         = (g (f x)):(map (g (f xs)))
(by <B>) = (f (g x)):(map (f (g xs)))

  => LHS = RHS

(1) & (2) => map f (map g l) = map g (map f l)

-}
