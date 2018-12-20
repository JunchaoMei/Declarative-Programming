-- 2.1 (a) --
isEven :: Int -> Bool
isEven n = n`mod`2==0

filterEven :: [Int] -> [Int]
filterEven arr = filter isEven arr

f :: [[Int]] -> [[Int]]
f arrarr = map filterEven arrarr


-- 2.1 (b) --
my_append :: [a] -> [a] -> [a]
my_append xs ys = foldr (:) ys xs


-- 2.2 (a) --
data MTree a = MLeaf (Maybe a)
             | MNode (Maybe a) (MTree a) (MTree a) deriving (Show)

--regular tree--
data Tree a = Leaf a
            | Node (Tree a) (Tree a) deriving (Show)

treeMap :: (a -> b) -> (Tree a) -> (Tree b)
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Node t1 t2) = Node (treeMap f t1) (treeMap f t2)


-- 2.2 (b) --
maybeFunctn :: (a -> b) -> (Maybe a) -> (Maybe b)
maybeFunctn f Nothing = Nothing
maybeFunctn f (Just x) = Just (f x)

mTreeMap :: (a -> b) -> (MTree a) -> (MTree b)
mTreeMap f (MLeaf x) = MLeaf (maybeFunctn f x)
mTreeMap f (MNode x t1 t2) = MNode (maybeFunctn f x) (mTreeMap f t1) (mTreeMap f t2)


-- 2.2 (c) --
maybePred :: (a -> Bool) -> (Maybe a) -> (Maybe a)
maybePred p Nothing = Nothing
maybePred p (Just x) = if p x then (Just x) else Nothing

mTreeFilter :: (a -> Bool) -> MTree a -> MTree a
mTreeFilter p (MLeaf x) = MLeaf (maybePred p x)
mTreeFilter p (MNode x t1 t2) = MNode (maybePred p x) (mTreeFilter p t1) (mTreeFilter p t2)

