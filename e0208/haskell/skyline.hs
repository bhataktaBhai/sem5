import Data.List (sort)
data BST a = Null | Leaf a | Branch { value :: a
                                    , left :: BST a
                                    , right :: BST a
                                    } deriving (Show)

rightMost :: BST a -> a
rightMost (Leaf x) = x
rightMost (Branch x _ r) = rightMost r

full :: BST a -> Bool
full Null = True
full (Leaf _) = True
full b = full (right b)

create :: Ord a => [a] -> BST a
create xs = push Null (sort xs)

push :: Ord a => BST a -> [a] -> BST a
push Null (x:xs) = push (Leaf x) xs
push (Leaf y) (x:xs) = push (Branch y (Leaf y) (Leaf x)) xs
push (Branch y l r) (x:xs)
  | full r = push (Branch (rightMost r) (Branch y l r) (Leaf x)) xs
