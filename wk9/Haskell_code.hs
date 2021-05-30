
--Laziness

loop x = loop x;

zero x = 0;

cbn_test = zero (loop 9)

list = [loop 0, loop 2, 3]
x::y::z::zs = list

z
x
-- Write your own control primitives
cond :: Bool -> a -> a -> a
cond True  t e = t
cond False t e = e

result_cond = cond True 10 (loop 9)

-- short-circuiting or
sc :: Bool -> Bool -> Bool
sc True   x = True
sc False  x = x

result = sc False (loop 9)

-- Better overload than in ML

f :: Eq a => a -> a -> Bool
f x y = x == y ;

-- class Eq a where 
--  (==)                  :: a -> a -> Bool

-- instance Eq Integer where 
--   x == y                =  x `integerEq` y

-- instance Eq Float where
--   x == y                =  x `floatEq` y

-- Comparing trees
---

data Tree a  = Leaf a | Node (Tree a) (Tree a)

instance (Eq a) => (Eq (Tree a)) where
 (Leaf x)     == (Leaf y)     =  (x == y)
 (Node t1 t2) == (Node t3 t4) =  (t1 == t3) && (t2 == t4)

tree1 = Leaf 2
tree2 = Leaf 3
tree3 = Node tree1 tree2 

res_tree_1_2 = f tree1 tree2
res_tree_3_3 = f tree3 tree3

--- 

g :: Ord a => a -> a -> Bool
g x y = x < y ;


-- class  (Eq a) => Ord a  where
--   (<), (<=), (>=), (>)  :: a -> a -> Bool
--   max, min              :: a -> a -> a

---
-- fun insert (x,[]) = [x]
-- |   insert (x, y :: ys ) = if x < y then x :: y :: ys else
--                             y :: (insert (x, ys));
-- val insert = fn : int * int list -> int list
-- fun sort [] = []
-- ...........
-- sort: int list -> int list

--  (op <):  int * int -> bool

insert :: Ord a => (a, [a]) -> [a]
insert (x,[]) = [x]
insert (x, y:ys) = if x < y then x : y : ys
                          else y : (insert (x, ys))

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert (x,  (sort xs))

sort_n = sort [4,9,1]
sort_b = sort [False, True]
sort_s = sort ["hi","bye","ciao"]

sort_fn=sort [\x->x]  


-- Cuts down the search space


--data Tree a  = Leaf a | Node (Tree a) (Tree a)

fringe (Leaf a) = [a]
fringe (Node x y) = (fringe x) ++ (fringe y)


samefringe t1 t2 = (fringe t1) == (fringe t2)
samefringe :: Eq t => Tree t -> Tree t -> Bool

test1 = Node (Node (Leaf 1) (Leaf 2))  (Leaf  3 )
test2 = Node (Leaf 1)  (Node (Leaf 2) (Leaf 3))
test3 = Node (Leaf 4)  (Node (Leaf 2) (Leaf 3))
test4 = Node (Leaf 1)  (Node (Leaf 2) (Leaf 3))

res1 = samefringe test1 test2
res2 = samefringe test1 test3
res3 = samefringe test4 test3



--- Streams


genint :: Integer -> [Integer]
genint n = n : (genint (n+1))

third = let
          x : y : z : xs = genint 0
        in z

merge [] ys = ys
merge xs [] = xs  
merge (x:xs) (y:ys) = x : y : (merge xs ys)

ones = 1 : ones

alts0 = 0 : alts1
alts1 = 1 : alts0
a = [ x^3  | x <- [0..]]   --  [0,1,8,27,64,125,216,343,512,729,
b = [ 3^x | x <- [0..]]    --  [1,3,9,27,81,243,729,2187,6561,1
double = [ x^2 | x<- [0..]] -- [0,1,4,9,16,25,36,49,64,81,100
c = merge [0..] double      -- [0,0,1,1,2,4,3,9,4,16,5,25,6,36,7,49
d = [ -x | x <-[1..]]

fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)


add :: [Integer] -> [Integer] ->  [Integer]
add (x : xs) ( y:ys) = (x+y) : (add xs ys)

fibonacci = 1 : 1 :  add fibonacci (tail fibonacci)

-- 1 1 2 3 5 8
-- 1 2 3 5 8

take1 1 (x:xs) = x
take1 n (x:xs) = take1 (n-1) xs 


take2 1 (x:xs) = [x]
take2 n (x:xs) = x : (take2 (n-1) xs)

-- Typical paradigm
-- generate all solutions (an enormous tree)
-- walk the tree to find the solution you want


primes :: [Integer] -> [Integer]
primes (n:ns) = n : primes (filter (\v -> v `mod` n /= 0) ns)


res = primes (genint 2)




