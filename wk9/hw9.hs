code = 1 : code

intList n = n : intList (n + 1)

intList' n = [n..]

takeN n (x:xs) = if n == 1 then [x] else x : takeN (n - 1) xs

evens :: [Int]
-- evens n = if n % 2 == 0 then n : evens(n + 2) else []

nats = intList 0
evens = filter (\x -> x `mod` 2 == 0) nats 

odds :: [Int]
odds = filter (\x -> x `mod` 2 == 1) nats 

merge (x:xs) (y:ys) = x : y : merge xs ys

-- 0,1,8,27,64,125,216,343,512,729,1000,1331,...

cubes = [ n^3 | n <- nats]
cubes' = map (^3) nats

-- 1,3,9,27,81,243,729,2187,6561,19683,59049,...

threePowerOf = [ 3^n | n <- nats]
threePowerOf' = map (3^) nats

-- 0,0,1,1,2,4,3,9,4,16,5,25,6,36,7,49,...

squares = map (^2) nats

mergedNatsSquares = merge nats squares

-- The negative numbers
pos = intList 1

negs = map (* (-1)) pos
negs' = [ -n | n <- pos]

main = do
	print (takeN 3 (intList' 22))
	print (takeN 3 evens)
	print (takeN 3 odds)
	print (takeN 5 (merge evens odds))
	print (head (merge evens odds))
	print (takeN 10 cubes)
	print (takeN 10 threePowerOf)
	print (takeN 20 mergedNatsSquares)
	print (takeN 3 negs)
	print (takeN 3 negs')




