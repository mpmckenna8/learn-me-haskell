add :: Int -> ( Int -> Int )
add x y = x + y

nohead x y = x + y


-- ex 1
-- ([False, True], ['0', '1']) :: ([Bool], [Char])
-- ['a', 'b', 'c'] :: [Char]
-- weiredest one to me
-- [tail, init, reverse] :: [[a] -> [a]]


-- ex 2 I should just have the type definitions above the function instead of
-- commented out
second xs = head (tail xs)

-- second :: [a] -> a

swap (x,y) = (y, x)
-- swap :: (t1, t) -> (t, t1)

pair x y = (x, y)
--  pair :: t -> t1 -> (t, t1)
twice f x = f (f x)
-- twice :: (t -> t) -> t -> t

double :: Num a => a -> a
double x = x*2
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

-- ex 3 see definitions for above

{- ex 4
Functions can only be Eq class things if you can iterate through them maybe? And
the whole functional language thing is why you generally can't compare functions
except that the same imputs give you the same outputs??
-}

