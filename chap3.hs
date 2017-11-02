add :: Int -> ( Int -> Int )
add x y = x + y

nohead x y = x + y

-- ex 1
-- ([False, True], ['0', '1']) :: ([Bool], [Char])
-- ['a', 'b', 'c'] :: [Char]
-- weiredest one to me
-- [tail, init, reverse] :: [[a] -> [a]]


-- ex 2
second xs = head (tail xs)
-- second :: [a] -> a
swap (x,y) = (y, x)

