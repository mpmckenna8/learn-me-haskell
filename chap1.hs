-- ex1
doubleit x = x + x

-- sometimes infix and prefix can be mixed around

blah = div 4 2

-- doesn't work but auger says it should
-- blahis = 4 'div' 2

-- actually declaring types and what it returns
factorial0 :: Integer -> Integer

factorial0 n =
    if n == 1
    then 1
    else n * factorial0 (n-1)


-- factorial with cases



-- using guards is what i was all confused about in the silly qsort.

-- tail recursive definitions are good to look at

-- define tail recursive length rather than list recursive
-- define fibonacci


-- ex2
sum_identity x = sum[x]

-- ex3 product takes a list of numbers and multiplies them
producto [] = 1
producto (x:xs) =  x * producto xs

-- ex 4 reverse qsort
rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
                where
                    smaller = [a | a <- xs, a <= x ]
                    larger = [b | b <- xs, b > x ]


-- ex 5
-- removing the <= will get rid of any exact matches and only have unique values


-- ex 6 

