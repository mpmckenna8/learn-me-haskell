{-
Defining functions chapter
-}

--isDigit :: Char -> Bool
-- doesn't work!!!
isDigit c = c>='0' && c<='9'


even1 :: Integral a => a -> Bool
even1 n = n `mod` 2 == 0

splitAt1 :: Int -> [a] -> ([a],[a])
splitAt1 n xs = (take n xs, drop n xs)

-- for lamda functions you do a forward slash like:
-- (\ x-> x+2 ) 2
-- which will work in the console to add 2 to 2

--ex1
halve :: [a] -> ([a],[a])
halve a = splitAt ((length a) `div` 2) a


-- ex2
safetail :: [a] -> [a]
-- pattern matching 
--safetail [] = []
--safetail (x:xs) = xs
-- guarded
-- safetail x | (length x) == 0         = []
--           | otherwise               = tail x 
-- conditional
safetail x = if (length x) > 0
                then tail x;
             else []

-- ex 3 the logical disjunction operator or i tink OR type thing
mor  :: Bool ->  Bool  -> Bool
mor True _ = True
mor _ True = True
mor False False = False

-- Ex 4 Do the conjunction operator using conditional expressions
mand :: Bool -> Bool -> Bool
mand x y = if x == True
                then if y == True
                        then True
                        else False
                 else False
-- ex 5 different way to write it more concisely
dand :: Bool -> Bool -> Bool
dand x y = if x then y
            else False


-- ex 6 see notebook 11/3
mult :: Num a => a -> a -> a -> a
mult x y z = x * y * z
