-- chap 5 notes and exercises
import Data.Char
import GHC.Prim 
import Data.Int


-- lots of cool stuff with list comprehensions

mycon :: [[a]] -> [a]
mycon xss = [ x | xs <- xss, x <- xs]

-- finds the length of the given list
mylen :: [a] -> Int
mylen xs = sum [1| _ <- xs]

-- grabs the first element of the tuple from each array element
firsts :: [(a,b)] -> [a]
firsts ps = [x | (x, _) <- ps]


factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0]

-- then we know that a prime number will have [1, itself] as the res so 
prime :: Int -> Bool
prime n = factors n == [1,n]

-- or a list of all primes to a given limit
primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]



-- zip is a library function that makes a new list combining elements into
-- tuples which share the same index.
-- zip :: [a] -> [b] -> [(a, b)]




-- isLower    c = iswlower (ord c) /= 0
-- lowers takes a string and returns an Int of how many lowercase characters
lowers :: String -> Int
lowers xs = length [ x | x <- xs, isLower x ]


-- count is a fuunstion which takes a Char and string and returns the number of
-- times the char appears in the String
count :: Char -> String -> Int
count a str = length [ x | x <- str, a == x]

mult x y z = x*y*z

let2int :: Char -> Int
let2int c = ord c - ord 'a'


int2let :: Int -> Char
int2let x = chr (ord 'a' + x)

-- can use toUpper and toLower to change it around.
shift :: Int -> Char -> Char
shift n c   | isLower c = int2let ((let2int c+n ) `mod` 26)
            | otherwise = if (isUpper c) then toUpper (shift n (toLower c))
                else c

positions  :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
                    where n = length xs -1




-- you can decode by simply putting in the sift number negative
encode      :: Int -> String -> String
encode n xs = [ shift n x | x <- xs ]

table :: [Float]
table = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]


percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100




freqs   :: String -> [Float]
freqs  xs = [ percent ( count x xs) n | x <- ['a'..'z']]
            where n = lowers xs


chisqr  :: [Float] -> [Float] -> Float
chisqr os es = sum [ ( (o-e)^2) / e | (o,e) <- zip os es ]


rotate  :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack   :: String -> String
crack xs = encode (-factor) xs
    where
        factor = head (positions ( minimum chitab ) chitab )
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs



-- ex 1
-- sum [x^2 | x <- [1..100]]


-- ex 2 make replicate using a list comprehension
myrep       :: Int  -> a -> [a]
myrep n x   = [ x | _ <- [1..n] ]



-- ex 3 pythagorean triple will produce identicals but in different orders
-- maybe could try more guards like x < y and a <y or something.
pyths   :: Int -> [(Int, Int, Int)]

pyths x     = [ (z, y, a) | z <-  [1..x-2], y <- [2..x-1], a <- [3..x] , z^2 + y^2 == a^2 ]


-- ex 4 All the perfect numbers before a given number
perfects    :: Int -> [Int]
perfects n = [ x | x <- [1..n] ,  x == sum (take ((length (factors x)) - 1) (factors x)) ]


-- ex 5 re express the thing as a single generator
-- want to ask if there's a better cleaner way to do this.
fungo [] []= []
fungo arr  arr2 = multi arr (length arr)
    where 
    multi [] i = []
    multi (x:xs) i = zip (replicate i x)  arr2 ++ multi xs i 


-- ex 6 redefine positions with find
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [ v | (k', v) <- t, k == k']



fpos    :: Eq a => a -> [a] -> [Int]

fpos v ar = find v ( zip ar [1..(length ar)] )



-- ex 7 Scalar product of 2 lists of integers

scalprod :: [Int] -> [Int] -> Int
scalprod a b = sum [x*y | (x,y) <- zipped ]
    where zipped = zip a b


-- ex 8 modifying above code so the Caesar Cipher works ok with capitals
