-- notes for chap 6 of Programming in Haskell

-- supposedly the n+1 pattern for recursioning is deprecated

sifac :: Int -> Int
sifac n = product [1..n]

-- in the plus 1 deprecated style
-- dont work
-- myfac :: Int -> Int
-- myfac 0 = 1
-- myfac (n+1) = (n+1) * myfac n


-- recursion on lists is super helplful
mylen   :: [a] -> Int
mylen [] = 0;
mylen (_:xs) = 1 + mylen xs

myrev   :: [a] -> [a]
myrev [] = []
myrev (x:xs) = myrev xs ++ [x]



-- insert thing into an already sorted list in a sorted way
insert  :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)  | x <= y = x:y:ys
                | otherwise = y : insert x ys


-- we can now define an insertion sort using insert
isort   ::  Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)


-- see quick sort in fist chapter notes for example of mult recur


-- ex 3
-- and logical thing for lists
mand     :: [Bool] -> Bool
mand [] =        False
mand [True] =    True
mand (x:xs) =   if(x == False)
                    then False
                 else
                    mand xs


-- concats lists in a list
myconcat     ::  [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) =  x ++ ( myconcat xs)

-- replicat creates a list of n of the same elements
myrep       :: a -> Int -> [a]
myrep   _ 0 = []
myrep  x n  = [x] ++ ( myrep x (n-1) )

-- my select thing (!!)
mys     :: [a] -> Int -> a
mys (x:xs) n = if n == 0
                then x
               else 
                mys xs (n-1)

-- my elem says if a elem is in a list
myel    :: Eq a => a -> [a] -> Bool
myel d [] = False
myel d (x:xs) = if d == x
                  then True
                else
                  myel d xs



-- ex 4 a function which merges 2 sorted lists
merge       :: Ord a => [a] -> [a] -> [a]

merge ar [] = ar
merge [] br = br
merge (y:ys) (x:xs) = if y < x
                        then merge (y:[ q | q <- ys, x > q ]++[x]++[ o | o <- ys, o > x ]) xs
                      else
                        merge (x:y:ys)  xs



-- ex 5 mergesort using halve
-- splits a list in two with length differing by at most 1
halve   :: [a] -> ([a],[a])

halve [] = ([],[])
halve [x] = ([x],[])
halve [x,y] = ([x],[y])

halve x = splitAt (floor ((fromIntegral (length x))/2)) x


-- msort splits a list into singletons then merges them all.

msort       :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]

msort a = if donesplit splited
            then appmerge splited
            else
            keepsplit splited
    where 
        splited = halve a
        appmerge (x,y) = merge x y
donesplit (x,y) = if (length x) > 1 || (length y) > 1
                        then False
                        else True


keepsplit (x,y) = merge (msort x) (msort y)



-- ex 6 

-- takes a number of elements from the start of a list

mtake    :: Int  -> [a] -> [a]

mtake _ [] = []
mtake 0 a = []
mtake n (x:xs)   = x:(mtake (n-1) xs)
