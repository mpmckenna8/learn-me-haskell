-- my first haskell script i think this is how one comments in these.

doubleMe x = x + x 

-- very simple function above it can also have a function header describing things with types and stuff but it doesn't need to

-- using our function from above in our function below
doubleUs x y = doubleMe x + doubleMe y


-- example using haskell if else, in these the else is necessary
doubleSmallNumber x = if x > 100
                        then x
                        else x*2

sumto n = sum[1..n]

-- want to know what in the world the | are and how the arrow things work
qsort[] = []
qsort(x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [a | a <- xs, a <=x]
                    larger = [b | b <- xs, b > x]


