-- chap2 notes and functions

double x = x+ x
quadruple x = double(double x)

factorial n = product[1..n]
average bs = sum bs `div` length bs

no = a `div` length xs
          where 
            a = 10
            xs = [1,2,3,4,5]


-- ex 4
mylast x = x !! ((length x) - 1);

myinit a = take ((length a)-1) a



