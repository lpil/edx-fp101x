double    x = x + x
quadruple x = double $ double x

factorial n = product [1..n]
average  ns = sum ns `div` length ns -- backticks make a function infix

n = a `div` length xs
  where a = 10
        xs = [1,2,3,4,5]

prod [] = 1
prod (x: xs) = x * prod xs

qsort [] = []
qsort (x : xs) = 
    reverse (reverse (qsort larger) ++ [x] ++ reverse (qsort smaller))
  where smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]

xs = [6,9,1,8,4,2,6,0,8,10,3]
