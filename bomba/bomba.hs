fib' 0 _ _ _ = [1]
fib' 1 _ _ _ = [1, 1] 
fib' n a b c | c == n = [b, a + b] 
             | otherwise = b:fib' n b (a+b) (c+1) 

fib n = fib' n 1 1 2





fibonacci 0 = []
fibonacci n = (fibonacci (n-1))++[fib n]

fibs = 1:1:zipWith (+) fibs (tail fibs)
