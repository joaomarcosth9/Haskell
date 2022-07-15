fat n = if n == 0 then 1 else n * fat (n-1)

fat'' n | n == 0 = 1
        | n > 0 = n * fat'' (n-1)
