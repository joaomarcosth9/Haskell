zero = \s z -> z
um = \s z -> s z
dois = \s z -> s (s z)
tres = \s z -> s (s (s z))
quatro = \s z -> s (s (s (s z)))


suc = \w y x -> y (w y x)
prede = \n f x -> n (\g h -> h (g f)) (\u -> x) (\u -> u)
add = \x y w u -> x w (y w u)
mul = \x y w u -> x (y w) u

v = \v f -> v
f = \v f -> f

e = \x y -> x y (\u v -> v)
ou = \x y -> x (\u v -> u) y
nao = \x -> x (\ u v -> v) (\a b -> a)

ehZero = \n -> n (\d -> f) v

teste = \n -> (ehZero n) um dois

par x = if rem x 2 == 0 then True else False
