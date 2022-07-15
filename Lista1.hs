-- QUESTAO 1
-- recebe a, b e c e verifica se eh um triangulo 
maior a b c | a >= b && a >= c = a 
            | b >= a && b >= c = b 
            | c >= b && c >= a = c 

soma_menores a b c = a+b+c - maior a b c

ehTriangulo a b c | maior a b c >= soma_menores a b c = False
                  | otherwise = True

-- QUESTAO 2
-- recebe a, b e c e verifica o tipo do triangulo

tipoTriangulo a b c | (a == b && a==c) = "equilatero"
                    | (a /= b && a /= c && b/=c) = "escaleno"
                    | (a == b && a /= c) || (a == c && a /= b) || (c == b && c /= a) = "isosceles"

-- QUESTAO 3
-- recebe a, b e c e verifica se eh um triangulo e diz seu tipo 

triangulo a b c | ehTriangulo a b c && (a == b && a==c) = "equilatero"
                | ehTriangulo a b c && (a /= b && a /= c && b/=c) = "escaleno"
                | ehTriangulo a b c && ((a == b && a /= c) || (a == c && a /= b) || (c == b && c /= a)) = "isosceles"
                | otherwise = "nao eh um triangulo"

-- QUESTAO 4
-- recebe n e soma os valores pares entre 0 e n

somaPares n | n == 0 = 0
            | rem n 2 == 0 = n + somaPares (n-2) 
            | rem n 2 == 1 = somaPares (n-1) 

somaPares' n | n == 0 = 0
             | rem n 2 == 0 = n + somaPares (n-2) 
             | rem n 2 == 1 = (n-1) + somaPares (n-3) 

-- QUESTAO 5
-- recebe m e n e retorna somatorio de 2^i*m com i indo de 0 a n 

somaPot2m m n | n == 0 = m
              | otherwise = (2^n)*m + somaPot2m m (n-1)

-- QUESTAO 6
-- Verifica se eh primo

divisor n m | rem n m == 0 = True
            | otherwise = False

primo' n m | n == m = True
           | divisor n m == True = False
           | divisor n m == False = primo' n (m+1)

primo n = primo' n 2


-- QUESTAO 7
-- Aproximacao de pi

seriePI' n m | m >= n = 0
             | rem (round(m)+1) 4 == 2 = 4/m + seriePI' n (m+2)
             | rem (round(m)+1) 4 == 0 = -4/m + seriePI' n (m+2)

seriePI n  = seriePI' n 1
