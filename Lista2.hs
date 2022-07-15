import Data.Char

tam [] = 0
tam (x:xs) = 1 + tam xs

-- 1) verifica se um elemento pertence a uma lista
pertence _ [] = False
pertence x (y:ys) | x == y = True
                  | otherwise = pertence x ys

-- 2) retorna a intersecção de duas listas
interseccao _ [] = []
interseccao [] _ = []
interseccao (x:xs) (ys) | pertence x ys = [x]++(interseccao xs ys)
                  | otherwise = []++(interseccao xs ys)

-- 3) retorna a lista reversa
inverso [] = []
inverso (x:xs) = (inverso xs) ++ [x]

-- 4) retorna os n ultimos
nPrimeiros _ [] = []
nPrimeiros 0 _ = []
nPrimeiros n (x:xs) = x:(nPrimeiros (n-1) xs)

nUltimos _ [] = []
nUltimos 0 _ = []
nUltimos n (xs) = inverso (nPrimeiros n (inverso xs))

-- 5) soma do enesimo de uma lista com o enesimo de outra lista
soma2 [] _ = []
soma2 _ [] = []
soma2 (x:xs) (y:ys) = (x+y):(soma2 xs ys)

-- 6) potencias de 2 a 2^n
pot2' 0 = []
pot2' n = (2^n:pot2' (n-1))
pot2 n = inverso(pot2' n)

-- 7) da merge em duas listas
intercalacao xs [] = xs
intercalacao [] ys = ys
intercalacao (x:xs) (y:ys) | x <= y = x:(intercalacao xs (y:ys))
                           | y < x = y:(intercalacao (x:xs) ys)

-- 8) menor elemento da lista
menor [x] = x
menor (x:xs) | x < (menor xs) = x
             | otherwise = (menor xs)

-- 9) remove elemento da lista
removerElem _ [] = []
removerElem n (x:xs) | n == x = xs
                     | otherwise = [x]++(removerElem n xs)

-- 10) ordene os elementos
ordenar [] = []
ordenar xs = (m):(ordenar (removerElem m xs))
    where m = menor xs

-- 11) insere o elemento na posicao correta ordenada
insereOrd y [] = [y]
insereOrd n (x:xs) | n <= x = n:(x:xs)
                   | otherwise = [x]++(insereOrd n xs) 

-- 12) retorna enesimo elemento
enesimo _ [] = 0
enesimo 0 _ = 0
enesimo n (x:xs) | n == 1 = x
                 | otherwise = enesimo (n-1) xs

-- 13) retorna uma lista com n elementos de valor e
repetir 0 _ = []
repetir n e = e:(repetir (n-1) e) 

-- 14) remove os tabs de uma string e substitui por espaco
removeTab [] = []
removeTab (x:xs) | x == '\t' = ' ':(removeTab xs)
                 | otherwise = [x]++(removeTab xs)

-- 15) transforma as letras maiusculas em minusculas
minusculas [] = []
minusculas (x:xs) = (toLower x):(minusculas xs)

-- 16) retorna o inverso de cada dupla
inversoDupla [] = []
inversoDupla ((x,y):xs) = (y,x):(inversoDupla xs)

-- 17) diz se duplas sao iguais
simetrico [] = []
simetrico ((x,y):xs) | x == y = (True):(simetrico xs)
                     | otherwise = (False):(simetrico xs)

-- 18) converte numero pra string
numToList 0 = []
numToList n = (numToList (div (n - (rem n 10)) 10))++[rem n 10]

gambiarra [] = []
gambiarra (x:xs) | x == 1 = '1':(gambiarra xs)
                 | x == 2 = '2':(gambiarra xs)
                 | x == 3 = '3':(gambiarra xs)
                 | x == 4 = '4':(gambiarra xs)
                 | x == 5 = '5':(gambiarra xs)
                 | x == 6 = '6':(gambiarra xs)
                 | x == 7 = '7':(gambiarra xs)
                 | x == 8 = '8':(gambiarra xs)
                 | x == 9 = '9':(gambiarra xs)
                 | x == 0 = '0':(gambiarra xs)

numString n = gambiarra (numToList n)

-- 19) converte string pra numero
gambiarra2 [] = []
gambiarra2 (x:xs) | x == '1' = 1:(gambiarra2 xs)
                  | x == '2' = 2:(gambiarra2 xs)
                  | x == '3' = 3:(gambiarra2 xs)
                  | x == '4' = 4:(gambiarra2 xs)
                  | x == '5' = 5:(gambiarra2 xs)
                  | x == '6' = 6:(gambiarra2 xs)
                  | x == '7' = 7:(gambiarra2 xs)
                  | x == '8' = 8:(gambiarra2 xs)
                  | x == '9' = 9:(gambiarra2 xs)
                  | x == '0' = 0:(gambiarra2 xs)

listToNum [x] = x
listToNum (x:xs) = x*10^(m-1) + (listToNum xs)
    where m = tam (x:xs)

stringNum xs = listToNum (gambiarra2 xs)

-- 20) converte de decimal pra binario
decBin 0 = []
decBin n | rem n 2 == 0 = (decBin (div n 2))++['0']
         | rem n 2 == 1 = (decBin (div n 2))++['1']

-- 21) converte de binario pra decimal
binDec [] = 0
binDec (x:xs) | x == '1' = 2^(m-1) + (binDec xs)
              | x == '0' = binDec xs
    where m = tam (x:xs)

-- 22) troco do cafe
moedas n | div n 50 > 0 = (50,(div n 50)):(moedas (n-(50*(div n 50))))
         | div n 20 > 0 = (20,(div n 20)):(moedas (n-(20*(div n 20))))
         | div n 10 > 0 = (10,(div n 10)):(moedas (n-(10*(div n 10))))
         | div n 5 > 0 = (5,(div n 5)):(moedas (n-(5*(div n 5))))
         | otherwise = []

trocoCafe x y = moedas (y-x)
