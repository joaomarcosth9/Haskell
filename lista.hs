import Data.Char

list1 = 1:[]

list2 = 1:2:3:[]

list4 = [0,5..100]

-- retorna o tamanho de uma lista
tam [] = 0
tam (x:xs) = 1 + tam xs

-- retorna o maior elemento de uma lista
maior [x] = x
maior (x:xs) = if x > m then x else m
    where m = maior xs

-- retorna o menor elemento de uma lista
menor [x] = x
menor (x:xs) = if x < m then x else m
    where m = menor xs

-- retorna o somatorio de uma lista
somatorio [] = 0
somatorio (x:xs) = x + somatorio xs 

-- dobra todos os elementos de uma lista
dobra [] = []
dobra (x:xs) = 2*x:(dobra xs)

-- retorna os n primeiros elementos de uma lista
nprimeiros _ [] = []
nprimeiros 0 _ = []
nprimeiros n (x:xs) = x:(nprimeiros (n-1) xs)

-- transforma todos os elementos de uma lista em maiusculas
maiuscula [x] = toUpper(x):[]
maiuscula (x:xs) = toUpper(x):(maiuscula xs)

-- concatena duas listas
concatena [] ys = ys
concatena (x:xs) ys = x:concatena xs ys

-- verifica se um elemento pertence a uma lista
pertence e [] = False
pertence e (x:xs) | e == x = True
                  | otherwise = pertence e xs

pertence' e xxs@(x:xs) | e == x = True
                       | xxs == [] = False
                       | otherwise = pertence' e xs

pertence'' e xs | e == head xs = True
                | xs == [] = False
                | otherwise = pertence'' e (tail xs)

pertence''' e (x:xs) | e == x = True
                     | (x:xs) == [] = False
                     | otherwise = pertence''' e xs

-- retorna a intersecção de duas listas
inter [] ys = []
inter (x:xs) ys | pertence x ys = x:(inter xs ys)

inter' xs ys | xs == [] = []
             | pertence (head xs) ys = (head xs):(inter' (tail xs) ys)

-- retorna a lista reversa
reverso [] = []
reverso (x:xs) = (reverso xs)++[x]

reverso' [] = []
reverso' (x:xs) = concatena (reverso' xs) [x]

-- retorna o maior e o menor elementos de uma lista
maiorMenor (xs) = (menor xs, maior xs)

-- retorna uma lista com duplas, contendo os n'esimos elementos de cada lista
combina [] xs = []
combina ys [] = []
combina (x:xs) (y:ys) = (x,y):combina xs ys

desCombina [] = ([],[])
desCombina ((x,y):xs) = (x:fst (desCombina xs), y:snd (desCombina xs))

desCombina' [] = ([],[])
desCombina' ((x,y):xs) = (x:l1, y:l2)
    where (l1,l2) = desCombina' xs

--compreensao de lista

