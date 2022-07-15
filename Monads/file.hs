import System.IO

main = do putStr ("Arquivo: ")
          txt <- readFile "a.txt"
          putStr(txt)
