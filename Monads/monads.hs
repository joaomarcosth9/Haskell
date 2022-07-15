main = putStr("Qual seu nome\n") >> a.txt >>= (\nome -> putStr ("Alo " ++ nome ++ "\n"))

main2 = do putStr("Qual seu nome\n");
           nome <- getLine;
           putStr("Alo " ++ nome);

main3 = do putStr("Arquivo: ");
        txt <- getFile a.txt;
        putStr(txt);
