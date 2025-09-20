import Data.Map (Map)
--Tarefa a
primeiroUltimo :: [a] -> (a, a)
primeiroUltimo xs = (head xs, last xs)

--Tarefa b
listaNomes :: [String] -> (String, String)
listaNomes nomes = (head nomes, last nomes)

--Tarefa c
primeiroEUltimoNome :: [String] -> String
primeiroEUltimoNome [] = ""
primeiroEUltimoNome nomes = head nomes ++ " " ++ last nomes

--Tarefa d
calculoLengthUltimoNome :: [String] -> Int
calculoLengthUltimoNome [] = 0
calculoLengthUltimoNome nomes = length(last nomes)

--Tarefa e
caracterNaString :: Char -> String -> Bool
caracterNaString char str = elem char str

--Tarefa f
removerElementos :: [Char] -> [Char]
removerElementos nomes = drop 2 (nomes)




