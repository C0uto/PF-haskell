--primeiroUltimo :: [a] -> (a, a)
--primeiroUltimo xs = (head xs, last xs)

listaNomes :: [String] -> (String, String)
listaNomes nomes = (head nomes, last nomes)