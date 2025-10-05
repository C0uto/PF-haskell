-- Ficha1.hs
-- Exercício 1
perimetro :: Double -> Double
perimetro r = 2 * pi * r

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

primUlt :: [a] -> (a, a)
primUlt xs = (head xs, last xs)

multiplo :: Int -> Int -> Bool
multiplo m n = mod n m == 0

truncaImpar :: [a] -> [a]
truncaImpar xs = if mod (length xs) 2 /= 0 
                    then init xs 
                    else xs

max2 :: (Int,Int) -> Int
max2 (n,m) = if n > m 
                then n 
                else m 

max3 :: (Int,Int,Int) -> Int
max3 (n,m,x) = if n >= m && n >= x 
                then n 
                else if m >= n && m >= x 
                        then m 
                        else x

-- Exercício 2
