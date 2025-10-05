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
nRaizes :: Double -> Double -> Double -> Int
nRaizes a b c
    | d > 0     = 2
    | d == 0    = 1
    | otherwise = 0
    where d = b^2 - 4*a*c

raizes :: Double -> Double -> Double -> [Double]
raizes a b c
    | n == 2    = [(-b + sqrt d) / (2*a), (-b - sqrt d) / (2*a)]
    | n == 1    = [(-b) / (2*a)]
    | otherwise = []
    where
        d = b^2 - 4*a*c
        n = nRaizes a b c

-- Exercício 3
type Hora = (Int, Int)

horaValida :: Hora -> Bool
horaValida (h,m) = h > 0 && h <= 23 && m > 0 && m <= 59

compararHoras :: Hora -> Hora -> Bool
compararHoras (h1,m1) (h2,m2) = h1 > h2 || (h1 == h2 && m1 > m2)

converterHoras :: Hora -> Int
converterHoras (h,m) = h*60 + m

converterMinutos :: Int -> Hora
converterMinutos m = (div m 60, mod m 60)

diferencaHoras :: Hora -> Hora -> Int
diferencaHoras (h1,m1) (h2,m2) = converterHoras (h1,m1) - converterHoras (h2,m2)

-- Exercício 5
data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

stop :: Semaforo -> Bool
stop Vermelho = True
stop _ = False

safe :: Semaforo -> Semaforo -> Bool
safe Vermelho _ = True
safe _ Vermelho = True
safe _ _ = False

-- Exercício 6
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show, Eq)