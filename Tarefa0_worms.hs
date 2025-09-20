{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
type Matriz a = [[a]]
type Dimensao = (Int, Int)
type Posicao = (Int, Int)


eIndiceListaValido :: Int -> [lista] -> Bool
eIndiceListaValido indice lista = 0 <= indice && indice < length lista

dimensaoMatriz :: Matriz a -> Dimensao 
dimensaoMatriz [] = (0,0)
dimensaoMatriz matriz = (length matriz, length (head matriz))

ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool
ePosicaoMatrizValida (l,c) ls = eIndiceListaValido l ls && eIndiceListaValido c (ls !! l)

eMatrizValida :: Matriz a -> Bool
eMatrizValida [] = True
eMatrizValida (x:xs) = validaComprimentos (length x) xs 

validaComprimentos :: Int -> Matriz a -> Bool
validaComprimentos len [] = True    
validaComprimentos len (x:xs) = length x == len && validaComprimentos len xs