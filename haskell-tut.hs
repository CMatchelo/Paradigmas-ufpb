-- Paradigmas aplicado a comutação - Trabalho final
-- Grupo 4 - Programação funcional
-- Alisson, Cicero, Diogo, Evandro
-- Vá para a pasta onde esse arquivo esta
-- Execute "ghci" no terminal pra abrir o compilador Haskell
-- Rode :l haskell-tut para executar

-- Comment
import Data.List
import System.IO
--
-- Exemplo 14.6 do livro
--
powerc :: Int -> Float -> Float
power :: (Int, Float) -> Float

power (n,b) = if n == 0 then 1.0 else b * power (n-1, b)
powerc n b  = if n == 0 then 1.0 else b * powerc (n-1) b

--
--
-- Exemplos extra de livre escolha
--
multiplyList :: Integer -> [Integer] -> [Integer]
multiplyList _ [] = []
multiplyList m (n:ns) = (m*n) : multiplyList m ns

-- Exemplo 14.4 do Livro
summ :: [Int] -> Int
summ[] = 0
summ (n:ns) = n+sum ns

-- Exemplo 14.7 do livro
filterr :: (t -> Bool) -> [t] -> [t]
filterr f[] = []
filterr f(x : xs) = 
	if f x then x : filterr f xs else filterr f xs

mapp :: (s -> t) -> [s] -> [t]
mapp f[] = []
mapp f (x:xs) = f x : map f xs

-- [1,2,3,4] : x=1 | xs=[2,3,4]
-- [2,3,4]   : x=2 | xs=[3,4]
-- [3,4]     : x=3 | xs=[4]
-- [4]       : x=4 	

-- Exemplo 14.4 do Livro
data Shape = Pointy | Circular Float | Rect(Float, Float)
area :: Shape -> Float

area s = case s of
	Pointy     -> 0.0
	Circular r -> pi*r*r
	Rect (h,w) -> h*w



--
-- Tratamento de listas
numsPrimos = 3 : 5 : 7 : 11 : []
maisPrimo = numsPrimos ++ [13, 17, 19, 23] :: [Float]
maxPrimo = maximum maisPrimo

-- Funcao com if
ehImpar :: Int -> Bool
ehImpar n
	| n`mod` 2==0 = False
	| otherwise = True
ehPar n = n`mod` 2 == 0

localSala :: Int -> String

localSala num
	| (num >= 100) && (num <= 199) = "Primeiro Andar"
	| (num >= 200) && (num <= 299) = "Segundo Andar"
	| (num >= 300) && (num <= 399) = "Terceiro Andar"

media :: Double -> Double -> Double -> String
media n1 n2 n3
	| avg < 4.0 = "Reprovado"
	| avg < 7.0 = "Final"
	| avg >= 7.0 = "Aprovado"
	where avg = (n1 + n2 + n3) / 3.0 

-- Exemplo do livro simplificado
powercSimples :: Int -> Float -> Float
powerSimples :: (Int, Float) -> Float

powercSimples n b = b^n
powerSimples (n,b) = b^n

-- Lazy Lists

from :: Int -> [Int]
from n = n : from (n+1)
firstm7 :: [Int] -> Int
firstm7 [] = 0
firstm7 (n : ns) =
	if n`mod`7==0 then n else firstm7 ns
-- Lazy Lists 2
approxRoots :: Float -> [Float]
approxRoots x =
	let rootsFrom r =
		r : rootsFrom (0.5 * (r + x/r))
	in rootsFrom 1.0

absolute :: Float -> [Float] -> Float
absolute eps (r1 : r2 : rs) =
	if abs (r1 - r2) <= eps
	then r2
	else absolute eps (r2 : rs)
sqrtt = absolute 0.0001 . approxRoots



-- Programa basico com entrada
--main = do
--putStrLn "Qual seu nome?"
--nome <- getLine
--putStrLn ("Ola " ++ nome)


