import System.IO

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

fac n = product [1..n]

fact n = if(n==0)then 1 else n*(fact(n-1))

inc :: Int -> Int
inc = \x -> x+1

pls :: Int -> Int
pls  = \n -> n+4

e                     :: (a->b) -> [a] -> [b]
e f  []               =  []
e f (x:xs)            =  f x : map f xs


length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs 


main = do
	withFile "essai.txt" ReadMode (\handle -> do 
	contents <- hGetContents handle
	putStr contents
	head contents)

--Les chaines de caratères sont des tableaux donc il suffit d'utiliser les fonctions
--des tableaux afin de les traiter. Désormais le problème est de convertir un
--IO en string de sortie.
--

hello = putStrLn "hello, world"

yourname = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

--Monades (truc mathématique comme les monoides mais en fonctionnelle) 
-- permet de faire des entrées sorties
-- type Sring = [char] --type synonyme
--Parsec
--type Parse x = String->[]

--fonctions many et seq
--

--Parsec [Monadic Parser Combinator]

--parse :: Parse Exp
--int :: Parser Int = String -> [(Int)]
--int = many(char'0') 'alt' ...)

char :: CharString 
