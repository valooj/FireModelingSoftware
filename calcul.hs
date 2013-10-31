import System.IO


--Factorielle à l'aide d'une liste
fac n = product [1..n]

--Factorielle récursive
fact n = if(n==0)then 1 else n*(fact(n-1))

--Petite fonction incrémentation
inc :: Int -> Int
inc = \x -> x+1

--Fonction d'ordre supérieure pour appliquer une fonction à des éléments d'une liste
e                     :: (a->b) -> [a] -> [b]
e f  []               =  []
e f (x:xs)            =  f x : map f xs

--
length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs 

--Fonction qui permet de lire le contenu des caractères d'un fichier
main = do
	withFile "essai.txt" ReadMode (\handle -> do 
	contents <- hGetContents handle
	putStr contents
	head contents)

--Les chaines de caratères sont des tableaux donc il suffit d'utiliser les fonctions
--des tableaux afin de les traiter. Désormais le problème est de convertir un
--IO en string de sortie.

--Exemple rapide de l'utilisation de putStrLn
hello = putStrLn "hello, world"

--Exemple de l'utilisation de getLine
yourname = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
    

--Monades (truc mathématique comme les monoides mais en fonctionnelle) 
-- permet de faire des entrées sorties
-- type String = [char] --type synonyme
--Parsec
--type Parse x = String->[]

--fonctions many et seq
--

--char :: CharString 

--Parsec [Monadic Parser Combinator]

--parse :: Parse Exp
int :: Parser Int => String -> [(Int)]
int = many(char '0' 'alt' char '1' 'alt' char '2' 'alt' char '3'
	'alt' char '4' 'alt' char '5' 'alt' char '6'
	'alt' char '7' 'alt' char '8' 'alt' char '9')

--Utilisation de Parsec
import Text.ParserCombinators.Parsec


csvFile = endBy line eol

line = sepBy cell separatorf --(char '+')

cell = many (noneOf "+ - ;")

separatorf = char '+' <|> char '-'

eol = char ';'

parsef :: String -> Either ParseError [[String]]
parsef input = parse csvFile "(unknown)" input
