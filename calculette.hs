import System.IO 
import Text.ParserCombinators.Parsec

-- chercher documentation sur PARSEC "Monoide Parser Combinator" 

type String = [char] -- définition synonyme de String

data Exp = val Int --| ... | ...

eval (val v) = v
eval (Plus e e') = eval(e) + eval(e')

--Parsec

type Parse x = String -> [(x, String)]
char :: char -> Parser char
all :: Parser x -> Parsec x -> Parser x

many :: Parsec x -> Parsec[x]
seq :: Parser x -> (x -> Parser y) -> Parser y

int :: Parse Int => String -> Int
