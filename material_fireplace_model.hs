import Text.ParserCombinators.Parsec

main = do
 f <- readFile "projet3A.txt"
 let (Right m) = (parse ifc "" f)
 putStrLn m

lib :: Parser Model
lib = many entry

entry = do
 c <- cat
 return c
 
cat = do
	m <- mat
	return m
	<|>
	do
	s <- sbo
	return s

mat = do
 string "&MAT"
 id <- idt
 p <- param
 return (id,p)

sbo = do
 string "&SBO"
 id <- idt
 p <- param
 return (id,p)
 
idt = do
 string "ID= ' "
 l <- alphabet
 d <- integer
 string " '"
 return number_of_param

alphabet = many letter
integer = many digit

param = do
	l <- alphabet
	string " = "
	v <- value
	return (l,v)
	
val = do
 integer
 integer
 return (1k,l)
 <|>
 do
 alphabet
 return l
