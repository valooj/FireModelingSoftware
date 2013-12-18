import Text.ParserCombinators.Parsec

main = do
 f <- readFile "projet3A.txt"
 let (Right m) = (parse ifc "" f)
 putStrLn m
 

data Value = Ref Int | Val String | Pair (String, [Value])
data Ifc = List [(Int, String, Value)]

parseFile :: Parser Ifc
parseFile = many entry

entry = do
 c <- cat
 return c

cat = do {
            ; m <- mat

            ; return m }
          <|> do {
		         ; s <- sbo

				 ; return s }

	       <|> do {
		      ; i <- ifc
			      ; return i}

mat = do
 string "&MAT"
 number_of_entry <- idl
 name_of_entry <- paraml
 return (number_of_entry,name_of_entry)

sbo = do
 string "&SBO"
 number_of_entry <- idl
 name_of_entry <- paraml
 return (number_of_entry,name_of_entry)

ifc = do
 string "#"
 number_of_entry <- integer
 string " = "
 name_of_entry <- idi
 string " ("
 params_of_entry <- params
 string ")"
 return (number_of_entry,name_of_entry,params_of_entry)

idl = do 
 string "ID = ' "
 l <- many letter
 string " "
 d <- many digit
 string " ' "
 return (l,d)

paraml = do
 l <- many letter
 string " = "
 v <- val
 return (l,v)

idi = do
 identity <- many (letter <|> digit)
 return identity

params = do
	p <- parami
	c <- cont
	return (p,c)
 <|> do
 epsilon

parami = do {
			string "#"
			
            ; i <- integer

            ; return i }
          <|> do {
		         ; i <- idi
				 
				 ; string "("
				 
				 ; ps <- params
				 
				 ; string")"
				 
				 ; return (i,ps) }
	       <|> do {
					 ; string "'"
					 
					 ; t <- text
					 
					 ; string "'"
					 
					 ; return t}
	
cont = do
	string ","
	p <- parami
	c <- cont
	return (p,c)
	<|> do
	epsilon
	
val = do {
            ; d <- many1 digit
			
			; string " [."
			
			; dd <- many1 digit
			
			; string "]"

            ; return (d,dd) }
          <|> do {
		         ; string "' "
				 
				 ; l <- many letter
				 
				 ; string " '"
				 
				 ; return l }

e = do
	n <- room
	string " "
	u <- undesired
	return (n,u)
	
undesired = do {
			; s <- symbol
			
			; string " < "
			
            ; v <- val

            ; return (s,v) }
          <|> do {
		         ; s <- symbol

				 ; string " > "
				 
				 ; v <- val
				 
				 ; return (s,v) }

	       <|> do {
					 ; u <- undesired
					 
					 ; string " && "
					 
					 ; u1 <- undesired
			  
					 ; return (u,u1) }
 

text = do
 txt <- many letter
 return txt

integer = many1 digit

symbol = many letter

room = many1 letter

epsilon = do 
 return string ""
