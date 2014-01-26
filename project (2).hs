import Text.ParserCombinators.Parsec
import Prelude hiding (Integer)
import Types.hs

main = do
 f <- readFile "projet3A.txt"
 let (Right m) = (parse file "" f)
 putStrLn m

file :: Parser entry
file = many entry

entry :: Category
entry = do
 c <- cat
 return (CAT c)

cat :: Category
cat = do {
            ; m <- mat

            ; return (MAT m) }
          <|> do {
		         ; s <- sbo

				 ; return (SBO s) }

	       <|> do {
		      ; i <- ifc
			      ; return (IFC i) }

mat :: Mat				  
mat = do
 string "&MAT"
 number_of_entry <- idl
 name_of_entry <- paraml
 return MAT (number_of_entry,name_of_entry)

sbo :: Sbo
sbo = do
 string "&SBO"
 number_of_entry <- idl
 name_of_entry <- paraml
 return PairSbo (number_of_entry,name_of_entry)

ifc :: Ifc
ifc = do
 string "#"
 number_of_entry <- integer
 string " = "
 name_of_entry <- idi
 string " ("
 params_of_entry <- params
 string ")"
 return (ListISV (number_of_entry,name_of_entry,params_of_entry))

idl :: Idl
idl = do 
 string "ID = ' "
 l <- many letter
 string " ["
 d <- many digit
 string "]"
 return (PairLD (l,d))

paraml :: Paraml
paraml = do
 l <- many letter
 string " = "
 v <- val
 return (PairLV(l,v))

idi :: Idi
idi = do
 identity <- many (letter <|> digit)
 return identity

params :: Params
params = do
	p <- parami
	c <- cont
	return (List_Parami_Cont (p,c))
 <|> do
 e <- epsilon
 return (Vide e)

parami :: Parami
parami = do {
			string "#"
			
            ; i <- integer

            ; return i }
          <|> do {
		         ; i <- idi
				 
				 ; string "("
				 
				 ; ps <- params
				 
				 ; string")"
				 
				 ; return (List_id_params (i,ps)) }
	       <|> do {
					 ; string "'"
					 
					 ; t <- text
					 
					 ; string "'"
					 
					 ; return t}

cont :: Cont				 
cont = do
	string ","
	p <- parami
	c <- cont
	return (List_Cont (p,c))
	<|> do
 e <- epsilon
 return (Vide1 e)
	
val :: Value
val = do {
            ; d <- integer
			
			; string " [."
			
			; dd <- integer
			
			; string "]"

            ; return (PairInt (d,dd)) }
          <|> do {
		         ; string "' "
				 
				 ; l <- many letter
				 
				 ; string " '"
				 
				 ; return (Val l) }


e :: Event_room
e = do
	n <- room
	string " "
	u <- undesired
	return (PairRU (n,u))

undesired :: Undesired
undesired = do {
			; s <- symbol
			
			; string " < "
			
            ; v <- val

            ; return (PairInf (s,v)) }
          <|> do {
		         ; s <- symbol

				 ; string " > "
				 
				 ; v <- val
				 
				 ; return (PairSup (s,v)) }

	       <|> do {
					 ; u <- undesired
					 
					 ; string " && "
					 
					 ; u1 <- undesired
			  
					 ; return (And (u,u1)) }
 

text :: Text
text = do
 txt <- many letter
 return txt

integer :: Integer
integer = many1 digit

symbol :: Symbol
symbol = many letter

room :: Room
room = many1 letter

epsilon :: Epsilon
epsilon = do 
 return string ""
