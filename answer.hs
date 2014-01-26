import Text.ParserCombinators.Parsec

main = do
 f <- readFile "answer.txt"
 let (Right m) = (parse dsc "" f)
 putStrLn (trf m)

type Comp  = String
type Prop  = String
type Value = String
type Model = [(Comp,[(Prop,Value)])]

trf ms = concat [ c++"\t"++n++"\t"++v++"\n" 
                | (c,ps)<-ms, (n,v)<-ps]

dsc :: Parser Model
dsc = many cmp

cmp = do
 string "&"
 i <- idt
 string " "
 ps <- sepBy prp (string " ")
 string "\n"
 return (i,ps)

idt = many letter

prp = do
 i <- idt
 string "="
 v <- val
 return (i,v)

val = do
 string "\""
 v <- many (letter <|> digit <|> char ',')
 string "\""
 return v
