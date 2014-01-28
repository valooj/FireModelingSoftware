import Text.ParserCombinators.Parsec

main = do
 f <- readFile "projet3A.txt"
 let m = (parse ifc "" f)
 print m

--faire (Right m) si on est sûr que le résultat est bon
--et qu'on veut supprimer le "Right" avant le texte à afficher

-- putStrLn demande un string
-- print imprime n'importe quel objet
-- print x = putStrLn(show x)

type Idt = String
type Name = String
type Prop = String
type Model = [(Idt,Name,Prop)]

ifc :: Parser Model
ifc = many entry

entry = do
 string "#"
 i <- idt
 string "= "
 name <- idt
 string"("
 prop <- idt
 string")"
-- string";"
-- string"\n"
 return (i,name,prop)
 
idt = many (letter <|> digit <|> char '\'' <|> char ','
 <|> char '#' <|> char '$' <|> char '.')
-- <|> char ')' <|> char ';' <|> char '\n' <|> char '='
-- <|> char ' ' <|> char '(')