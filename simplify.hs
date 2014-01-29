import Text.ParserCombinators.Parsec

main = do
 f <- readFile "ifc.txt"
-- f<- readFile "lib.txt"
 let m = (parse ifc "" f)
 print m

--faire (Right m) si on est sûr que le résultat est bon
--et qu'on veut supprimer le "Right" avant le texte à afficher
--Left m permet de remonter les erreurs

-- putStrLn demande un string
-- print imprime n'importe quel objet
-- print x = putStrLn(show x)

type IfcIdt = String
type IfcName = String
type IfcProp = [(String)]
type IfcModel = [(IfcIdt,IfcName,IfcProp)]

ifc :: Parser IfcModel
ifc = do
 ent <- many ifcentry
 eof
 return (ent)

ifcentry = do
 string "#"
 i <- idt
 string "= "
 name <- idt
 string "("
 prop <- many ifcprp
 string ")"
 string";"
 string"\n"
 return (i,name,prop)
 
idt = many (letter <|> digit <|> char ','
 <|> char '#' <|> char '$' <|> char '.'
 <|> char ')' <|> char '=' <|> char ';' <|> char '\n'
 <|> char ' ' <|> char '(' <|> char '\'')

ifcprp = do
 string "\'"
 sequence <- idt
 string "\'"
 return (sequence)

--lib
type LibIdt = String
type LibName = String
type LibProp = [(String)]
type LibModel = [(LibIdt,LibName,LibProp)]

lib :: Parser LibModel
lib = many libentry

libentry = do
 string "&"
 i <- idt
 string "= "
 name <- idt
 string "("
 prop <- many libprp
 string ")"
-- string ";"
-- string "\n"
 return (i,name,prop)

libprp = do
 string "'"
 sequence <- idt
 string "'"
 return (sequence)