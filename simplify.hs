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
type IfcProp = [String]
type IfcModel = [(IfcIdt,IfcName,IfcProp)]

ifc :: Parser IfcModel
ifc = many ifcentry

ifcentry = do
 string "#"
 i <- idtnumber
 string "= "
 name <- idtname
 opening
 prop <- ifcprop
 closing
 eol
 return (i,name,prop)

idtnumber = many digit
idtname = many (letter <|> digit)

ifcprop = sepBy prop (char ',')
prop = many (noneOf "=,();\n")
eol = string ";\n"
opening = try (string "((")
 <|> string "("
closing = try (string "))")
 <|> string ")"