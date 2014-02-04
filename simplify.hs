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
 string "("
 prop <- ifcprop
 eol
 return (i,name,prop)

idtnumber = chiffre
idtname =  alphabet
--idt = many ( digit <|> letter <|> char ',' <|> char '$'
-- <|> char ')' <|> char '\'' <|> char ' ' <|> char '('
-- <|> char '.' <|> char '#')

ifcprop = sepBy prop (char ',')
prop = many (noneOf ",")
eol = string ");"
chiffre = many digit
alphabet = many letter

--ip = many (letter <|> digit <|> char '.' <|> char ',')
--code permettant de distinguer les propriétés

-- ifcprp = do
 -- string "\'"
 -- sequence <- ip
 -- string "\'"
 -- return (sequence)
 -- <|>
 -- do
 -- string "#"
 -- numero <- chiffre -- on doit définir une nuovelle fonction
 -- return (numero)   -- car la précédente (idt) avale trop de caractères
 -- <|>
 -- do
 -- string "$"
 -- return ("$")
 -- <|>
 -- do
 -- string "."
 -- alpha <- alphabet
 -- string"."
 -- return (alpha)
 
 

--lib
-- type LibIdt = String
-- type LibName = String
-- type LibProp = [(String)]
-- type LibModel = [(LibIdt,LibName,LibProp)]

-- lib :: Parser LibModel
-- lib = many libentry

-- libentry = do
 -- string "&"
 -- i <- idt
 -- string "= "
 -- name <- idt
 -- string "("
 -- prop <- many libprp
 -- string ")"
-- string ";"
-- string "\n"
 -- return (i,name,prop)

-- libprp = do
 -- string "'"
 -- sequence <- idt
 -- string "'"
 -- return (sequence)