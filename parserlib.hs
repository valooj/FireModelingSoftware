import Text.ParserCombinators.Parsec
--lib
main = do
 f<- readFile "lib.txt"
 let m = (parse lib "" f)
 print m

type LibName = String
type LibProp = [[String]]
type LibModel = [(LibName,LibProp)]

lib :: Parser LibModel
lib = many libentry

libentry = do
 string "&"
 i <- libname
 string " "
 prop <- libprop
 eol
 return (i,prop)

libname = many letter
eol = string "/\n"

libprop = sepBy propname (char ' ')
propname = sepBy prop (char '=')
prop = many (noneOf " =/\n")
