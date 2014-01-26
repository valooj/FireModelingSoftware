import Text.ParserCombinators.Parsec

file = endBy ifc eoifc

ifc = sepBy idt sepidt

idt = many (noneOf "# = ;")

sepidt = char '#' <|> char '='

eoifc = char ';'

 
--int :: Parser Int => String -> [(Int)]
--int = many(char '0' 'alt' char '1' 'alt' char '2' 'alt' char '3'
--        'alt' char '4' 'alt' char '5' 'alt' char '6'
--        'alt' char '7' 'alt' char '8' 'alt' char '9')
		
parseidt :: String -> Either ParseError [[String]]
parseidt input = parse file "" input