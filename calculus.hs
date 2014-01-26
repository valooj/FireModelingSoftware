--Exemple pris à partir du chapitre 16 Parsec, Haskell real world

import Text.ParserCombinators.Parsec

File = endBy line eol

line = sepBy cell separatorf --(char '+')

cell = many (noneOf "+ - ;")

separatorf = char '+' <|> char '-'

eol = char ';'

parsef :: String -> Either ParseError [[String]]
parsef input = parse File "(unknown)" input


--data Exp = Val v

--eval (Val v) = v
--eval (Plus e e') = eval(e) + eval(e')

--type Parse x = String -> [(x,String)]

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
     <|> (char '+' >> return ' ')
     <|> p_hex

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

