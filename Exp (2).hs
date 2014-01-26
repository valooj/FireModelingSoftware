import Prelude hiding (exp)

import Text.ParserCombinators.Parsec

data Value = Int | String | (String, [Value])
data Exp = Val Int | Plus Exp Exp | Mult Exp Exp | Less Exp Exp
  deriving Show

eval (Val v)     = v

eval (Plus e e') = (eval e)+(eval e')

eval (Mult e e') = (eval e)*(eval e')
eval (Less e e') = (eval e)-(eval e')

int :: Parser Exp
int = do

 v <- many1 digit

 return (Val (read v))



exp = do e <- int

         do { char '+'
            ; e' <- exp

            ; return (Plus e e') }
          <|> do { char '*'

		         ; e' <- exp

				 ; return (Mult e e') }

	       <|> do { char '-'
		      ; e' <- exp
			      ; return (Less e e')}
			
			<|> return e


calc txt = eval ast

  where (Right ast) = parse exp "" txt

-- calc "12+23*34" == 794
