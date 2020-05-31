module Parser (
    Expression (..),
    parse
) where

import Parsers

data Expression = Variable String
                 |Abstraction String Expression
                 |Application Expression Expression
                 |LetExpr (String, Expression) Expression
                 deriving Eq

instance Show Expression where
    show (Variable c) = c
    show (Abstraction c e) = "(L" ++ c ++ "." ++ (show e) ++ ")"
    show (Application e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (LetExpr (c,def) ein) = "let " ++ c ++ " = " ++ (show def) ++ " in:\n" ++ (show ein)

pvariable :: Parser Expression
pvariable = (pmany1 plower) |>> Variable

pabstraction :: Parser Expression
pabstraction = pbparens (pseq (pbetween (pchar 'L') (pchar '.') (pmany1 plower)) pexpression (\(v, e) -> Abstraction v e))

papplication :: Parser Expression
papplication = pbparens (pseq (pleft pexpression (pchar ' ')) pexpression (\(e1, e2) -> Application e1 e2))

plet :: Parser Expression
plet = pseq (pbetween (pstr "let ") (pstr " in ") 
             (pseq (pleft (pmany1 plower) (pstr " = ")) pexpression id) 
            ) pexpression (\(b, e) -> LetExpr b e)

pexpression :: Parser Expression
pexpression = plet <|> pvariable <|> pabstraction <|> papplication

parse :: String -> Maybe Expression
parse i = case pexpression . prepare $ i of
               Success res _ -> Just res
               Failure _ _   -> Nothing
