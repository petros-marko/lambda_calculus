module Parser (
    Expression (..),
    parse
) where

import Parsers

data Expression = Variable String
                 |Abstraction String Expression
                 |Application Expression Expression
                 deriving Eq

instance Show Expression where
    show (Variable c) = c
    show (Abstraction c e) = "(L" ++ c ++ "." ++ (show e) ++ ")"
    show (Application e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"

pvariable :: Parser Expression
pvariable = (pmany1 plower) |>> Variable

pabstraction :: Parser Expression
pabstraction = pbparens (pseq (pbetween (pchar 'L') (pchar '.') (pmany1 plower)) pexpression (\(v, e) -> Abstraction v e))

papplication :: Parser Expression
papplication = pbparens (pseq (pleft pexpression (pchar ' ')) pexpression (\(e1, e2) -> Application e1 e2))

pexpression :: Parser Expression
pexpression = pvariable <|> pabstraction <|> papplication

parse :: String -> Maybe Expression
parse i = case pexpression . prepare $ i of
               Success res _ -> Just res
               Failure _ _   -> Nothing
