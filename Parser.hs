module Parser (
    Expression (..),
    parse
) where

import Parsers

data Expression = Variable Char
                 |Abstraction Char Expression
                 |Application Expression Expression
                 deriving (Eq, Show)

pvariable :: Parser Expression
pvariable = plower |>> Variable

pabstraction :: Parser Expression
pabstraction = pbparens (pseq (pbetween (pchar 'L') (pchar '.') plower) pexpression (\(v, e) -> Abstraction v e))

papplication :: Parser Expression
papplication = pbparens (pseq pexpression pexpression (\(e1, e2) -> Application e1 e2))

pexpression :: Parser Expression
pexpression = pvariable <|> pabstraction <|> papplication

parse :: String -> Maybe Expression
parse i = case pexpression . prepare $ i of
               Success res _ -> Just res
               Failure _ _   -> Nothing
