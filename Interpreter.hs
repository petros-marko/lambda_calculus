module Interpreter (
    eval
) where

import Parser

alphaReduce :: Expression -> Char -> Char -> Expression
alphaReduce wh from to = wh -- TODO Write real implementation for alpha reduction

betaReduce :: Expression -> Expression -> Expression
betaReduce wh to = to -- TODO Write real implementation for beta reduction

eval :: Expression -> Expression
eval var@(Variable _) = var
eval abs@(Abstraction c exp) = Abstraction c (eval exp)
eval app@(Application e1 e2) = let r1 = eval e1
                                   r2 = eval e2
                               in
                                   case r1 of
                                        abs@(Abstraction _ _) -> eval $ betaReduce r1 r2
                                        exp                   -> Application r1 r2
