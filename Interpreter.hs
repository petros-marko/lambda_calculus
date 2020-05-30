module Interpreter (
    eval
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Parser

alphaNormalize :: Expression -> Expression
alphaNormalize e = alphaNormalizeHelper e (free e) (Map.fromList [])

alphaNormalizeHelper :: Expression -> Set.Set Char -> Map.Map Char Char -> Expression
alphaNormalizeHelper var@(Variable c) u r = case Map.lookup c r of
                                                 Just c' -> Variable c'
                                                 Nothing -> var
alphaNormalizeHelper abs@(Abstraction c e) u r = if Set.member c u then
                                                    let c' = fresh u in
                                                        Abstraction c' (alphaNormalizeHelper e (Set.insert c' u) (Map.insert c c' r))
                                                 else
                                                    Abstraction c (alphaNormalizeHelper e (Set.insert c u) r)
alphaNormalizeHelper app@(Application e1 e2) u r = Application (alphaNormalizeHelper e1 u r) (alphaNormalizeHelper e2 u r)

free :: Expression -> Set.Set Char
free e = freeHelper e (Set.fromList []) (Set.fromList [])

freeHelper :: Expression -> Set.Set Char -> Set.Set Char -> Set.Set Char
freeHelper (Variable c) bound soFar = if Set.member c bound then
                                         soFar
                                      else
                                         Set.insert c soFar
freeHelper (Abstraction c e) bound soFar = freeHelper e (Set.insert c bound) soFar
freeHelper (Application e1 e2) bound soFar = Set.union (freeHelper e1 bound soFar) (freeHelper e2 bound soFar)

fresh :: Set.Set Char -> Char
fresh s = Set.elemAt 0 $ Set.difference (Set.fromList ['a'..'z']) s

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
