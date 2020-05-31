module Interpreter (
    eval
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Parser

alphaNormalize :: Expression -> Expression
alphaNormalize e = fst $ alphaNormalizeHelper e (free e) (Map.fromList [])

alphaNormalizeHelper :: Expression -> Set.Set String -> Map.Map String String -> (Expression, Set.Set String)
alphaNormalizeHelper var@(Variable c) u m = case Map.lookup c m of
                                                 Just c' -> (Variable c', Set.insert c' u)
                                                 Nothing -> (var, u)
alphaNormalizeHelper abs@(Abstraction c e) u m = if Set.member c u then
                                                    let c' = fresh u
                                                        r  = (alphaNormalizeHelper e (Set.insert c' u) (Map.insert c c' m))
                                                        (e' , u') = r
                                                    in
                                                        ((Abstraction c' e'), u')
                                                 else
                                                    let r = (alphaNormalizeHelper e (Set.insert c u) m)
                                                        (e', u') = r
                                                    in
                                                        (Abstraction c e', u')
alphaNormalizeHelper app@(Application e1 e2) u m = let r1 = (alphaNormalizeHelper e1 u m)
                                                       (e1', u1') = r1
                                                       r2 = (alphaNormalizeHelper e2 u1' m)
                                                       (e2', u2') = r2
                                                    in
                                                       (Application e1' e2', u2')

free :: Expression -> Set.Set String
free e = freeHelper e (Set.fromList []) (Set.fromList [])

freeHelper :: Expression -> Set.Set String -> Set.Set String -> Set.Set String
freeHelper (Variable c) bound soFar = if Set.member c bound then
                                         soFar
                                      else
                                         Set.insert c soFar
freeHelper (Abstraction c e) bound soFar = freeHelper e (Set.insert c bound) soFar
freeHelper (Application e1 e2) bound soFar = Set.union (freeHelper e1 bound soFar) (freeHelper e2 bound soFar)

fresh :: Set.Set String -> String
fresh s = Set.elemAt 0 $ Set.difference (Set.fromList [show l| l <- ['a'..'z']]) s

betaReduce :: String -> Expression -> Expression -> Expression
betaReduce v var@(Variable c) w = if v == c then w else var
betaReduce v abs@(Abstraction c e) w = if v /= c then Abstraction c (betaReduce v e w) else abs 
betaReduce v app@(Application e1 e2) w = Application (betaReduce v e1 w) (betaReduce v e2 w)

eval :: Expression -> Expression
eval var@(Variable _) = var
eval abs@(Abstraction c exp) = Abstraction c (eval exp)
eval app = let app'@(Application e1 e2) = alphaNormalize app
               r1 = eval e1
               r2 = eval e2
           in
               case r1 of
                    abs@(Abstraction c e) -> eval $ betaReduce c e r2
                    exp                   -> Application r1 r2
