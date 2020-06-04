module Interpreter (
    eval
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Parser

alphaNormalize :: Expression -> Expression
alphaNormalize e = fst $ alphaNormalizeHelper e (free e) (Map.fromList [])

alphaNormalizeHelper :: Expression -> Set.Set String -> Map.Map String String -> (Expression, Set.Set String)
alphaNormalizeHelper l_t@(LetExpr b e) u m = let r = (alphaNormalizeHelper e u m)
                                                 (e', u') = r
                                             in  (LetExpr b e', u')
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
freeHelper (LetExpr (_, d) e) bound soFar = Set.union (freeHelper d bound soFar) (freeHelper e bound soFar)
freeHelper (Variable c) bound soFar = if Set.member c bound then
                                         soFar
                                      else
                                         Set.insert c soFar
freeHelper (Abstraction c e) bound soFar = freeHelper e (Set.insert c bound) soFar
freeHelper (Application e1 e2) bound soFar = Set.union (freeHelper e1 bound soFar) (freeHelper e2 bound soFar)

fresh :: Set.Set String -> String
fresh s = let allStrings = [ c : str | str <- "" : allStrings, c <- ['a'..'z'], (not $ (c:str) `Set.member` s)] in
              allStrings !! 0


betaReduce :: String -> Expression -> Expression -> Expression
betaReduce v l_t@(LetExpr b e) w = LetExpr b (betaReduce v e w)
betaReduce v var@(Variable c) w = if v == c then w else var
betaReduce v abs@(Abstraction c e) w = if v /= c then Abstraction c (betaReduce v e w) else abs 
betaReduce v app@(Application e1 e2) w = Application (betaReduce v e1 w) (betaReduce v e2 w)

evalHelper :: Expression -> Map.Map String Expression -> (Expression, Map.Map String Expression)
evalHelper l_t@(LetExpr (c, d) e) m = evalHelper e (Map.insert c d m)
evalHelper var@(Variable c) m = case Map.lookup c m of
                                     Just e -> (e, m) --evalHelper e m
                                     Nothing -> (var, m)
evalHelper abs@(Abstraction c exp) m = let r = evalHelper exp m
                                           (e, m') = r
                                       in  (Abstraction c e, m')
evalHelper app@(Application e1 e2) m = case e1 of
                                            var'@(Variable c) -> let r = evalHelper var' m
                                                                     (e, m') = r
                                                                 in if var' == e then
                                                                       let r' = evalHelper e2 m
                                                                           (e', m'') = r'
                                                                       in (Application var' e', m'')
                                                                    else 
                                                                       (Application e e2, m')
                                            abs'@(Abstraction c e) -> (betaReduce c e e2, m)
                                            app' -> let r = evalHelper e1 m
                                                        (e', m') = r
                                                    in  if e' /= e1 then
                                                           (Application e' e2, m')
                                                        else
                                                           let r' = evalHelper e2 m
                                                               (e'', m'') = r'
                                                           in  (Application e1 e'', m'')

betaNorm :: Expression -> Map.Map String Expression -> Expression
betaNorm e m = let e' = alphaNormalize e
                   r  = evalHelper e' m
                   (e'', m') = r
               in  if e'' == e' then
                      e
                   else
                      betaNorm e'' m'

eval :: Expression -> Expression
eval e = betaNorm e (Map.fromList [])
