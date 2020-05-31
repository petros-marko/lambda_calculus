module Parsers (
    prepare,
    Input,
    Outcome (..),
    Parser,
    pseq, pchar, plower, (<|>), (|>>), pbetween, pmany0, pmany1, pbparens, pleft
) where

import Data.Char
import Data.List

type Input = (String, Int)

prepare :: String -> Input
prepare s = (s, 0)

input :: Input -> String
input (i, _) = i

position :: Input -> Int
position (_, p) = p

isEOF :: Input -> Bool
isEOF i = ((>=) (position i)) . length . input $ i

data Outcome a = Success a Input |Failure Int String
type Parser a = Input -> Outcome a

presult :: t -> Input -> Outcome t
presult a i = Success a i

pzero :: Parser a
pzero i = Failure (position i) "pzero"

pitem :: Parser Char
pitem (istr, pos) = if pos >= length istr then
                       Failure pos "pitem"
                    else
                       Success (istr !! pos) (istr, pos + 1)

pbind :: Parser a -> (a -> Parser b) -> Input -> Outcome b
pbind p f i = case p i of
                   Success a i' -> f a i'
                   Failure p r  -> Failure p r

pseq :: Parser a -> Parser b -> ((a, b) -> c) -> Parser c
pseq p1 p2 f = pbind p1 (\x -> pbind p2 (\y -> presult ( f (x, y))))

psat :: (Char -> Bool) -> Parser Char
psat f = pbind pitem (\c -> if f c then presult c else pzero)

pchar :: Char -> Parser Char
pchar c = psat (\c' -> c == c')

plower :: Parser Char
plower = psat isLower

(<|>) :: Parser a -> Parser a -> Input -> Outcome a
(<|>) p1 p2 i = let o = p1 i in
                case o of
                     Success _ _ -> o
                     Failure p r -> let o' = p2 i in
                                    case o' of
                                         Success _ _   -> o'
                                         Failure p' r' -> Failure p r

pfun :: Parser a -> (a -> b) -> Input -> Outcome b
pfun p f i = case p i of
                  Success a i' -> Success (f a) i'
                  Failure p r  -> Failure p r

(|>>) :: Parser a -> (a -> b) -> Parser b
(|>>) p f = pfun p f

pfresult :: Parser a -> b -> Parser b
pfresult p x = pbind p (\v -> presult x)

pleft :: Parser a -> Parser b -> Parser a
pleft l r = pbind l (\x -> pfresult r x)

pright :: Parser a -> Parser b -> Parser b
pright l r = pbind l (\_ -> r)

pbetween :: Parser a -> Parser b -> Parser c -> Parser c
pbetween l r m = pright l (pleft m r)

pmany0 :: Parser a -> Input -> Outcome [a]
pmany0 p i = let pm0 xs i = case p i of
                                 Failure p r -> Success xs i
                                 Success a i'-> pm0 (a:xs) i'
             in  case pm0 [] i of
                      Success xs i' -> Success (reverse xs) i'
                      Failure p r   -> Failure p r

pmany1 :: Parser a -> Parser [a]
pmany1 p = pseq p (pmany0 p) (\(x,xs) -> x:xs)

pbparens :: Parser a -> Parser a
pbparens = pbetween (pchar '(') (pchar ')')
