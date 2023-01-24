{-# LANGUAGE FlexibleInstances #-}
-- Brainfuck compiler
import Data.Char

-- Tape / Pointer

type DataTape = [Int]
type Pointer = Int


-- Data ops

data BFop = 
    MoveRight | 
    MoveLeft  | 
    Increment | 
    Decrement | 
    Output    | 
    Input     | 
    Loop [BFop] 
    deriving (Show, Eq, Ord) 



-- Parser

parser :: String -> [BFop]
parser "" = []
parser (x:xs)
 | x == '>' = MoveRight : parser xs
 | x == '<' = MoveLeft : parser xs
 | x == '+' = Increment : parser xs
 | x == '-' = Decrement : parser xs
 | x == '.' = Output : parser xs
 | x == ',' = Input : parser xs
 | x == '[' = looping xs 
 | x == ']' = parser xs
 where looping ys = let loop = break (==']') ys in (Loop (parser $ fst loop)): parser (snd loop)    



--Interpreter

-- Increment cell at pointer
incCell datatape pointer = let len = (length datatape) - 1 
 in [ if pointer == n then succ (datatape !! n) else (datatape !! n) | n <- [0..len] ] 

-- Decrement cell at pointer
decCell datatape pointer = let len = (length datatape) - 1 
 in [ if pointer == n then pred (datatape !! n) else (datatape !! n) | n <- [0..len] ]

evaluator :: DataTape -> [BFop] -> Pointer -> DataTape
evaluator datatape [] _ = datatape
evaluator datatape (op:ops) pointer = case op of
    MoveRight -> evaluator datatape ops (succ pointer)
    MoveLeft -> evaluator datatape ops (pred pointer)
    Increment -> evaluator (incCell datatape pointer) ops pointer
    Decrement -> evaluator (decCell datatape pointer) ops pointer
    Loop ls -> evaluator (looping datatape ls pointer) ops pointer
    where looping xs ops' p = if xs !! p > 0 
                              then looping (evaluator xs ops' p) ops' p 
                              else xs

