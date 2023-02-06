
-- Brainfuck Interpreter 
import Data.Char
import Data.List

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
incCell :: DataTape -> Int -> DataTape
incCell datatape pointer = let len = (length datatape) - 1 
 in [ if pointer == n then succ (datatape !! n) else (datatape !! n) | n <- [0..len] ] 

-- Decrement cell at pointer
decCell :: DataTape -> Int -> DataTape
decCell datatape pointer = let len = (length datatape) - 1 
 in [ if pointer == n then pred (datatape !! n) else (datatape !! n) | n <- [0..len] ]

-- Insert in the cell at
insertCell datatape element pointer = 
    let split = splitAt pointer datatape
    in (fst split) ++ [element] ++ tail (snd split)

eval :: DataTape -> [BFop] -> Pointer -> IO DataTape
eval datatape [] _ = return datatape
eval datatape (op:ops) pointer = case op of
    Output -> do 
        print $ chr $ (head . drop pointer) datatape
        result <- eval datatape ops pointer  
        return result 
    MoveRight -> eval datatape ops (succ pointer)
    MoveLeft -> eval datatape ops (pred pointer)
    Increment -> eval (incCell datatape pointer) ops pointer
    Decrement -> eval (decCell datatape pointer) ops pointer
    Loop array -> do 
        loop <- (looping datatape array pointer)
        eval loop ops pointer
    where looping :: DataTape -> [BFop] -> Pointer -> IO DataTape
          looping dt ops' p = if dt !! p > 0 
                              then do 
                                evaluation <- eval dt ops' p
                                looping evaluation ops' p 
                              else 
                                return dt

main = do
    instructions <- getLine
    let parse = parser instructions
        bf = eval [0,0,0,0,0,0,0,0] parse 0
    return bf





