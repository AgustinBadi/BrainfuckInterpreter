-- Brainfuck compiler


-- Tape
data Cell = Cell {value :: Int} deriving (Show,Ord,Eq)

instance  Bounded Cell where
 minBound = Cell 0
 maxBound = Cell 256

type DataTape = [Cell]
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
 | x == '[' = Loop (parser $ fst (parseLoop xs)) : (parser $ tail $ snd $ parseLoop xs)
 where parseLoop xs = let loop = break (== ']') xs 
                      in if (snd loop) == "" then error "Syntax error ']' missing" else loop


--Interpreter

-- From a datatape apply some BFoperator (wether Increment or Decrement) at target pointer
-- If the current pointer is equal of target pointer, then apply operator at current cell.
-- Otherwise, add 1 to the current pointer.
modCell :: DataTape -> BFop -> Pointer -> Pointer -> DataTape
modCell [] _ _ _ = []
modCell (d:dt) op startP targetP
 | startP == targetP = operate op d : dt 
 | otherwise = d : (modCell dt op (startP+1) targetP)  
 where operate :: BFop -> Cell -> Cell
       operate op c = if op == Increment then Cell (value c + 1) else Cell (value c -1)


-- let tape1 = take 32 $ repeat Cell 0
-- map (value)  $ modCell tape1 Increment 0 5


interpreterBF :: DataTape -> [BFop] -> Pointer -> DataTape
interpreterBF dt [] _ = dt
interpreterBF dt (op:ops) pointer = case op of
    MoveRight -> interpreterBF dt (ops) (pointer+1)
    MoveLeft  -> interpreterBF dt (ops) (pointer-1)
    Increment -> interpreterBF (modCell dt Increment 0 pointer) (ops) pointer
    Decrement -> interpreterBF (modCell dt Decrement 0 pointer) (ops) pointer









-- Crear un tipo de dato representando los bytes del array (8bits)

{--

-- Crear tipo con las intrucciones de brainfuck
import Data.Char
array :: [Int]
array = take 32 $ repeat 0

--instruction = ""

modCell :: [Int] -> (Int -> Int) -> Int -> [Int]
modCell info op pointer  = 
 let large = length info 
 in [ if i == pointer then op (info !! i) else info !! i | i <- [0,1..(large -1)]]


bf :: String -> [Int] -> Int -> [Int]
bf [] info pointer = info
bf (x:xs) info pointer
 | x == '+' = bf xs (modCell info succ pointer) pointer
 | x == '-' = bf xs (modCell info pred pointer) pointer
 | x == '>' = bf xs info (pointer+1)
 | x == '<' = bf xs info (pointer-1)   


decode xs = map (chr) xs

--}