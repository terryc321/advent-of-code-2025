
module Parser

import System.File
import Text.Parser
import Text.Parser.Core

public export
data Turn = L | R
  deriving Show

-- Parse a single L or R
export
turn : Parser Turn
turn = do
  c <- anyChar
  case c of
    'L' => pure L
    'R' => pure R
    _   => fail "Expected L or R"

-- Parse a positive integer
export
int : Parser Int
int = natural

-- Parse one instruction: L42 or R7
export
instruction : Parser (Turn, Int)
instruction = do
  t <- turn
  n <- int
  pure (t, n)

-- Parse whole file: many lines of instructions
public export
instructions : Parser (List (Turn, Int))
instructions = many (instruction <* skipSpaces)
