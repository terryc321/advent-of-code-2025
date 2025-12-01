

module Main where

-- what exactly does the Prelude import by default
-- can we not import Prelude by default ?

import qualified System.Directory as SD
import qualified Text.Printf as P

-- we can see the type of printf procedure in Text.Printf module
-- :t P.printf
-- SD.getCurrentDirectory
-- SD.getHomeDirectory
-- Text.Printf.printf
-- putStrLn dir 
-- putStrLn "Current directory is "
-- 123
-- we need explicit say 123 is an Int fixnum ? and not a BigInteger Integer

data Turn = R Int | L Int
  deriving (Show, Eq)

parseTurn :: String -> Maybe Turn
parseTurn (d:ds)
  | d == 'R' = Just (R (read ds))
  | d == 'L' = Just (L (read ds))
parseTurn _ = Nothing

--- (1::Int) : [1,2,3]
--- 
--- turn d n c

describe_turn (R n) = putStrLn ("Turn right " ++ show n)
describe_turn (L n) = putStrLn ("Turn left " ++ show n)

process (x:xs) =  do
  describe_turn x 
  process xs

process [] = do
  putStrLn ""

process2 f = do
  print f
  process f         
 

main :: IO ()
main = do
    contents <- readFile "../input.txt"
    let linesOfFile = lines contents
        parsedTurns = mapM parseTurn linesOfFile  -- mapM :: [Maybe Turn] -> Maybe [Turn]
    case parsedTurns of
        Just turns -> process2 turns
        Nothing    -> putStrLn "Failed to parse some line"
    putStr "Ok."
    
    
        
