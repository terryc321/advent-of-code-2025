
module Main

import System.File
import Parser  -- import your parser module

main : IO ()
main = do
  Right text <- readFile "input.txt"
    | Left err => putStrLn ("Error: " ++ show err)

  case parse instructions text of
    Left e  => printLn e
    Right v => printLn v

