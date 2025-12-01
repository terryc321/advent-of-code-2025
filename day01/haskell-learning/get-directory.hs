

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



main :: IO ()
main = do {
  dir <- SD.getCurrentDirectory ;
  P.printf "Current directory is %s %d\n " dir (3 ::Int) ;
  P.printf "some values %d %d\n" (1 ::Int) (2 :: Int) ;
  }



