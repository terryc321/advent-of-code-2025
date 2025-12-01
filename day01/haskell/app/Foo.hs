

module Foo where

-- what exactly does the Prelude import by default
-- can we not import Prelude by default ?

import qualified System.Directory as SD
import qualified Text.Printf as P


data Turn = R Int | L Int
  deriving (Show, Eq)

-- how would do this pattern matching ??
-- turn (R 0) d ct = (d,ct)
-- turn (R n) d ct = 
-- turn (L n) d ct = 

poo = "yes"



        
