
module Fun

-- Booleans 
t : Bool
t = True

f : Bool
f = False

-- fixnums 
foo : Int 
foo = 3 

-- foo + foo
-- int to String conversion 
-- read "3" :: Int
-- show 3 
-- "asdf" + (show foo)

-- simple addition for fixnums
add1 : Int -> Int
add1 x = x + 1 
-- what is the largest Fixnum ?
-- what is the smallest Fixnum ?
-- can we show wrap around behaviour
-- add 1 to largest fixnum
-- subtract 1 to largest negative fixnum 

-- power 2 raised to 3 gives float ?
p23 = 2 `pow` 3

-- Int
i1 : Int
i1 = 123

-- Integer
i2 : Integer
i2 = 123123123123123123123123123123123

-- Float -- no floats
-- f : Float
-- f = 3.14

-- Double
d : Double
d = 3.14

-- Char
ch : Char 
ch = 'A'

-- List 
xs : List Int 
xs = [1,2,3]
ys = 2 :: xs

-- String is also a list
str : String
str = "alpha"

-- string reverse
str2 = reverse str
-- char at nth 

-- functions on string 
-- see Data.String words lines unlines 
-- words undefined in idris ?

-- wds : List String
-- wds = words "alpha beta charlie delta echo foxtrot"


-- haskell "asdf" == ['a','s','d','f'] is True

-- we cannot equate types ? i thought we could talk about types ?
-- are two types equal ?
-- clearly cannot be unless they are an alias for each other
-- in which case we would know right ?

-- lets do the factorial function 
-- mutually recursive - we are okay since we cannot redefine the procedure elsewhere 
-- unlike ml ocaml - we can redefine so extort to and f2 and f3 and f4 ...

-- how would i say call factorial2 provide r then provide n 
-- in general have positional arguments 

factorial2 : Integer -> Integer -> Integer 
factorial2 n r =  if n == 1 then r else factorial2 (n - 1) (r * n)

factorial : Integer -> Integer 
factorial 1 = 1
factorial 2 = 2
factorial n = factorial2 n 1


