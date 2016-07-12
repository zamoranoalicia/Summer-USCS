module MasterMind where

import System.Random  -- for randoms
import System.IO      -- for hFlush
import Data.List (sort, intersect)
import Data.Char (digitToInt)

import qualified Data.MultiSet as MultiSet

type Row = [Int]
type Guess = Row
type Solution = Row

colors = 6
width  = 4

-- A function that indicates places on which you have to work:
tODO :: a -> a
tODO = id

-- This is the complete main function. It just initializes the
-- game by generating a solution and then calls the game loop.
main :: IO ()
main =
  do
    s <- generateSolution -- initialization
    putStrLn "--------------------------------------------"
    putStrLn "************* MASTER MIND ******************"
    putStrLn "--------------------------------------------"
    loop s  -- game loop

-- The following function is given. It generates a random solution of the
-- given width, and using the given number of colors.
generateSolution =
  do
    g <- getStdGen
    let rs = take width (randoms g)
    return (map ((+1) . (`mod` colors)) rs)

-- The loop function is supposed to perform a single interaction. It
-- reads an input, compares it with the solution, produces output to
-- the user, and if the guess of the player was incorrect, loops.
loop :: Solution -> IO ()
loop solution =
  do
    guess <- input            -- read (and parse) the user input
    let (black, white, result) = check solution guess
    putStrLn $ report (black, white, result)
    if result then return () else loop solution


black, white :: Solution -> Guess -> Int
black xs ys = (length.filter id) (zipWith (==) xs ys)

white solution guess = countWhite' $ unzip $ filter (uncurry (/=)) $ (zip solution guess)

countWhite' :: ([Int],[Int]) -> Int
countWhite' (xs,ys) = let xm = MultiSet.fromList xs
                          ym = MultiSet.fromList ys
                      in MultiSet.size $ MultiSet.intersection  xm ym

check :: Solution -> Guess -> (Int,   -- number of black points,
                               Int,   -- number of white points
                               Bool)  -- all-correct guess?
check solution guess = (black solution guess,
                        white solution guess,
                        length solution == black solution guess)

-- report is supposed to take the result of calling check, and
-- produces a descriptive string to report to the player.
report :: (Int, Int, Bool) -> String
report (black, white, correct) =  show black ++ " black " ++ show white ++" white "++ if correct then "\nYou Win!!!." else " "

-- The function input is supposed to read a single guess from the
-- player. The first three instructions read a line. You're supposed
-- to (primitively) parse that line into a sequence of integers.
input :: IO Guess
input =
  do
    putStr "? "
    hFlush stdout -- ensure that the prompt is printed
    l <- getLine
    let guess = map digitToInt l
    return guess

-- The following function |readInt| is given and parses a single
-- integer from a string. It produces |-1| if the parse fails. You
-- can use it in |input|.
readInt :: String -> Int
readInt x =
  case reads x of
    [(n, "")] -> n
    _         -> -1

-- The function valid tests a guess for validity. This is a bonus
-- exercise, so you do not have to implement this function.
valid :: Guess -> Bool
valid guess = tODO True
