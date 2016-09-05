module Main where

import System.Random  -- for randoms
import System.IO      -- for hFlush
import Control.Monad

type Row = [Int]
type Guess = Row
type Solution = Row

data Player = H | C
    deriving (Show, Eq)

colors, width :: Int
colors = 6
width  = 4

-- A function that indicates places on which you have to work:
tODO :: a -> a
tODO = id

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    s <- generateSolution
    putStr "Is the player a (H)uman or a (C)omputer? "
    p <- parsePlayer <$> getLine

    if p == H
     then loop s p [] []
     else do
         putStrLn ("(" ++ show s ++ " = solution)")
         loop s p [] [ [q, w, e, r] | q <- [1 .. colors]
                                       , w <- [1 .. colors]
                                       , e <- [1 .. colors]
                                       , r <- [1 .. colors]]

-- The loop function is supposed to perform a single interaction. It
-- reads an input, compares it with the solution, produces output to
-- the user, and if the guess of the player was incorrect, loops.
loop :: Solution ->
        Player   ->
        [Int]    ->   -- [b, w] for last guess
        [Guess]  ->   -- last set of guesses, its head is the last guess
        IO ()
loop s p score gs =
    do
        gs' <- input p score gs     -- if p=H then gs'=[g']
        let c@(score', correct) = check s (head gs')
        putStrLn $ report c
        unless correct $ loop s p score' gs'

black, white :: Solution -> Guess -> Int
black solution guess = sum [1 | (q,i) <- zip solution [0..], q == guess !! i]
white solution guess = sum [1 | b <- weakMatch solution' guess', b]
    where
        tmp = exactMatch solution guess
        solution' = [x | (x,b) <- zip solution tmp, not b]
        guess' = [x | (x,b) <- zip guess tmp, not b]

exactMatch :: Eq a => [a] -> [a] -> [Bool]
exactMatch [] _ = []
exactMatch (x:xs) [] = []
exactMatch (x:xs) (y:ys) = (x == y) : exactMatch xs ys

weakMatch :: Eq a => [a] -> [a] -> [Bool]
weakMatch [] _ = []
weakMatch _ [] = []
weakMatch (x:xs) l
    | x `elem` l = True : weakMatch xs (delFst x l)
    | otherwise  = False : weakMatch xs l
        where
            delFst x [] = []
            delFst x (y:ys)
                | x == y = ys
                | otherwise = y : delFst x ys

check :: Solution -> Guess -> ([Int], -- number of black and white points,
                               Bool)  -- all-correct guess?
check solution guess =
    let black' = black solution guess
    in ([black', white solution guess], black' == width)

-- report is supposed to take the result of calling check, and
-- produces a descriptive string to report to the player.
report :: ([Int], Bool) -> String
report ([blackScore, whiteScore], correct)
    | correct = "Yay!! You're a mastermind."
    | otherwise = show blackScore ++ " black,\n"
               ++ show whiteScore ++ " white."

input :: Player
      -> [Int]      -- [black, white] for last guess
      -> [Guess]    -- last subset of guesses
      -> IO [Guess] -- new subset of guesses (its head is the new guess)
input H _ _= do
    putStr "? "
    hFlush stdout
    g <- (map readInt . words) <$> getLine
    return [g]
input C score gs =
    let gs' = makeGuess score gs
    in putStrLn ("? " ++ show (head gs')) >> return gs'

{- The computer player starts with a prefixed guess g0 taken from the set gs0
of all possible guesses. The next guess g1 is done as follows: with the
information on blacks b and whites w for g0 it takes the subset of gs0 (in this
case the set of all possible answers) corresponding to the guesses which differ
exactly b blacks and w whites from g0, since we know this must hold for the correct
solution. As g1 we choose the head of gs1. In the step n we take a subset of gsn,
so it is guaranteed we don't make the same guess twice.
-}
makeGuess :: [Int]       -- [b, w] for last guess
          -> [Guess]     -- last guess subset
          -> [Guess]     -- new guess subset
makeGuess [] gs = gs
makeGuess [b, w] gs  = gs'
    where
        g = head gs   -- old guess
        gs' = [x | x <- gs, black g x == b, white g x == w] -- new set of guesses

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
valid guess =
    length guess == width &&
    all (\x -> x > 0 && x <= colors) guess

parsePlayer :: String -> Player
parsePlayer "C" = C
parsePlayer "H" = H
parsePlayer _   = error "parsePlayer: no parse"

choosePlayer :: IO Player
choosePlayer = do
    s <- getLine
    return (parsePlayer s)

-- The following function is given. It generates a random solution of the
-- given width, and using the given number of colors.
generateSolution :: IO [Int]
generateSolution =
  do
    g <- getStdGen
    let rs = take width (randoms g)
    return (map ((+1) . (`mod` colors)) rs)
