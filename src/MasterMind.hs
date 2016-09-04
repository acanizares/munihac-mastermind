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

-- This is the complete main function. It just initializes the
-- game by generating a solution and then calls the game loop.

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    s <- generateSolution
    putStr "Is the player a (H)uman or a (C)omputer? "
    p <- parsePlayer <$> getLine

    if p == H
     then tODO(return())
     else loop' s p [] [ [q, w, e, r] | q <- [1 .. colors]
                                , w <- [1 .. colors]
                                , e <- [1 .. colors]
                                , r <- [1 .. colors]]
loop' s p g gs = print s >> loop s p g gs -- debugging relay function for showing the solution at the beginning

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

-- The loop function is supposed to perform a single interaction. It
-- reads an input, compares it with the solution, produces output to
-- the user, and if the guess of the player was incorrect, loops.
{-loop :: Solution -> IO ()
loop s =
  do
    i <- input            -- read (and parse) the user input
    let c@(b, w, correct) = check s i
    putStrLn $ report c
    if not correct
        then loop s
        else putStrLn "Done"
-}
loop :: Solution ->
        Player   ->
        Guess    ->   -- last guess (l.g.)
        [Guess]  ->   -- guesses yielding same b,w as l.g. excluding l.g.
        IO ()
loop s H _ _ =
    do
      [i] <- input H (-1) (-1) [] [[]]            -- read (and parse) the user input
      let c@(b, w, correct) = check s i
      putStrLn $ report c
      unless correct $ loop s H [] [[]]
loop s C g gs
    | null g =
        do
            (g':gs') <- input C (-1) (-1) g gs
            loop s C g' gs'
    | otherwise =
        do
            let c@(b, w, correct) = check s g
            putStrLn $ report c
            unless correct $ do
                (g':gs') <- input C b w g gs
                loop s C g' gs'

--    (g':gs') <- input p s g gs

{-    let c = check s g'
     in case c of
        (_, _, True) -> void (putStrLn (report c))
        _            -> putStrLn (report c) >> loop s p g' gs'
-}
black, white :: Solution -> Guess -> Int
--black solution guess = sum (map boolToInt (exactMatch solution guess))
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

check :: Solution -> Guess -> (Int,   -- number of black points,
                               Int,   -- number of white points
                               Bool)  -- all-correct guess?
check solution guess =
    let black' = black solution guess
    in (black', white solution guess, black' == width)

-- report is supposed to take the result of calling check, and
-- produces a descriptive string to report to the player.
report :: (Int, Int, Bool) -> String
report (blackScore, whiteScore, correct)
    | correct = "Yay!! You're a mastermind."
    | otherwise = show blackScore ++ " black,\n"
               ++ show whiteScore ++ " white."

-- The function input is supposed to read a single guess from the
-- player. The first three instructions read a line. You're supposed
-- to (primitively) parse that line into a sequence of integers.
{-input :: IO Guess
input =
  do
    putStr "? "
    hFlush stdout -- ensure that the prompt is printed
    l <- (map readInt .
     words) <$> getLine :: IO [Int]
    return l
-}

input :: Player
      -> Int -- blacks
      -> Int -- whites
      -> Guess   -- last guess (l.g.)
      -> [Guess] -- guesses yielding same b,w as l.g. excluding l.g.
      -> IO [Guess]
input H _ _ _ _= do
    putStr "? "
    hFlush stdout
    i <- (map readInt . words) <$> getLine
    return [i]
input C (-1) (-1) [] gs =
    let sol = gs
    in putStrLn ("? " ++ show (head sol)) >> return sol
input C b w g gs =
    let sol = makeGuess b w g gs
    in putStrLn ("? " ++ show (head sol)) >> return sol

-- Black and white score and a history of all the guesses.

-- The algorithm to guess the solution shouldn't use the solution!
makeGuess b w g gs = g':gs'
    where
        --a = [x | (x, (b, w, _)) <- zip gs (map (check solution) gs)]
        g' = head a
        gs' = tail a
        a = [x | x <- gs, black g x == b, white g x == w]


{-    [x | (x, (b, w, _)) <- zip gs (map (check solution) gs)]
    let (b, w, _) = check solution g
        in [x | (x, (b, w, _)) <- zip gs (map (check solution) gs)]
-}


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
