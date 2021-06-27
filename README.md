# Mastermind #

## What is this?
A mastermind game in Haskell developed during the [Munihac 2016](http://munihac.de/).

## Installation and requirements
Clone this repository.

You only need [`GHCi`](http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html) to run the game on your terminal.

## Run the game
In your terminal, go to the directory where you stored `MasterMind.hs` and start `GHCi`. Start the game by typing `main`.

There are two options, (H)uman and (C)omputer. If you select C then the computer will play on its own and will 
always win after a few guesses. If you select H then you can play yourself. You need to guess a sequence of four numbers
between 1 and 6 (a single number might appear several times). Enter your guesses by typing the numbers separated by whitespaces.
The computer will then reveal the number of black guesses (correct number in the correct position) and the number of
white guesses (correct number in the wrong position) and with this information you can guess again. The number of guesses
is actually not limited, so sooner or later you will either win or end up in an infinite loop.
Below you see a sample (human) game:
```
Is the player a (H)uman or a (C)omputer?
> H
> 1 2 3 4
2 black,
0 white.
> 1 2 1 1
0 black,
0 white.
> 5 5 3 4
3 black,
0 white.
> 6 5 3 4
Yay!! You're a mastermind.
``` 
If that's too easy you can change in the code the parameters
```
colors = 6  -- How many different numbers
width  = 4  -- Length of the sequence
``` 
to whatever you like.
