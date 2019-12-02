-- CS3210 - Principles of Programming Languages - Fall 2019
-- Programming Assignment 02 - A Sudoku Solver
-- Author(s): Tanner Marsden, Ryan McCullough
-- Date: November 5, remember remember the fifth of November

import Data.List
import System.Environment
import System.IO

type Sequence = [Int]
type Board    = [Sequence]

-- ***** HELPER FUNCTIONS *****

-- name: toInt
-- description: converts given parameter to integer
-- input: a string
-- output: the string converted to integer
-- example: toInt "123" returns 123
toInt :: [Char] -> Int
toInt s = read s :: Int

-- name: toIntList
-- description: converts given parameter to a sequence of integers (one digit at a time)
-- input: a string
-- output: the string converted into a sequence of integers
-- example: toIntList "123" returns [1, 2, 3]
toIntList :: [Char] -> Sequence
toIntList s = [ toInt [c] | c <- s ]

-- ***** GETTER FUNCTIONS *****

-- TODO #1
-- name: getBoard
-- description: convert given string to a sudoku board
getBoard :: [Char] -> Board
getBoard s = [ toIntList x | x <- lines s]

-- TODO #2
-- name: getNRows
-- description: given a board, return its number of rows
getNRows :: Board -> Int
getNRows board = length board


-- TODO #3
-- name: getNCols
-- description: given a board, return its number of columns or 0 if rows do not have the same number of columns
--getNCols :: Board -> Int
equal :: [Int] -> Bool 
equal xs = and $ map (== head xs) (tail xs)
getNCols :: Board -> Int
getNCols board = 
    if (equal . map length) board then (map length board) !! 0
    else 0

-- TODO #4
-- name: getBox
-- description: given a board and box coordinates, return the correspondent box as a sequence
--getBox :: Board -> Int -> Int -> Sequence
getBox :: Board -> Int -> Int -> Sequence
getBox board i j = [board !! x !! y | y <- [j*3 .. j*3+2], x <- [i*3 .. i*3+2]]

-- TODO #5
-- name: getEmptySpot
-- description: given a board, return the first location that is empty (i.e., it has zero), if one exists; OK to assume that you will only call this function when you know that there is an empty spot
getEmptySpot :: Board -> (Int, Int)
getEmptySpot board = head [ (i,j) | i <- [0..8], j <- [0..8], board !! i !! j == 0]

-- TODO #6
-- name: isGridValid
-- description: given a board, return True/False depending whether the given board constitutes a valid grid (i.e., #rows = #cols = 9) or not
isGridValid :: Board -> Bool
isGridValid board = getNRows board == 9 && getNCols board == 9

-- TODO #7
-- name: isSequenceValid
-- description: return True/False depending whether the given sequence is valid or not, according to sudoku rules
-- input: a sequence of digits from 0-9
-- output: True/False
-- example 1: isSequenceValid [5,3,0,0,7,0,0,0,0] yields True
-- example 2: isSequenceValid [5,3,0,5,7,0,0,0,0] yields False
-- hint: build a list with the digits from the given sequence that are different than zero; then determine whether there are digits that repeats in the created list
isSequenceValid :: Sequence -> Bool
isSequenceValid seq = 
  length (nub noZero) == length noZero
  where noZero = filter (/=0) seq

-- TODO #8
-- name: areRowsValid
-- description: return True/False depending whether ALL of the row sequences are valid or not
-- input: a board
-- output: True/False
-- hint: use list comprehension and isSequenceValid
areRowsValid :: Board -> Bool
areRowsValid board = all isSequenceValid board

-- TODO #9
-- name: areColsValid
-- description: return True/False depending whether ALL of the col sequences are valid or not
areColsValid :: Board -> Bool
areColsValid b = areRowsValid (transpose b)

-- TODO #10
-- name: areBoxesValid
-- description: return True/False depending whether ALL of the box sequences are valid or not
-- input: a board
-- output: True/False
-- hint: use list comprehension, isSequenceValid, and getBox
areBoxesValid :: Board -> Bool
areBoxesValid board = all isSequenceValid [getBox board x y | x <- [0..2], y <- [0..2]]

-- TODO #11
-- name: isValid
-- description: return True/False depending whether the given board is valid sudoku configuration or not
-- isValid :: Board -> Bool
isValid :: Board -> Bool
isValid board = isGridValid board && areRowsValid board && areColsValid board && areBoxesValid board

-- TODO #12
-- name: isCompleted
-- description: return True/False depending whether the given board is completed or not; a board is considered completed if there isn't a single empty cell
-- hint: use list comprehension and the elem function
isCompleted :: Board -> Bool
isCompleted board = not (or [elem 0 (board !! x) | x <- [0 .. length board - 1]])

-- TODO #13
-- name: isSolved
-- description: return True/False depending whether the given board is solved or not; a board is solved if it is completed and still valid
isSolved :: Board -> Bool
isSolved board = isValid board && isCompleted board 

-- ***** SETTER FUNCTIONS *****

-- TODO #14
-- name: setRowAt
-- description: given a sequence, an index, and a value, writes the value at the index location, returning a new sequence, but only if the original value at the specified location is empty; otherwise, return the original sequence unchanged
setRowAt :: Sequence -> Int -> Int -> Sequence
setRowAt seq x y = take x seq ++ [y] ++ drop (x + 1) seq

-- TODO #15
-- name: setBoardAt
-- description: given a board, two indexes i and j (representing coordinates), and a value, writes the value at the (i, j) coordinate, returning the new board, but only if the original value at the specified location is empty; otherwise, return the original board unchanged
setBoardAt :: Board -> Int -> Int -> Int -> Board
setBoardAt board i j x = take i board ++ [setRowAt (board!!i) j x] ++ drop (i+1) board

-- TODO #16
-- name: buildChoices
-- description: given a board and a two indexes i and j (representing coordinates), generate ALL possible boards, replacing the cell at (i, j) with ALL possible digits from 1 to 9; OK to assume that the cell at (i, j) is empty
-- hint: use list comprehension and the function setBoardAt
buildChoices :: Board -> Int -> Int -> [Board]
buildChoices board num1 num2 = [setBoardAt board num1 num2 x | x <- [1 .. 9]]

-- name: solve
-- description: given a board, finds all possible solutions (note that dead ends or invalid intermediate solutions are listed as empty boards)
-- input: a board
-- output: a list of boards from the original board
-- note: this code is given to you (just uncomment it when you are ready to test the solver)
solve :: Board -> [Board]
solve board
  | isSolved board = [board]
  | isCompleted board = [[[]]]
  | not (isValid board) = [[[]]]
  | otherwise = concat [ solve choice | choice <- buildChoices board i j ]
  where
    emptySpot = getEmptySpot board
    i = fst emptySpot
    j = snd emptySpot


-- program starts here
main = do
  -- TODO #17: validate the command-line and get the file name containing the board
  args <- getArgs
  let fileName = head args

  -- TODO #18: read the contents of the board file into a string
  contents <- readFile fileName

  -- TODO #19: create a board from the string board (hint: use getBoard)
  let board = getBoard contents

  -- TODO #20: use solve to find the solutions, disconsidering the ones that are [[]]
  let good = solve board
  let filteredSol = filter(/= [[]]) good

  -- TODO #21: print the solutions found
  print filteredSol
  print "Done!"