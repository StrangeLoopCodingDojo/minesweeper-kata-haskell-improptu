module Data.String.Strip   where

import Data.Char
import qualified Data.Map as Map

data GameState = Playing | Victory deriving (Eq, Show)
data Board = Board Int Int (Map.Map Point Cell)
data Point = Point Int Int deriving (Ord, Eq)
data CellContent = Empty Int | Mine deriving (Eq, Show)
data Cell = Cell CellContent Bool deriving (Eq, Show)

isEmpty :: Board -> Int -> Int -> Bool
isEmpty (Board _ _ m) x y  = isPointEmpty m (Point x y)

isFlagged :: Board -> Int -> Int -> Bool
isFlagged b x y  = let (Cell _ t) = cellAt b x y in t

isPointEmpty :: (Map.Map Point Cell) -> Point -> Bool
isPointEmpty m p = not $ Map.member p m

cellAt :: Board -> Int -> Int -> Cell
cellAt (Board _ _ m) x y =
  cellAtPoint m (Point x y)

cellAtPoint :: (Map.Map Point Cell) -> Point -> Cell
cellAtPoint m p =
  case Map.lookup p m of
    Just c -> c
    otherwise -> Cell (Empty (-1)) False


countMines :: (Map.Map Point Cell) -> Int -> Int -> Int
countMines m x y =
   let points = [Point (x+1) y]
       empties = map (\p -> isPointEmpty m p) points in
    length $ filter not empties

countCorrectCells :: Board -> Int
countCorrectCells (Board _ _ m) =
  let cells = Map.elems m
      corrects = map  isCellCorrect cells in
  length $ filter id corrects

isCellCorrect :: Cell -> Bool
isCellCorrect (Cell c f) =
  case c of
    Empty n -> n /= (-1)
    Mine -> f



mkBoard :: Int -> Int -> Board
mkBoard w h = Board w h Map.empty

addMine :: Board -> Int -> Int -> Board
addMine (Board h w m) x y =
  let p = Point x y in
  Board h w (Map.insert p (Cell Mine False) m)

select :: Board -> Int -> Int -> Board
select (Board h w m) x y =
  let p = Point x y in
  Board h w (Map.insert p (Cell (Empty $ countMines m x y) False) m)

toggleFlag :: Board -> Int -> Int -> Board
toggleFlag b@(Board h w m) x y =
  let p = Point x y
      (Cell value flag) = cellAt b x y in
  Board h w (Map.insert p (Cell value $ not flag) m)

getStatus :: Board -> GameState
getStatus b@(Board h w m) =
  let numCells = h * w
      count = countCorrectCells b in
  case count == numCells of
    True -> Victory
    False -> Playing
