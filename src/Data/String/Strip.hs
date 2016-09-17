module Data.String.Strip   where

import Data.Char
import qualified Data.Map as Map

data Board = Board Int Int (Map.Map Point Cell)
data Point = Point Int Int deriving (Ord, Eq)
data Cell = Empty Int | Mine | Flagged Cell deriving (Eq, Show)

isEmpty :: Board -> Int -> Int -> Bool
isEmpty (Board _ _ m) x y  = isPointEmpty m (Point x y)

isFlagged :: Board -> Int -> Int -> Bool
isFlagged b x y  =
  case checkValue b x y of
   Flagged _ -> True
   otherwise -> False

isPointEmpty :: (Map.Map Point Cell) -> Point -> Bool
isPointEmpty m p = not $ Map.member p m

checkValue :: Board -> Int -> Int -> Cell
checkValue (Board _ _ m) x y =
  case Map.lookup (Point x y) m of
    Just c -> c
    otherwise -> Empty (-1)


countMines :: (Map.Map Point Cell) -> Int -> Int -> Int
countMines m x y =
   let points = [Point (x+1) y]
       empties = map (\p -> isPointEmpty m p) points in
    length $ filter not empties

mkBoard :: Int -> Int -> Board
mkBoard w h = Board w h Map.empty

addMine :: Board -> Int -> Int -> Board
addMine (Board h w m) x y =
  let p = Point x y in
  Board h w (Map.insert p Mine m)

select :: Board -> Int -> Int -> Board
select (Board h w m) x y =
  let p = Point x y in
  Board h w (Map.insert p (Empty $ countMines m x y) m)

flag :: Board -> Int -> Int -> Board
flag b@(Board h w m) x y =
  let p = Point x y in
  Board h w (Map.insert p (Flagged $ checkValue b x y) m)
