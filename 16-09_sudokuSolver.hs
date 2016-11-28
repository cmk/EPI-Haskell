--16-9 Create a Sudoku solver

import Data.List (sort)
import Control.Monad (guard)
type Element = (Int, Int)
type Board = [Element]

format :: Board -> Board
format elts = 
  let elts' = sort elts
      foo formatted new = [new] ++ filler  ++ formatted
        where filler = [(0,0) | i <- [1.. (fst new) - (fst $ head formatted)-1]]  
  in foldl foo [head elts'] (tail elts')

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs
    
toString :: Board -> String
toString board =
  let ary = fmap (++['\n']) $ chunks 9 $ fmap (head . show . snd) (format board)
  in reverse $ foldl (++) "" ary

showBoard :: Board -> IO ()
showBoard board = putStr $ toString board

board :: Board
board = [(0,5),(1,3),(4,7),(9,6),(12,1),(13,9),(14,5),
    (19,9),(20,8),(25,6),(27,8),(31,6),(35,3),(36,4),
    (39,8),(41,3),(44,1),(45,7),(49,2),(53,6),(55,6),
    (60,2),(61,8),(66,4),(67,1),(68,9),(71,5),(76,8),(79,7),(80,9)]
        
row :: Element -> Int
row e = quot (fst e) 9

col :: Element -> Int
col e =  mod (fst e) 9

tile :: Element -> Int
tile e = let
  tileX = quot (col e) 3
  tileY = quot (row e) 3
  in tileX + 3*tileY
  
checkX :: Board -> Element -> Bool
checkX board next = let
  check bool e = bool && ((row e == row next && snd e /= snd next) || row e /= row next)
  in foldl check True board

checkY :: Board -> Element -> Bool
checkY board next = let 
  check bool e = bool && ((col e == col next && snd e /= snd next) || col e /= col next)
  in foldl check True board
  
checkT :: Board -> Element -> Bool
checkT board next = let
  check bool e = bool && ((tile e == tile next && snd e /= snd next) || tile e /= tile next)
  in foldl check True board

notPlayed :: Board -> Int -> Bool
notPlayed board index = notElem index $ fmap fst board

isLegal :: Board -> Element -> Bool
isLegal board next = checkX board next && checkY board next && checkT board next

sudokuSolve :: Board -> [Board]
sudokuSolve problem = let
  indices = filter (notPlayed problem) [0..80]
  sudokuIter indices = case indices of
    [] -> [problem]
    index : tail -> do
      board <- sudokuIter tail
      k <- [1..9]
      guard $ isLegal board (index,k)
      return  $ (index,k) : board 
  in sudokuIter indices

