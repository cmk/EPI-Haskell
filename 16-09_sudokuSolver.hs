--16-9 Create a Sudoku solver
import Data.Set (Set)
import Data.List (sort)

type Element = (Int, Int)
type Board = [Element]

format :: Board -> Board
format elts = 
  let elts' = sort elts
      foo formatted new = [new] ++ filler  ++ formatted
        where filler = [(0,0) | i <- [1.. (fst new) - (fst $ head formatted)-1]]  
  in foldl foo [head elts'] (tail elts')

--chunks n = takeWhile (not.null) . unfoldr (Just . splitAt n)
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs
    
toString :: Board -> String
toString board =
  let ary = fmap (++['\n']) $ chunks 9 $ fmap (head . show . snd) (format board)
  in foldl (++) "" ary

showBoard :: Board -> IO ()
showBoard board = putStr $ toString board

board :: Board
board = [(0,5),(1,3),(4,7),(9,6),(12,1),(13,9),(14,5),
    (19,9),(20,8),(25,6),(27,8),(31,6),(35,3),(36,4),
    (39,8),(41,3),(44,1),(45,7),(49,2),(53,6),(55,6),
    (60,2),(61,8),(66,4),(67,1),(68,9),(71,5),(76,8),(79,7),(80,9)]
        
row :: Element -> Int
row e = undefined

column :: Element -> Int
column e = undefined

tile :: Element -> Int
tile e = undefined

checkX :: Board -> Element -> Bool
checkX board next = undefined

checkY :: Board -> Element -> Bool
checkY board next = undefined

checkT :: Board -> Element -> Bool
checkT board next = undefined

notPlayed :: Board -> Int -> Bool
notPlayed board = undefined

isLegal :: Board -> Element -> Bool
isLegal board next = checkX board next && checkY board next && checkT board next

sudokuSolve :: Board -> Set Board
sudokuSolve board = undefined
