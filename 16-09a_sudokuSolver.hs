--16-9 Create a Sudoku solver
--this version is taken from Richard Bird's "Pearls of Functional Algorithm Design"

import Data.List ((\\))

type Matrix a = [Row a]
type Row a = [a]
type Grid = Matrix Digit
type Digit = Char
type Choices = [Digit]

digits = ['1'..'9']
blank = (=='0')

solve = filter valid . expand . choices

choices :: Grid -> Matrix Choices
choices = map (map choice)

choice d = if blank d then digits else [d]

expand :: Matrix Choices -> [Grid]
expand = cp . map cp

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = do
  x <- xs
  ys <- cp xss
  return (x : ys)

valid :: Grid -> Bool
valid g = (all nodups (rows g)) && (all nodups (cols g)) && (all nodups (boxes g))

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = all (/= x) xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [a] = [[x] | x <- a]
cols (xs : xss) = zipWith (:) xs (cols xss)

boxes :: Matrix a -> Matrix a
boxes = map ungroup . ungroup . map cols . group . map group

group :: [a] -> [[a]]
group [] = []
group l = x : group xs where (x,xs) = splitAt 3 l 

ungroup :: [[a]] -> [a]
ungroup = concat

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxes . pruneBy cols . pruneBy rows

pruneBy f = f . map pruneRow . f

pruneRow :: Row Choices -> Row Choices
pruneRow row = map (remove fixed) row
  where fixed = [d | [d] <- row]
        remove xs ds = if singleton ds then ds else ds \\ xs
        singleton ds = length ds == 1
        
board :: Grid
board = [['5','3','0','0','7','0','0','0','0'],
         ['6','0','0','1','9','5','0','0','0'],
         ['0','9','8','0','0','0','0','6','0'],
         ['8','0','0','0','6','0','0','0','3'],
         ['4','0','0','8','0','3','0','0','1'],
         ['7','0','0','0','2','0','0','0','6'],
         ['0','6','0','0','0','0','2','8','0'],
         ['0','0','0','4','1','9','0','0','5'],
         ['0','0','0','0','8','0','0','7','9']]
  
main :: IO ()
main = do
  putStr . show $ cp [[1,2],[3],[4,5]]
