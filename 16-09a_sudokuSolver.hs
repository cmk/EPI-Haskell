--16-9 Create a Sudoku solver

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
valid = undefined

main :: IO ()
main = do
  putStr . show $ cp [[1,2],[3],[4,5]]
