--16-02 Solve the n-queens problem for an arbitrary board size

import Data.Maybe
import System.Environment
import Control.Applicative
import Control.Monad
import Control.Monad.Logic

type Queen = (Int,Int)
type Board = [Queen]

queens :: MonadPlus m => Int -> m Board
queens n = placeQueens n n

placeQueens :: MonadPlus m => Int -> Int -> m Board
placeQueens _ 0 = return mzero
placeQueens size k = do
  queens' <- placeQueens size (k-1)
  col <- msum [return x | x <-[1..size]]
  let queen = (k,col)
  guard $ queen `safeOn` queens'
  return (queen:queens')

safeOn :: Queen -> Board -> Bool
safeOn q = not . any (inCheck q)

inCheck :: Queen -> Queen -> Bool
inCheck p1@(x1,y1) p2@(x2,y2) = x1 == x2 || y1 == y2 || p1 `isDiagonalTo` p2

isDiagonalTo :: Queen -> Queen -> Bool
isDiagonalTo (x1,y1) (x2,y2) = abs (x1 - x2) == abs (y1 - y2)

main :: IO ()
main = do
  size <- maybe 8 read . listToMaybe <$> getArgs
  printLines (queens size :: [Board])
  printLines . observeAll $ (queens size :: Logic Board)
  where
    printLines :: Show a => [a] -> IO ()
    printLines = putStrLn . unlines . fmap show
