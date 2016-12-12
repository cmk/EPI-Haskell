--Find the first occurence of a substring
--Approach is to leverage laziness by generating a list of all occurences of a substring and taking the head.
import Data.List (scanl)
import Safe (headMay)

beginsWith :: Eq a => [a] -> [a] -> Bool
beginsWith [] x = True
beginsWith x [] = False
beginsWith (x:xs) (y:ys) = x == y && beginsWith xs ys

matches :: Eq a => [a] -> [a] -> [Int]
matches ws = map (\x-> x-len) . map fst . filter ((beginsWith sw) . snd) . scanl step (0,[])
  where sw = reverse ws
        len = length ws
        step (n,sx) x = (n+1,x:sx)

firstOccurence x = headMay . matches x
  
main :: IO ()
main = do
  putStrLn $ show $ firstOccurence "hi" "hi there hi"
