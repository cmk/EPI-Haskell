import qualified Data.HashMap.Strict as H

keypad :: H.HashMap Char String
keypad = H.fromList [('2',"abc"),('3',"def"),('4',"ghi"),('5',"jkl"),('6',"mno"),('7',"pqrs"),('8',"tuv"),('9',"wxyz")]

encode :: String -> [String]
encode [] = [[]]
encode (x:xs) = do
  rest <- encode xs
  chars <- keypad H.! x
  return (chars : rest)
  
main :: IO ()
main = do
  putStrLn $ show $ encode "29"
