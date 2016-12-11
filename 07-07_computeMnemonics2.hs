--07-07 compute all mnemonics for a phone numer
--applicative version w/ error handling
import qualified Data.HashMap.Strict as H



keypad :: H.HashMap Char String
keypad = H.fromList [('2',"abc"),('3',"def"),('4',"ghi"),('5',"jkl"),('6',"mno"),('7',"pqrs"),('8',"tuv"),('9',"wxyz")]

encode :: String -> Maybe [String]
encode [] = Just [""]
encode (x:xs) =  (<*>) <$> fmap (:) <$> (H.lookup x keypad) <*> (encode xs)
  
main :: IO ()
main = do
  putStrLn $ show $ encode "89"
