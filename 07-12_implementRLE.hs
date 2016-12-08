encode :: Eq a => [a] -> [(Int,a)]
encode y = foldr foo [(1,last y)] (init y)
  where foo x ys =
          if x == ((snd . head) ys)
          then (1 + (fst . head) ys,x) : tail ys
          else (1,x) : ys

decode :: Eq a => [(Int,a)] -> [a]
decode xs = foldr f [] xs
  where
    f (1,x) r = x : r
    f (k,x) r = x : f (k-1,x) r

l :: [Int]
l = [1,2,2,3,3,3,4,4,4,4]

main :: IO ()
main = do
  putStrLn $ show l
  putStrLn $ show $ encode l
  putStrLn $ show $ decode $ encode l
