module BuySellStockII where 

maxProfit :: [Int] -> Int
maxProfit xs = snd $ foldl (\(prev, acc) x -> (x, acc + max 0 (x - prev))) (head xs, 0) xs

maxProfitMain :: IO () 
maxProfitMain = do 
  print $ maxProfit [7,2,5,1,3,6,4]
  print $ maxProfit [7,1,5,3,6,4]
  print $ maxProfit [1,2,3,4,5]
  print $ maxProfit [5,4,3,2,1]
  print $ maxProfit [] 
