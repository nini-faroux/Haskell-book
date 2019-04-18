module Tutorial.Examples.Intro where 

import Conduit 

mainOne :: IO () 
mainOne = do
  print $ runConduitPure $ yieldMany [1..10] .| sumC 
  writeFile "input.txt" "Just a test" 
  runConduitRes $ sourceFileBS "input.txt" .| sinkFile "output.txt" 
  readFile "output.txt" >>= putStrLn 
  print $ runConduitPure $ yieldMany [1..10] .| mapC (+1) .| sinkList 

mainTwo :: IO () 
mainTwo = do 
  putStrLn "list version: "
  print $ take 10 [1..]
  putStrLn ""
  putStrLn "conduit version: "
  print $ runConduitPure $ yieldMany [1..] .| takeC 10 .| sinkList

mainThree :: IO () 
mainThree = do
  putStrLn "list version: "
  print $ takeWhile (< 18) $ map (*2) $ take 10 [1..]
  putStrLn "conduit version: "
  print $ runConduitPure 
        $ yieldMany [1..] 
       .| takeC 10
       .| mapC (* 2) 
       .| takeWhileC (< 18) 
       .| sinkList

mainFour :: IO () 
mainFour = do
  putStrLn "list version: "
  mapM_ print $ takeWhile (< 18) $ map (* 2) $ take 10 [1..]
  putStrLn "conduit version: "
  runConduit 
    $ yieldMany [1..]
   .| takeC 10 
   .| mapC (* 2) 
   .| takeWhileC (< 18) 
   .| mapM_C print
  
--- 
magic :: Int -> IO Int 
magic x = do
  putStrLn $ "magic with " ++ show x 
  return $ x * 2

mainFive :: IO () 
mainFive = do
  putStrLn "my list solution: "
  mapM_ print =<< takeWhile (< 18) <$> traverse magic (take 10 [1..]) 
  putStrLn "given list version: "
  mapM magic (take 10 [1..]) >>= mapM_ print . takeWhile (< 18)
  putStrLn "conduit version: "
  runConduit 
    $ yieldMany [1..]
   .| takeC 10 
   .| mapMC magic 
   .| takeWhileC (< 18) 
   .| mapM_C print
