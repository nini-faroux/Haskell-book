module C11_ADTs.Phone where

import Data.Maybe (fromMaybe)
import Data.Char (toLower, isUpper)
import Control.Monad (join)

newtype Phone = 
    Phone {
       buttons :: [Button]
    } 
   deriving (Eq, Show)

newtype Button = 
    Button {
      button :: (Digit, [Letter])
    } 
   deriving (Eq, Show)

data Digit = 
    One 
  | Two
  | Three
  | Four
  | Five
  | Six 
  | Seven 
  | Eight 
  | Nine 
  | Star
  | Zero 
  | Hashtag 
  deriving (Eq, Show)

type Letter = Char
type Presses = Int 
type Cost = Int

makePhone :: Phone 
makePhone = Phone $ Button <$> phoneLayout

mkDigitDict :: [(Digit, [Letter])]
mkDigitDict = button <$> (buttons $ makePhone)

fingerTaps :: [String] -> Presses
fingerTaps msg = sum . fmap sum . (fmap . fmap) (fromMaybe 0) . (fmap . fmap) third $ reverseConvo msg

third :: (a, b, c) -> c
third (_, _, c) = c

reverseConvo :: [String] -> [[(Char, Maybe Digit, Maybe Presses)]]
reverseConvo conv = join $ (fmap . fmap) reverseTap conv

reverseTap :: Char -> [(Char, Maybe Digit, Maybe Presses)]
reverseTap c 
  | isUpper c = ('^', Just Star, Just 1) : [(c, lookup (toLower c) letterDict, tap (toLower c) $ fromMaybe [] (lookup (fromMaybe Zero $ lookup (toLower c) letterDict) mkDigitDict))]
  | otherwise = [(c, lookup c letterDict, tap c $ fromMaybe [] (lookup (fromMaybe Zero $ lookup c letterDict) mkDigitDict))]

tap :: Letter -> [Letter] -> Maybe Presses
tap c xs = go c xs 0 
  where
    go _ [] _ = Nothing
    go c (x:xs) count 
      | c == x    = Just (count+1)
      | otherwise = go c xs (count+1)

cost :: Char -> Maybe Presses 
cost c 
  | (length $ reverseTap c) == 1 = third . head $ reverseTap c
  | otherwise = addMaybes $ third <$> reverseTap c

addMaybes :: [Maybe Int] -> Maybe Int 
addMaybes xs = Just . sum $ (fromMaybe 0) <$> xs

popLetterCost :: [String] -> (Char, Cost)
popLetterCost conv = (mostPopLetter conv, (fst . maximum $ charCount conv) * (fromMaybe 1 . cost $ mostPopLetter conv))

mostPopLetter :: [String] -> Char 
mostPopLetter = snd . maximum . charCount 

charCount :: [String] -> [(Int, Char)]
charCount xxs = go $ join xxs
  where
    go [] = [] 
    go st@(x:xs) 
      | x == ' '  = go $ filter (/=x) xs
      | otherwise = (length $ filter (==x) st, x) : (go (filter (/=x) xs))

phoneLayout :: [(Digit, [Letter])]
phoneLayout = [(One, "1"), (Two, "abc2"), (Three, "def3"), (Four, "ghi4"), (Five, "jkl5"), (Six, "mno6"), (Seven, "pqrs7"), (Eight, "tuv8"), (Nine, "wxyz9"), (Star, "^*"), (Zero, [' ','+','_','0']), (Hashtag, ".,#")] 

letterDict :: [(Letter, Digit)]
letterDict = [('1', One), ('a', Two), ('b', Two), ('c', Two), ('2', Two), ('d', Three), ('e', Three), ('f', Three), ('3', Three), ('g', Four), ('h', Four), ('i', Four), ('4', Four), ('j', Five), ('k', Five), ('l', Five), ('5', Five), ('m', Six), ('n', Six), ('o', Six), ('6', Six), ('p', Seven), ('q', Seven), ('r', Seven), ('s', Seven), ('7', Seven), ('t', Eight), ('u', Eight), ('v', Eight), ('8', Eight), ('w', Nine), ('x', Nine), ('y', Nine), ('z', Nine), ('9', Nine), ('^', Star), ('*', Star), ('+', Zero), ('_', Zero), (' ', Zero), ('0', Zero), ('.', Hashtag), (',', Hashtag), ('#', Hashtag)]

convo :: [String] 
convo = ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Just making sure rofl ur turn"]
