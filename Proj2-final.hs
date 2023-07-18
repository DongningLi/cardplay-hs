
module Proj2 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List

type GameState = [[Card]]

-- get all suit of the cards as a suit list, from card list
getAllSuit :: [Card] -> [Suit]
getAllSuit [] = []
getAllSuit (Card s _ : cardrest) = s:getAllSuit(cardrest)

-- get all rank of the cards as a rank list, from card list
getAllRank :: [Card] -> [Rank]
getAllRank [] = []
getAllRank (Card _ r : cardrest) = r:getAllRank(cardrest)

-- get the number of answer cards math guess
matchGuess :: [Card] -> [Card] -> Int
matchGuess cardlst1 cardlst2 = length(intersect cardlst1 cardlst2)

-- get the number of answer rank less than minimum rank of guess
lessThanRank :: [Card] -> [Card] -> Int
lessThanRank cardlst1 cardlst2 = length (filter (< (minimum ranklst2)) (ranklst1))
    where ranklst1 = getAllRank cardlst1
          ranklst2 = getAllRank cardlst2

--get the number of answer ranks match guess
matchRank :: [Card] -> [Card] -> Int
matchRank cardlst1 cardlst2 = min (length(intersect ranklst1 ranklst2)) (length(intersect ranklst2 ranklst1))
    where ranklst1 = getAllRank cardlst1
          ranklst2 = getAllRank cardlst2

-- get the number of answer rank greater than maximum rank of guess
greaterThanRank :: [Card] -> [Card] -> Int
greaterThanRank cardlst1 cardlst2 = length (filter (> (maximum ranklst2)) (ranklst1))
    where ranklst1 = getAllRank cardlst1
          ranklst2 = getAllRank cardlst2

-- get the number of answer suits match guess
matchSuit :: [Card] -> [Card] -> Int
matchSuit cardlst1 cardlst2 = min (length(intersect suitlst1 suitlst2)) (length(intersect suitlst2 suitlst1))
    where suitlst1 = getAllSuit cardlst1
          suitlst2 = getAllSuit cardlst2

-- get the feedback consists of five number mentioned above
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback cardlst1 cardlst2 = (guessmatch, rankless, rankmatch, rankgreat, suitmatch)
    where guessmatch = matchGuess cardlst1 cardlst2
          rankless = lessThanRank cardlst1 cardlst2
          rankmatch = matchRank cardlst1 cardlst2
          rankgreat = greaterThanRank cardlst1 cardlst2
          suitmatch = matchSuit cardlst1 cardlst2

-- get all combinations of the deck
-- Each selected card is different from each others
combinations :: Int -> [Card] -> [[Card]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations cardnumber (cardhead:cardrestst)
    = [ cardhead:comb | comb <- combinations (cardnumber-1) cardrestst] ++ combinations cardnumber cardrestst
 
-- get initially all possible guesses
allGuess :: Int -> [[Card]]
allGuess cardnumber = combinations cardnumber ([minBound..maxBound] :: [Card])

-- get the initial guess which consist of a list of card and gamestate
-- initial card list is given by the author
-- initial gamestate is all possibile guess defined in "allGuess"
initialGuess :: Int -> ([Card],GameState)
initialGuess cardnumber
    | cardnumber == 2 = ([Card Heart R6, Card Spade R10], allGuess cardnumber)
    | cardnumber == 3 = ([Card Heart R5, Card Spade R8, Card Diamond Jack], allGuess cardnumber)
    | otherwise = ([Card Heart R3, Card Spade R6, Card Diamond R9, Card Club Queen], allGuess cardnumber)
    
-- get the middle guess card in the card list    
middleCard :: GameState -> [Card]
middleCard cardlst = mid
    where m = (length cardlst) `div` 2
          mid = cardlst !! m

-- get the next guess given by the feedback
-- next gamestate is consisted of the card from previous gamestate and suit the feedback
-- next guess card is the head of the gamestate
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (card, gamestatelst) feedbacklst = (cardnext, gsnext)
    where cardnext = middleCard gsnext
          gsnext = [gs | gs <- gamestatelst, feedback gs card == feedbacklst]
