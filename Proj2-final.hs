-- Replace this comment with your opening documentation.  
-- Leave this module declaration as is:
module Proj2 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List

type GameState = [[Card]]

getAllSuit :: [Card] -> [Suit]
getAllSuit [] = []
getAllSuit (Card s _ : cardrest) = s:getAllSuit(cardrest)

getAllRank :: [Card] -> [Rank]
getAllRank [] = []
getAllRank (Card _ r : cardrest) = r:getAllRank(cardrest)

matchGuess :: [Card] -> [Card] -> Int
matchGuess cardlst1 cardlst2 = length(intersect cardlst1 cardlst2)

lessThanRank :: [Card] -> [Card] -> Int
lessThanRank cardlst1 cardlst2 = length (filter (< (minimum ranklst2)) (ranklst1))
    where ranklst1 = getAllRank cardlst1
          ranklst2 = getAllRank cardlst2
          
matchRank :: [Card] -> [Card] -> Int
matchRank cardlst1 cardlst2 = min (length(intersect ranklst1 ranklst2)) (length(intersect ranklst2 ranklst1))
    where ranklst1 = getAllRank cardlst1
          ranklst2 = getAllRank cardlst2

greaterThanRank :: [Card] -> [Card] -> Int
greaterThanRank cardlst1 cardlst2 = length (filter (> (maximum ranklst2)) (ranklst1))
    where ranklst1 = getAllRank cardlst1
          ranklst2 = getAllRank cardlst2

matchSuit :: [Card] -> [Card] -> Int
matchSuit cardlst1 cardlst2 = min (length(intersect suitlst1 suitlst2)) (length(intersect suitlst2 suitlst1))
    where suitlst1 = getAllSuit cardlst1
          suitlst2 = getAllSuit cardlst2

feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback cardlst1 cardlst2 = (guessmatch, rankless, rankmatch, rankgreat, suitmatch)
    where guessmatch = matchGuess cardlst1 cardlst2
          rankless = lessThanRank cardlst1 cardlst2
          rankmatch = matchRank cardlst1 cardlst2
          rankgreat = greaterThanRank cardlst1 cardlst2
          suitmatch = matchSuit cardlst1 cardlst2

allGuess :: Int -> [[Card]]
allGuess cardnumber
    | cardnumber == 2 = [ [a,b] | a<-[minBound..maxBound] :: [Card], b<- [minBound..maxBound] :: [Card], a/=b]
    | cardnumber == 3 = [ [a,b,c] | a<-[minBound..maxBound] :: [Card], b<-[minBound..maxBound] :: [Card], c<-[minBound..maxBound] :: [Card], a /= b, a/=c, b/=c]
    | otherwise = [ [a,b,c,d] | a<-[minBound..maxBound] :: [Card], b<-[minBound..maxBound] :: [Card], c<-[minBound..maxBound] :: [Card], d<-[minBound..maxBound] :: [Card]
                    , a/=b, a/=c, a/=d, b/=c, b/=d, c/=d]

initialGuess :: Int -> ([Card],GameState)
initialGuess cardnumber
    | cardnumber == 2 = ([Card Heart R5, Card Spade R9], allGuess cardnumber)
    | cardnumber == 3 = ([Card Heart R5, Card Spade R8, Card Diamond Jack], allGuess cardnumber)
    | otherwise = ([Card Heart R3, Card Spade R6, Card Diamond R9, Card Club Queen], allGuess cardnumber)

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (card, gamestatelst) feedbacklst = (cardnext, gsnext)
    where cardnext = head gsnext
          gsnext = [gs | gs <- gamestatelst, feedback gs card == feedbacklst]
