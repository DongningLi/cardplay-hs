{-
possibleGuess :: (Card, GameState) -> (Int,Int,Int,Int,Int) -> [Card]
possibleGuess (card, card) _ = [card]
possibleGuess (card, (gshead:gsrest)) feedbacklist
    |feedback card [gshead] == feedbacklist = (gshead : (possibleGuess (card, gsrest) feedbacklist))
    |otherwise = possibleGuess (card, gsrest) feedbacklist

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess ([card], gs) feedbacklist = (cardnext, gsnext)
    where (cardnext: gsnext) = intersect gs (possibleGuess (card, gs) feedbacklist)
-}


-- Replace this comment with your opening documentation.  
-- Leave this module declaration as is:
module Proj2 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List

data GameState = GameState [Card] deriving (Eq, Show)

matchGuess :: [Card] -> [Card] -> Int
matchGuess cardlst1 cardlst2 = length(intersect cardlst1 cardlst2)

lessThanRank :: [Card] -> [Card] -> Int
lessThanRank cardlst1 cardlst2 = length (filter (< (minimum (rank cardlst2))) (rank cardlst1))

matchRank :: [Card] -> [Card] -> Int
matchRank cardlst1 cardlst2 = length(intersect (rank cardlst1) (rank cardlst2))

greaterThanRank :: [Card] -> [Card] -> Int
greaterThanRank cardlst1 cardlst2 = length (filter (> (maximum (rank cardlst2))) (rank cardlst1))

matchSuit :: [Card] -> Int
matchSuit cardlst1 cardlst2 = length(intersect (suit cardlst1) (suit cardlst2))

feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback cardlst1 cardlst2 = (guessmatch, rankless, rankmatch, rankgreat, suitmatch)
    where guessmatch = matchGuess cardlst1 cardlst2
          rankless = lessThanRank cardlst1 cardlst2
          rankmatch = matchRank cardlst1 cardlst2
          rankgreat = greaterThanRank cardlst1 cardlst2
          suitmatch = matchSuit cardlst1 cardlst2

initialGuess :: Int -> ([Card],GameState)
initialGuess cardnumber
    | cardnumber == 2 = ([Card Heart R2, Card Spade R8], [minBound..maxBound])
    | cardnumber == 3 = ([Card Heart R2, Card Spade R6, Card Diamond R10], [minBound..maxBound])
    | otherwise = ([Card Heart R2, Card Spade R5, Card Diamond R8, Card Club Jack], [minBound..maxBound])

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (card, gamestatelst) feedbacklst = (cardnext, gsnext)
    where cardnext = [Card Heart R2]
          gsnext = [gs | gs <- gamestatelst, feedback [gs] card == feedbacklst]
          
          
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (card, gamestatelst) feedbacklst = (cardnext, gsnext)
    where cardnext = take (length card) gsnext
          gsnext = [gs | gs <- (take (length card) gamestatelst), feedback [gs] card == feedbacklst]
          -- gsnext = (filter (\gs -> (feedback [gs] card) == feedbacklst) gamestatelst)

