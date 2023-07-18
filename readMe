Date: 16/05/2023

Filename: Proj2.hs
Purpose: Card guess game solver in Haskell

This Haskell program solves the game "guess card".
The game starts with a complete standard deck of cards, without jokers.
There are one person each for both Guesser and Answerer.
The Answerer starts with selecting 2-4 cards from his deck without showing to guesser.
The Guesser will choose the same number of cards from own deck and show them to answerer.
The answerer will respond with feedback which consists of five indicating numbers,
which are "number of answer cards math guess",
"number of answer rank less than minimum rank of guess",
"number of answer ranks match guess",
"number of answer rank greater than maximum rank of guess",
and "number of answer suits match guess"
In this game, rank is order by 2-10, then Jack, Queen, King and Ace.

Here are some denotation:
2-9 denotes the number 2-9, T denotes number 10,
J denotes Jack, Q denotes Queen, K denotes King and A dnotes Ace.
Also, C denotes Card, H denotes Heart, S denotes spade, 
and for sure D denotes Diamond.

An example card selected by answerer is as "3D 4H"
And gussers make a guess as "3D 3H"
Then the feedback from answerer should be "1,0,1,1,2"

We define the card as "Card Suit Rank",
the previous answer can be represented as:
[Card Diamond R3, Card Heart R4], 
in which R3 indicates rank 3, and potentially, Jack indicates rank Jack.

The strategy to solve the game is
from possible guesses pool,
get the possible guess in the middle of list as new guess.
And then consists all new guesses that suits the feedback as a new guesses pool.
