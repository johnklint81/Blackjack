{-
Group 5
Lukas Carling, Benjamin Eld, John Klint
-}
module Blackjack where

import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)


-- Creating a card
aCard1 :: Card
aCard1 = Card (Numeric 2) Clubs

-- Creating another card
aCard2 :: Card
aCard2 = Card Jack Hearts

-- Third card for testing
aCard3 :: Card
aCard3 = Card Ace Spades

-- Creating a hand
aHand :: Hand
aHand = [aCard1, aCard2]

-- Testing hand
aHand3 :: Hand
aHand3 = [aCard1, aCard2, aCard3]

-- Hand for Task A1
hand1 :: Hand
hand1 = [Card (Numeric 2) Hearts, Card Ace Spades, Card Ace Clubs]

hand2 :: Hand
hand2 = Card (Numeric 2) Hearts : (Card Jack Spades : [])

hand3 :: Hand
hand3 = Card (Numeric 2) Hearts : (Card Ace Spades : [])

hand4 :: Hand
hand4 = [Card (Numeric 2) Hearts, Card Ace Spades, Card Ace Clubs]

hand5 :: Hand
hand5 = Card (Numeric 2) Diamonds : (Card Ace Hearts : [])

-- Task A1
sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size ([Card (Numeric 2) Hearts, Card Jack Spades])
            , 1 + size ([Card Jack Spades])
            , 1 + 1 + size []
            , 1 + 1 + 0
            , 2]

-- Task A2
-- Show a card. Use putStrLn to display Unicode symbols
-- displayCard :: Card -> String
-- displayCard (Card r s)
--   | s == Hearts   = show r ++ " of " ++ "\9829"
--   | s == Spades   = show r ++ " of " ++ "\9824"
--   | s == Diamonds = show r ++ " of " ++ "\9830"
--   | s == Clubs    = show r ++ " of " ++ "\9827"
displayCard :: Card -> String
displayCard (Card r s)
  | s == Hearts   = show r ++ " of " ++ show s
  | s == Spades   = show r ++ " of " ++ show s
  | s == Diamonds = show r ++ " of " ++ show s
  | s == Clubs    = show r ++ " of " ++ show s

-- Show hand
display :: Hand -> String
display (x:xs)
  | null xs = displayCard x
  | otherwise = displayCard x ++ "\n" ++ display xs

-- Task A3
-- Calculate rank-value of card
valueRank :: Rank -> Int
valueRank (Numeric n) = n
valueRank rank
  | rank == Ace = 11
  | otherwise = 10

-- Calculate value of card
valueCard :: Card -> Int
valueCard card = valueRank (rank card)

-- Calculate number of aces
numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (x:xs)
  | rank x == Ace = 1 + numberOfAces xs
  | otherwise = numberOfAces xs

-- Compute value of a hand with aces worth 11
valueAces11 :: Hand -> Int
{-
valueAces11 [] = 0
valueAces11 (x:xs) = valueCard x + valueAces11 xs
remade into:
valueAces11 = foldr ((+) . valueCard) 0
-}
valueAces11 = foldr ((+) . valueCard) 0

-- Compute value of hand with aces worth 1
valueAces1 :: Hand -> Int
valueAces1 hand = valueAces11 hand - 10 * numberOfAces hand

-- Get the value of the hand, considering aces are 1 or 11
value :: Hand -> Int
value hand
  | valueAces11 hand <= 21 = valueAces11 hand
  | otherwise = valueAces1 hand

-- Task A4
-- Check if hand is over 21
gameOver :: Hand -> Bool
gameOver hand = value hand > 21
-- Changed on suggestion of TA
-- gameOver hand
--   | valueAces11 hand > 21 && valueAces1 hand > 21 = True
--   | otherwise = False

{- 
Check winner. 
Guest wins if their score is higher than Bank and none are bust.
Guest wins if Bank is bust while not bust themselves.
Bank wins all other situations.
-}
winner :: Hand -> Hand -> Player
winner g b
    | (value g == value b) = Bank
    | (value g > value b) && (not (gameOver g)) = Guest
    | otherwise = Bank

-- Task B1
{-
Returns a list of all ranks
-}
allRanks :: [Rank]
allRanks = [Numeric x | x <- [2..10]] ++ [Jack, Queen, King, Ace] 

{-
Returns a list of all ranks
-}
allSuits :: [Suit]
allSuits = [Clubs, Diamonds, Hearts, Spades]

{-
Creates all cards
-}
allCards :: [Card]
allCards = [Card r s | r <- allRanks, s <- allSuits]

{-
Returns a full deck.
-}
fullDeck :: Deck
fullDeck = allCards 

{-
Checks if deck is full

Testhand:
aHand = [aCard1, aCard2]
aCard1 = Card (Numeric 2) Clubs
aCard2 = Card Jack Hearts
-}
prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

-- Task B2
{-
Draw a card from a deck and put into a hand
-}
draw :: Deck -> Hand -> (Deck, Hand)
draw [] _ = error "draw: The deck is empty"
draw d h = (tail d, head d : h)

{-
Help function to convert pair to list
-}
pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

{-
Help function to retrieve first element of pair
-}
first :: (a, b) -> a
first (x, y) = x

{-
Help function to retrieve second element of pair
-}
second :: (a, b) -> b
second (x, y) = y

-- Task B3
{-
Play for the bank
-}
playBank :: Deck -> Hand
playBank deck = playBank' deck []

{-
Help function for playBank
-}
playBank' :: Deck -> Hand -> Hand
playBank' deck bankHand
  | value (bankHand) < 16 = playBank' deck' bankHand'
  | otherwise = bankHand
  where 
    (deck', bankHand') = draw deck bankHand

{-
Shuffle a deck
-}
shuffle :: [Double] -> Deck -> Deck
shuffle rand deck = shuffle' rand deck []

{-
Help function for shuffle
-}
shuffle' :: [Double] -> Deck -> Deck -> Deck
shuffle' rand dinp dout
  | (length dinp) > 0 = shuffle' (tail rand) (dropElement index dinp) ((dinp !! index) : dout) 
  | otherwise = dout
    where
      index = floor(fromIntegral(length dinp) * rand !! 0)

{-
Drops an element at a specific index from a list
-}
dropElement :: Int -> Deck -> Deck
dropElement _ [] = []
dropElement 0 (x:xs) = xs
dropElement n (x:xs) = x : dropElement (n - 1) (xs)

{-
Testing the dropElement function manually...

dropElement 0 [1, 2, 3] =
dropElement 0 1 : [2, 3]  =
                        = [2, 3]

dropElement 1 [1, 2, 3]
dropElement 1 (1 : [2, 3]) = 1 : dropElement (1 - 1) [2, 3]
                           = 1 : dropElement 0 2 :[3]
                           = 1 : 3

dropElement 2 [1, 2, 3]    = 
dropElement 2 (1 : [2, 3]) = (1 : dropElement (2 - 1) [2, 3]) 
                           = (1 : dropElement 1 [2, 3] 
                           = (1 : (2 : dropElement (1 - 1) [3])) 
                           = (1 : (2 : dropElement 0 [3])) 
                           = (1 : (2 : (dropElement 0 3 : [])))
                           = (1 : (2 : ([])))
-}


{-
Shuffling does not change the cards in a hand
-}
belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs


{-
Test ... something 
-}
prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

{-
The size of the deck does not change by shuffling
-}
prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = size deck == size (shuffle randomlist deck)

{-
Test shuffle
-}
testShuffle :: IO Deck
testShuffle = do
  Rand ds <- generate arbitrary
  return (shuffle ds fullDeck)

{-
Package up the functions
-}
implementation = Interface
  {  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iDisplay   = display
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

{-
to play, run main
-}
main :: IO ()
main = runGame implementation

