--  File        : Proj1.hs
--  Author      : Jianyu Zhu 
--  Student ID  : 734057
--  Purpose     : An implementation of a two-player logical guessing card
--                game.

-- | This code implements a two-player card game, including the 
--   initualization of the game, the process of guessing in each turn as
--   well as the feedback generation for each guess. Besides, GameState is
--   defined to store all possible answers in the game. In one game, the
--   guesser will firstly generate all possible answers and the initual
--   guess according to the number of cards in the answer. Answerer will  
--   give feedbacks for all of the guesses and guesser change its guess 
--   based on the feedback for last guess and the possible answers stored 
--   in GameState till answerer tells the guess is  correct.


module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card

-- | GameState is a data type to store all possible answers and will
--   be filtered after each time the guesser receive feedbacks.
type GameState = [[Card]] 

-- | Feedback function.  Calculate the feedback tuple based on the 
--   guess and the correct answer, which including five numbers:
--      1. the number of correct cards;
--      2. the number of cards in the answer that have lower rank
--         than the lowest rank in the guess;
--      3. the number of cards in the answer that have the same rank
--         as the rank in the guess;
--      4. the number of cards in the answer that have higher rank
--         than the highest rank in the guess;
--      5. the number of cards in the answer that have the same suit
--         as the rank in the guess.
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback [] _ = (0,0,0,0,0)
feedback _ [] = (0,0,0,0,0)
feedback x y = (a,b,c,d,e)
    where 
        a = findCorrect x y
        c = compareRank x y
        (b,d) = compareRank' x (minRank y) (maxRank y)
        e = compareSuit x y

-- | FindCorrect function. Find the number of common cards in two
--   given list of cards.
findCorrect :: [Card] -> [Card] -> Int
findCorrect [] _ = 0
findCorrect _ [] = 0
findCorrect (x:xs) y = 
    if x `elem` y then 1 + findCorrect xs y
    else findCorrect xs y

-- | CompareRank function. Find the number of cards with common ranks
--   in two given lists.
compareRank :: [Card] -> [Card] -> Int
compareRank [] _ = 0
compareRank _ [] = 0
compareRank ((Card s r):xs) y = a + compareRank xs b 
    where
        (a,b) = findSameRank (Card s r) y

-- | FindSameRank function. Find if a list of cards have one that has
--   the same rank as the given card.
findSameRank :: Card -> [Card] -> (Int, [Card])
findSameRank _ [] = (0, [])
findSameRank (Card s1 r1) ((Card s2 r2):ys) = 
    if r1 == r2 then (1,ys)
    else tupleAddition (0,[(Card s2 r2)]) (findSameRank (Card s1 r1) ys)

-- | MinRank function. Find out the card that has the lowest rank in a
--   a given list.
minRank :: [Card] -> Card
minRank [] = error "no cards in the input list"
minRank (x:[]) = x
minRank ((Card s1 r1):xs) = 
    if  r1 < r2 then (Card s1 r1)
    else (Card s2 r2)
        where (Card s2 r2) = minRank xs

-- | MaxRank function. Find out the card that has the highest rank in a
--   a given list.
maxRank :: [Card] -> Card
maxRank [] = error "no cards in the input list"
maxRank (x:[]) = x
maxRank ((Card s1 r1):xs) = 
    if  r1 > r2 then (Card s1 r1)
    else (Card s2 r2)
        where (Card s2 r2) = maxRank xs

-- | CompareRank' function. Find out the number of cards in the given list
--   that have lower rank than the first given card and the number of cards
--   in the list that have higher rank than the second given card.
compareRank' ::  [Card] -> Card -> Card -> (Int,Int)
compareRank' [] _ _ = (0,0)
compareRank' ((Card s r):xs) (Card ls lr) (Card hs hr)
    | r < lr 
        = tupleAddition' (1,0) (compareRank' xs (Card ls lr) (Card hs hr))
    | r > hr 
        = tupleAddition' (0,1) (compareRank' xs (Card ls lr) (Card hs hr))
    | otherwise 
        = tupleAddition' (0,0) (compareRank' xs (Card ls lr) (Card hs hr))

-- | TupleAddition and TupleAddition' function. To add two tuples with same
--   data type together by adding elements in the tuples one by one.
tupleAddition::(Int,[Card]) -> (Int,[Card]) -> (Int,[Card])
tupleAddition (a1,b1) (a2,b2) = (a1+a2,b1++b2)

tupleAddition'::(Int,Int) -> (Int,Int) -> (Int,Int)
tupleAddition' (a1,b1) (a2,b2) = (a1+a2,b1+b2)

-- | CompareSuit function. Fing out the number of cards with common suits 
--   in two given lists.
compareSuit :: [Card] -> [Card] -> Int
compareSuit [] _ = 0
compareSuit _ [] = 0
compareSuit a g 
    = sum (zipWith (min) (calculateSuitNum a) (calculateSuitNum g))

-- | CalculateSuitNum function. Calculate the numbers of all kinds of suits
--   in a given list of cards.
--   The return is a list of 4 Ints representing the suit distribution in 
--   the given list of cards.
calculateSuitNum :: [Card] -> [Int]
calculateSuitNum [] = [0,0,0,0]
calculateSuitNum ((Card s _):xs) 
  | s == Club = zipWith (+) [1,0,0,0] (calculateSuitNum xs)
  | s == Diamond = zipWith (+) [0,1,0,0] (calculateSuitNum xs)
  | s == Heart = zipWith (+) [0,0,1,0] (calculateSuitNum xs)
  | otherwise = zipWith (+) [0,0,0,1] (calculateSuitNum xs)

-- | InitialGuess function. To generate the initual guess and initualize the
--   GameState that stores all possible answer according to the number of 
--   cards to guess.
initialGuess :: Int -> ([Card],GameState)
initialGuess n
    -- generate the initual guess and possible answer set when 2 cards to
    -- guess
    | n == 2  
        = ([Card Club R5, Card Diamond R10], initualGameState)
    -- when more than 2 cards to guess, pick the first combination in
    -- GameState as the initial
    | otherwise = (head initualGameState, tail initualGameState)
    where
        initualGameState = combination n cardSet

-- | CardSet variable. It is the list of all cards in order.
cardSet = [minBound..maxBound]::[Card]

-- | Combination function. To generate all possible combinations of cards
--   based on the given number and card set.
combination :: Int-> [Card] -> [[Card]]
combination n (y:ys)
    | n == 0 
        = [[]]
    | n == length (y:ys)
        = [(y:ys)]
    | otherwise
        = map (y:) (combination (n-1) ys)
            ++ combination n ys

-- | NextGuess function. To generate next guess according to last guess, 
--   the collection of possible answers and the feedback for last guess.
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (guess, gameState)
    (correct, lower, sameRank, higher, sameSuit) =
    (newGuess, newGameState)
    where
        -- Ensure the number of correct cards is consistant.
        newGameState1 
            = filter (correctFilter correct guess) gameState
        -- Ensures the number of suits in our guesses is consistant.
        newGameState2 
            = filter (suitFilter sameSuit guess) newGameState1
        -- Ensures the distribution of ranks is consistant.
        newGameState3 
            = filter (rankFilter (lower, sameRank, higher) guess) 
                newGameState2
        -- Generate the new guess based on the remaining possible answer.
        newGuess 
            = newGameState3 !! div (length newGameState3) 2
        -- Generate the new collection of possible answer.
        newGameState 
            = filter (/= newGuess) newGameState3

-- | CorrectFilter function. Returns True if the two lists of cards have 
--   the given number of cards in common.
correctFilter :: Int -> [Card] -> [Card] -> Bool
correctFilter n guess possible = findCorrect possible guess == n

-- | SuitFilter function. Returns True if the two lists of cards have 
--   the given number of suits in common.
suitFilter :: Int -> [Card] -> [Card] -> Bool
suitFilter n guess possible = compareSuit possible guess == n

-- | RankFilter function. Returns True if the two lists of cards have 
--   the given distribution of ranks.
rankFilter :: (Int,Int,Int) -> [Card] -> [Card] -> Bool
rankFilter n guess possible = (a,b,c) == n
    where b = compareRank possible guess
          (a,c) = compareRank' possible (minRank guess) (maxRank guess)