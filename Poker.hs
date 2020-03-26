{- First you type in ghci Poker.hs, then you type in the module name. or function name idk --}
module Poker where 
    import Data.List
    import Data.Function
    import Data.Unique
    --Type Declarations 


    organizedDeck = [(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(10,1),(11,1),(12,1),(13,1),
                    (1,2),(2,2),(3,2),(4,2),(5,2),(6,2),(7,2),(8,2),(9,2),(10,2),(11,2),(12,2),(13,2),
                    (1,3),(2,3),(3,3),(4,3),(5,3),(6,3),(7,3),(8,3),(9,3),(10,3),(11,3),(12,3),(13,3),
                    (1,4),(2,4),(3,4),(4,4),(5,4),(6,4),(7,4),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4)]
    getValue tuple = fst tuple
    getSuit tuple = snd tuple
    dealHand1 deck = [ organizedDeck !! ((snd x)-1) | x <- zip[1..10] deck, odd(fst x)]
    dealHand2 deck = [ organizedDeck !! ((snd x)-1) | x <- zip[1..10] deck, even(fst x)]
    --Two hands, organized  
    entireHand1 deck = sort (dealHand1 deck) 
    entireHand2 deck = sort (dealHand2 deck)

    --Only the card Value 
    cardHand1 deck = do
            let currentHand = (entireHand1 deck)  
            let currentList = map (\element -> fst(element)) currentHand 
            show(currentList)
    cardHand2 deck = do 
            let currentHand = (entireHand2 deck)  
            let currentList = map (\element -> fst(element)) currentHand 
            return currentList
    --Only the suit 
    suitHand1 deck = do
            let currentHand = (entireHand1 deck)  
            let currentList = map (\element -> snd(element)) currentHand 
            show(currentList)
    suitHand2 deck = do
            let currentHand = (entireHand2 deck)  
            let currentList = map (\element -> snd(element)) currentHand 
            show(currentList)
    
    --OnePair: PRIORITY 2 
    isOnePair hand = do
        let uniqueHand = nub hand
        if length uniqueHand == 4 then 2
        else 0

    --TwoPair: PRIORITY 3 
    isTwoPair hand = do
        let uniqueHand = nub hand
        let duplicateHand = (hand \\ uniqueHand)
        --check if the duplicate hand is unique or not 
        let uniqueDuplicateHand = nub duplicateHand 
        if length uniqueDuplicateHand == 2 then 3 
        else 0 

    --Three of a Kind: PRIORITY 4 
    isThreeOfAKind hand = do
        let uniqueHand = nub hand
        let duplicateHand = (hand \\ uniqueHand)
        --check if the duplicate hand is unique or not 
        let uniqueDuplicateHand = nub duplicateHand 
        if length uniqueDuplicateHand == 1 then 4 
        else 0 
    
    --Straight: PRIORITY 5 
    --     checkTuple = {elem(tupleChangedHand, 4), elem(tupleChangedHand, 3)+1, elem(tupleChangedHand, 2)+2, elem(tupleChangedHand, 1)+3, elem(tupleChangedHand, 0)+4}

    --Flush: PRIORITY 6 
    isFlush hand = do 
        let uniqueSuit = nub hand 
        if length uniqueSuit == 1 then 6 
        else 0 

    --FullHouse: PRIORITY 7
    --two pair flag to differentiate between fullhouse and 4 of a kind 
    isFullHouse hand = do 
        let uniqueHand = nub hand
        if (isTwoPair hand == 3) 
            then if (length uniqueHand == 2)
                then 7
            else 0
        else 0
    --FourOfAKind: PRIORITY 8 
    isFourOfAKind hand = do
        let uniqueHand = nub hand
        --if the twoPair flag is triggered then, it's a fullhouse and not a FourOfAKind
        if (isTwoPair hand == 3) 
            then 0
        else if (length uniqueHand == 2)
            then 8
        else 0

    --StraightFlush: PRIORITY 9 
    --Straight flag and flush flag 

    --RoyalFLush: PRIORITY 10 
