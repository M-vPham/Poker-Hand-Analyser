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
    actualDeck =    ["1C","2C","3C","4C","5C","6C","7C","8C","9C","10C","11C","12C","13C",
                    "1D","2D","3D","4D","5D","6D","7D","8D","9D","10D","11D","12D","13D",
                    "1H","2H","3H","4H","5H","6H","7H","8H","9H","10H","11H","12H","13H",
                    "1S","2S","3S","4S","5S","6S","7S","8S","9S","10S","11S","12S","13S"]
    getValue tuple = fst tuple
    getSuit tuple = snd tuple
    dealHand1 deck = [ organizedDeck !! ((snd x)-1) | x <- zip[1..10] deck, odd(fst x)]
    dealHand2 deck = [ organizedDeck !! ((snd x)-1) | x <- zip[1..10] deck, even(fst x)]
    --Two hands, organized  (tuple)
    entireHand1 deck = sort (dealHand1 deck) 
    entireHand2 deck = sort (dealHand2 deck)

    --Only the card Value 
    cardHand1 deck = map (\element -> fst(element)) (entireHand1 deck)
    cardHand2 deck = map (\element -> fst(element)) (entireHand2 deck)  



    {--CHECKING THE HANDS 
        input: sorted hand, or tuple 
        output: priority ranging from (0-10) 
    --}

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
    isStraight hand = do
        --this case accounts for 1,10,11,12,13 and 1,2,3,4,5
        let checkConsecutive = [hand !! 4, (hand !! 3) + 1, (hand !! 2) + 2, (hand !! 1) + 3, (hand !! 0) + 4] 
        let lengthOfList = nub checkConsecutive
        if (head hand == 1)
            then 
                case tail hand of
                    [10,11,12,13] -> 5
                    [2,3,4,5] -> 5
                    _-> 0
        else if (length lengthOfList == 1)
            then 5 
        else 0 

    --Flush: PRIORITY 6
    --input should be the tuple hand instead of the normal hands so the "entireHand1 deck" 
    isFlush tupleHand = do 
        let sortedTuple = sort (tupleHand)  
        let suitHand = map (\element -> snd(element)) sortedTuple 
        let uniqueSuit = nub suitHand 
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
    --input must also be the tuple list 
    isStraightFlush tupleHand = do 
        let sortedTuple = sort (tupleHand)  
        let handValue = map (\element -> fst(element)) sortedTuple
        if (isFlush sortedTuple == 6)
            then if (isStraight handValue == 5)
                then 9
            else 0
        else 0

    --RoyalFLush: PRIORITY 10 
    --input must also be the tuple list ugh 
    isRoyalFlush tupleHand = do 
        let sortedTuple = sort (tupleHand)  
        let handValue = map (\element -> fst(element)) sortedTuple
        if (isFlush sortedTuple == 6)
            then 
                case handValue of 
                    [1,10,11,12,13] -> 10 
                    _-> 0 
        else 0

    
    checkPriority tupleValue handValue
        | (isRoyalFlush tupleValue == 10) = 10
        | (isStraightFlush tupleValue == 9) = 9
        | (isFourOfAKind handValue == 8) = 8
        | (isFullHouse handValue == 7) = 7
        | (isFlush tupleValue == 6) = 6
        | (isStraight handValue == 5) = 5
        | (isThreeOfAKind handValue == 4) = 4
        | (isTwoPair handValue == 3) = 3 
        | (isOnePair handValue == 2) = 2
        | otherwise = 1

    {--
        HighestCard TieBreaker 
        Input: list of tuples only eg [(1,1)....(5,1)]
        Output: integer of the hand that wins eg 1 or 2 
        Case1: unique max numbers, 
        Case2: common max number, must go to the next one. (make sure that it applies to every iteration) 
        Case3: same hands completely, go to suit breaker
            Case:1 Check highest number for the suit, and it should be done. Because two of the same cannot be the same suit 
    --}
    --do deck
    highestCardTieBreaker handValue1 handValue2 = do 
        -- let tupleValue1 = sort (dealHand1 deck) 
        -- let tupleValue2 = sort (dealHand2 deck)
        -- let handValue1 = map (\element -> fst(element)) (tupleValue1)
        -- let handValue2 = map (\element -> fst(element)) (tupleValue2)
        let hand1Max = maximum handValue1 
        let hand2Max = maximum handValue2
        -- cannot remove all the common values because maximum will yield and error 
        let intersectList = handValue1 `intersect` handValue2
        let uniqueHandValue1 = handValue1 \\ intersectList
        let uniqueHandValue2 = handValue2 \\ intersectList
        let nextMax1 = maximum uniqueHandValue1
        let nextMax2 = maximum uniqueHandValue2 
        -- show(hand1Max,hand2Max)
        --Case1 
        if (hand1Max > hand2Max)
            then
                1 
        else if (hand2Max > hand1Max)
            then 2 
        --if they're the same 
        else 
            --exact same hand 
            if (length uniqueHandValue1 == 0)
                then -10 --DO SUITBREAKER
            else if (nextMax1 > nextMax2)
                then 1
            else 2 
    {--
        SuitBreaker OnePair, Two Pair  
        input: tuple 
        output: tuple that is higher
    --}
    --
    suitBreaker tuple1 tuple2 = do
        let handValue1 = maximum(map (\element -> fst(element)) (sort tuple1))
        let handValue2 = maximum(map (\element -> fst(element)) (sort tuple2))
        if (handValue1 > handValue2)
            then 1 
        else 2
        

         

    {--
        onePair/threeOfAKind
        input: tupleHand1, tupleHand2 
        output: winning hand integer 
    --}
    onePairTieBreaker tuple1 tuple2 = do 
        let handValue1 = map (\element -> fst(element)) (sort tuple1)
        let handValue2 = map (\element -> fst(element)) (sort tuple2)
        let comparePair1 = handValue1 \\ (nub handValue1) 
        let comparePair2 = handValue2 \\ (nub handValue2)
        let maxNum1 = maximum(comparePair1)
        let maxNum2 = maximum(comparePair2)
        --does not include the pair 
        let noPair1 = (handValue1 \\comparePair1) \\ comparePair1
        let noPair2 = (handValue2 \\comparePair2) \\comparePair2

        if (maxNum1 > maxNum2)
            then 1
        else if (maxNum2 > maxNum1)
            then 2 
        else 
            if (highestCardTieBreaker noPair1 noPair2 == -10)
                then -1000000000 --dosuitBreaker on the pair 
            else 
                highestCardTieBreaker noPair1 noPair2 
    
    {--
        twoPair
        input: tupleHand1, tupleHand2 
        output: winning hand integer 
    --}
    twoPairTieBreaker tuple1 tuple2 = do 
        let handValue1 = map (\element -> fst(element)) (sort tuple1)
        let handValue2 = map (\element -> fst(element)) (sort tuple2)
        let comparePair1 = handValue1 \\ (nub handValue1) 
        let comparePair2 = handValue2 \\ (nub handValue2)
        --why do I intersect, i intersect to get rid of any common numbers. but that causes problems when I use maxNum
        let intersectList = (comparePair1 `intersect` comparePair2)
        let maxNum1 = (comparePair1 !! 0)
        let maxNum2 = (comparePair2 !! 0)
        let maxNum1b = (comparePair1 !! 1)
        let maxNum2b = (comparePair2 !! 1)
        --noPair 
        let noPair1 = (handValue1 \\comparePair1) \\ comparePair1
        let noPair2 = (handValue2 \\comparePair2) \\comparePair2
        --onlyPair 
        let maxIndex1 = elemIndices maxNum1 handValue1
        let tupleList1 = [ sort tuple1 !! (maxIndex1 !! 0)] ++ [sort tuple1 !! (maxIndex1 !! 1)]
        let maxIndex2 = elemIndices maxNum2 handValue2
        let tupleList2 = [ sort tuple2 !! (maxIndex2 !! 0)] ++ [sort tuple2 !! (maxIndex2 !! 1)]
        --if both pairs are the same then do the unique cards 
        if (comparePair1 == comparePair2)
            --if the highestCards are the same, suitBreak with the tupleList of the pairs  
            then if (highestCardTieBreaker noPair1 noPair2 == -10)
                then suitBreaker tupleList1 tupleList2 
            --otherwise perform highcard 
            else 
                highestCardTieBreaker noPair1 noPair2
        --check if the maximum number is bigger than the other
        else if (maxNum1b > maxNum2b) then 1 
        else if (maxNum2b > maxNum1b) then 2 
        else if (maxNum1b == maxNum2b)
            then if (maxNum1 > maxNum2)
                then 1 
            else 2
        else -10




    {--
        threeOfAKind check
        input: tuple1, tupl2 
        output number 
    --}
    threeOfAKindTieBreaker tuple1 tuple2 = do 
        let handValue1 = map (\element -> fst(element)) (sort tuple1)
        let handValue2 = map (\element -> fst(element)) (sort tuple2)
        let comparePair1 = handValue1 \\ (nub handValue1) 
        let comparePair2 = handValue2 \\ (nub handValue2)
        let maxNum1 = maximum(comparePair1)
        let maxNum2 = maximum(comparePair2)
        if (maxNum1 > maxNum2)
            then 1
        else 2

    {--
        straightTieBreaker/Flush
        input: tupleHand1, tupleHand2 
        output: winning hand integer
        do not have to worry about if both cases has an Ace* bc that would be royalFlush 
    --}
    straightTieBreaker tuple1 tuple2 = do 
        let handValue1 = map (\element -> fst(element)) (sort tuple1)
        let handValue2 = map (\element -> fst(element)) (sort tuple2)
        let maxNum1 = maximum(handValue1)
        let maxNum2 = maximum(handValue2)
        --getting the tuple of the max Card 
        let maxIndex1 = elemIndices maxNum1 handValue1
        let tupleList1 = [ sort tuple1 !! (maxIndex1 !! 0)]
        let maxIndex2 = elemIndices maxNum2 handValue2
        let tupleList2 = [ sort tuple2 !! (maxIndex2 !! 0)]
        if (maxNum1 == maxNum2)
            then suitBreaker tupleList1 tupleList2 
        else if (maxNum1 > maxNum2)
            then 1 
        else 2


    {--
        fullHouseTieBreaker 
        input: tupleHand1, tupleHand2 
        output: winning hand integer 
    --}
    fullHouseTieBreaker tuple1 tuple2 = do 
        let handValue1 = map (\element -> fst(element)) (sort tuple1)
        let handValue2 = map (\element -> fst(element)) (sort tuple2)
        let threeRepeat1 = maximum(filt handValue1 3) 
        let threeRepeat2 = maximum(filt handValue2 3)
        let repeat1 = maximum(filt handValue1 2)
        let repeat2 = maximum(filt handValue2 2)
        if (threeRepeat1 > threeRepeat2)
            then 1 
        else 2

    {--
        fourOfAKind 
        input: tupleHand1, tupleHand2 
        output: winning hand integer 
    --}
    fourOfAKindTieBreaker tuple1 tuple2 = do 
        let handValue1 = map (\element -> fst(element)) (sort tuple1)
        let handValue2 = map (\element -> fst(element)) (sort tuple2)
        let threeRepeat1 = maximum(filt handValue1 4) 
        let threeRepeat2 = maximum(filt handValue2 4)
        if (threeRepeat1 > threeRepeat2)
            then 1 
        else 2

    {--
        royalTieBreaker
        input: tuple1 tuple2 
        output: integer of winning hand 
    --}
    royalTieBreaker tuple1 tuple2 = do
        let handValue1 = map (\element -> fst(element)) (sort tuple1)
        let handValue2 = map (\element -> fst(element)) (sort tuple2)
        let maxTupleSuit1 = snd(tuple1 !! 0)
        let maxTupleSuit2 = snd(tuple2 !! 0) 
        if (maxTupleSuit1 > maxTupleSuit2)
            then 1
        else 2 
    
        

    {--helper functions --}
    uniq :: Eq a => [a] -> [a] -> [a]
    uniq x [] = x 
    uniq [] (a:xs) = uniq [a] xs
    uniq x (a:xs) = if a `elem` x then uniq x xs else uniq (a:x) xs 

    count :: Eq a => Integral b => a -> [a] -> b
    count e [] = 0
    count e (a:xs) = (count e xs +) $ if a == e then 1 else 0

    filt :: Show a => Num a => Read a => Eq a => Integral b => [a] -> b -> [a]
    filt a b = reverse $ uniq [] [i | i <- a, count i a >= b]


    checkTieBreaker tuple1 tuple2 priority
        |(priority == 10) = royalTieBreaker tuple1 tuple2
        |(priority == 9) = straightTieBreaker tuple1 tuple2 
        |(priority == 8) = fourOfAKindTieBreaker tuple1 tuple2 
        |(priority == 7) = fullHouseTieBreaker tuple1 tuple2
        |(priority == 6) = highestCardTieBreaker tuple1 tuple2 
        |(priority == 5) = straightTieBreaker tuple1 tuple2 
        |(priority == 4) = threeOfAKindTieBreaker tuple1 tuple2 
        |(priority == 3) = twoPairTieBreaker tuple1 tuple2 
        |(priority == 2) = onePairTieBreaker tuple1 tuple2
        |otherwise = highestCardTieBreaker tuple1 tuple2 

    --pass in the array of indices 
    convertToActual winningTuple = map (actualDeck !!) (winningTuple)      

    --Deal function
    deal deck = do 
        -- TupleValue 
        let tupleValue1 = sort (entireHand1 deck)
        let tupleValue2 = sort (entireHand2 deck) 
        -- CardValue
        let cardValue1 = cardHand1 deck
        let cardValue2 = cardHand2 deck 
        -- assign Priorities 
        let priority1 = checkPriority tupleValue1 cardValue1 
        let priority2 = checkPriority tupleValue2 cardValue2

        let hand1Indices = (elemIndices (tupleValue1 !! 0) organizedDeck) ++(elemIndices (tupleValue1 !! 1) organizedDeck) ++(elemIndices (tupleValue1 !! 2) organizedDeck) ++(elemIndices (tupleValue1 !! 3) organizedDeck) ++(elemIndices (tupleValue1 !! 4) organizedDeck)
        let hand2Indices = (elemIndices (tupleValue2 !! 0) organizedDeck) ++(elemIndices (tupleValue2 !! 1) organizedDeck) ++(elemIndices (tupleValue2 !! 2) organizedDeck) ++(elemIndices (tupleValue2 !! 3) organizedDeck) ++(elemIndices (tupleValue2 !! 4) organizedDeck)
        if (priority1 == priority2)
            --hand1 trumps hand  
            then if (checkTieBreaker tupleValue1 tupleValue2 priority1 == 1)
                then convertToActual hand1Indices
            else convertToActual hand2Indices
        else if (priority1 > priority2)
            then convertToActual hand1Indices
        else convertToActual hand2Indices
    



   
