import System.Random
import Data.List
import Data.Maybe

-- STEP 1
data Suit = Hearts | Clubs | Spades | Diamonds deriving (Eq, Show, Enum)

data Pip = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
             Jack | Queen | King | Ace deriving (Eq, Show, Enum)

type Card = (Pip, Suit)
type Deck = [Card]
type Stock = [Card]


-- STEP 2
pack :: Deck
pack = [(p, s) | p <- [Two .. Ace], s <- [Hearts .. Diamonds]]

sCard :: Card -> Card -- exception when no succ (probably need to use isAce)
sCard (Ace, s) = (Two, s) 
sCard (p, s) = (succ p, s)

pCard :: Card -> Card -- exception when no pred
pCard (Two, s) = (Ace, s)
pCard (p, s) = (pred p, s)

isAce :: Card -> Bool
isAce (p, s) = p == Ace

isKing :: Card -> Bool
isKing (p, s) = p == King


cmp :: Ord b => (a, b) -> (a, b) -> Ordering -- think about types
cmp (x1,y1) (x2,y2) = compare y1 y2

shuffle :: Int -> Deck
shuffle n = [card | (card, n) <- sortBy cmp (zip pack (randoms (mkStdGen n) :: [Int]))] -- how to get random n


-- STEP 3
type Foundation = [Card]
type Column = [[Card]]
type Reserve = [Card]
type Hidden = [Int]

data Board = EOBoard Foundation Column Reserve | SBoard Foundation Column Hidden Stock deriving Eq

instance Show Board where
    show t = showBoard t
        where
          showBoard (EOBoard foundations columns reserves) = "EOBoard \nFoundations  " 
            ++ wrapSquareBracket (showCards foundations 0)
            ++ "\nColumns\n" ++ unpackColumns "\n" columns [0,0,0,0,0,0,0,0] ++ "\nReserve      "
            ++ wrapSquareBracket (showCards reserves 0)

          
          showBoard (SBoard foundations columns hidden stock) = "SBoard \nFoundations  " 
            ++ wrapSquareBracket (showCards foundations 0)
            ++ "\nColumns\n" ++ unpackColumns "\n" columns hidden ++ "\nStock  "
            ++ show ((length (stock)) `div` 10) ++ " Deals remaining"


          -- create a String representation of a list of cards
          showCards :: [Card] -> Int -> String
          showCards [] _ = "" -- might be redundant
          showCards [card] 0 = displayCard card
          showCards [card] n  = "<unknown>"
          showCards (card:restCards) n | (length (card:restCards)) > n  = displayCard card ++ "," ++ (showCards restCards n)
                                       | otherwise                      = "<unknown>" ++ "," ++ (showCards restCards n)

          -- creates a String representation of a card pair
          displayCard card = "(" ++ (show (fst card)) ++ "," ++ (show (snd card)) ++ ")"

          wrapSquareBracket :: String -> String
          wrapSquareBracket cards = "[" ++ cards ++ "]"

          -- cretates a string representation of nested lists of cards
          unpackColumns :: String -> [[Card]] -> [Int] -> String
          unpackColumns separator [] _ = "" -- might be redundant
          unpackColumns separator [card] [x] = "  " ++ (wrapSquareBracket(showCards card x))
          unpackColumns separator (column:restColumns) (x:xs) = "  " ++ (wrapSquareBracket (showCards column x)) 
            ++ separator ++ (unpackColumns separator restColumns xs)


displayEO = EOBoard [] [[]] []
eODeal n = EOBoard [] ((getColumns.groupCards.shuffle) n) ((getReserve.groupCards.shuffle) n)

groupCards [] = []
groupCards list = [fst (splitAt 6 (list))] ++ groupCards (snd (splitAt 6 (list)))

getColumns list = tail (reverse list)

getReserve :: [[Card]] -> [Card]
getReserve list = head (reverse list)

splitColumns _ [] = []
splitColumns n list =  [fst (splitAt n (list))] ++ splitColumns n (snd (splitAt n (list)))

sDeal n = SBoard [] ((splitColumns 6 (fst cardsToDeal)) ++ (splitColumns 5 (snd cardsToDeal))) [5,5,5,5,4,4,4,4,4,4] (snd cards)
    where 
      cards = splitAt 54 (shuffleS n)
      cardsToDeal = splitAt 24 (fst cards)

shuffleS n = [card | (card, n) <- sortBy cmp (zip (pack ++ (shuffle n)) (randoms (mkStdGen n) :: [Int]))]


-- toFoundations
toFoundations (EOBoard f c r) | newBoard /= (EOBoard f c r) = toFoundations newBoard
                              | otherwise                              = EOBoard f c r
                               where 
                                 newBoard = manageSubsC . manageSubsR . manageAcesC  . manageAcesR $ EOBoard f c r

manageAcesR (EOBoard f c r) = aceInList (EOBoard f c r) r
manageAcesC (EOBoard f c r) = aceInColumns (EOBoard f c r) c
manageSubsR (EOBoard f c r) = lookForSubsR (EOBoard f c r) (length f - 1)
manageSubsC (EOBoard f c r) = lookForSubsC (EOBoard f c r) (length f - 1)

aceInList :: Board -> [Card] -> Board
aceInList (EOBoard f c r) [] = EOBoard f c r
aceInList (EOBoard f c r) (x:xs) | fst x == Ace  = EOBoard (f ++ [x]) c (deleteCard r x)
                                 | otherwise     = aceInList (EOBoard f c r) xs

aceInColumns :: Board -> [[Card]] -> Board
aceInColumns (EOBoard f c r) [] = EOBoard f c r
aceInColumns (EOBoard f c r) (x:xs) | (length x) == 0        = aceInColumns (EOBoard f c r) xs
                                    | (fst (head x)) == Ace  = EOBoard (f ++ [(head x)]) (replace x newX c) r
                                    | otherwise           = aceInColumns (EOBoard f c r) xs
                                      where
                                        newX = deleteCard x (head x)

-- takes a board, list and card, then goes through all the cards in list looking for succesor, if found than adding it to f deleteting from r
subInList :: Board -> [Card] -> Card -> Board
subInList (EOBoard f c r) [] card = EOBoard f c r
subInList (EOBoard f c r) (x:xs) card | x == succ  = EOBoard (replaceCard f card x) c (deleteCard r x)
                                      | otherwise  = subInList (EOBoard f c r) xs card
                                        where succ = sCard (card)

-- 
subInColumns :: Board -> [[Card]] -> Card -> Board
subInColumns (EOBoard f c r) [] card = EOBoard f c r
subInColumns (EOBoard f c r) (x:xs) card  | (length x) == 0           = subInColumns (EOBoard f c r) xs card
                                          | (head x) == succ          = EOBoard (replaceCard f card (head x)) (replace x newX c) r
                                          | otherwise                 = subInColumns (EOBoard f c r) xs card
                                           where 
                                             succ = sCard (card)
                                             newX = deleteCard x (head x)


lookForSubsR :: Board -> Int -> Board
lookForSubsR (EOBoard f c r) (-1) = EOBoard f c r
lookForSubsR (EOBoard f c r) index | newBoard /= EOBoard f c r   = newBoard
                                   | otherwise                   = lookForSubsR (EOBoard f c r) (index - 1)
                                    where
                                      newBoard = subInList (EOBoard f c r) r (f!!index)

lookForSubsC :: Board -> Int -> Board
lookForSubsC (EOBoard f c r) (-1) = EOBoard f c r
lookForSubsC (EOBoard f c r) index | newBoard /= EOBoard f c r   = newBoard
                                   | otherwise                   = lookForSubsC (EOBoard f c r) (index - 1)
                                    where
                                      newBoard = subInColumns (EOBoard f c r) c (f!!index)

-- helper
replaceCard :: [Card] -> Card -> Card -> [Card]
replaceCard [] old new = []  
replaceCard (x:xs) old new | x == old     = new : xs
                               | otherwise      = x : replaceCard xs old new

deleteCard :: [Card] -> Card -> [Card]
deleteCard list card = delete card list

-- replace all occurences in a list
replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace old new (x:xs) | old == x     = new:(replace old new xs)
                       | otherwise    = x:(replace old new xs)


-- findMoves :: Board -> [Board]

-- if there is free space in reserve then it adds a card there
freeSpaceInR :: Board -> Card -> Board
freeSpaceInR (EOBoard f c r) card | length (r) < 8  = EOBoard f c (r ++ [card])
                                  | otherwise = EOBoard f c r

-- the same as before except the action (create just one function)
-- successorInC :: Board -> Card -> [[Card]] -> Board
-- successorInC (EOBoard f c r) (x:xs) card |  (length x) == 0           =  subInColumns (EOBoard f c r) xs card
--                                          |  (head x) == succ  = EOBoard f (replaceColumn c x) (replaceColumn c x) r
--                                          | 


-- chooseMove :: Board -> Maybe Board

haveWon :: Board -> Bool
haveWon (EOBoard f [[]] []) = True
haveWon board = False 

-- playSolitair :: Board -> Int
